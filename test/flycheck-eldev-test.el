;; -*- lexical-binding: t; -*-
(require 'flycheck-eldev)
(require 'ert)
(require 'dash)


(defvar flycheck-eldev--test-dir (file-name-directory (or load-file-name (buffer-file-name))))


;; Copied from Eldev source code, see documentation there.
(eval-when-compile
  (defmacro flycheck-eldev-ert-defargtest (name arguments values &rest body)
    (declare (indent 3))
    (let ((function (make-symbol (format "%s:impl" name))))
      `(progn
         (cl-macrolet ((skip-unless (form) `(ert--skip-unless ,form)))
           (defun ,function ,arguments ,@body))
         ,@(mapcar (lambda (arg-values)
                     `(ert-deftest ,(intern (format "%s/%s" name (flycheck-eldev--ert-defargtest-format-arguments arg-values))) ()
                        (,function ,@(if (= (length arguments) 1) (list arg-values) arg-values))))
                   values))))

  (defun flycheck-eldev--ert-defargtest-format-arguments (arguments)
    (let ((print-quoted t))
      (downcase (replace-regexp-in-string " " "/" (replace-regexp-in-string (rx (not (any word "-" " "))) "" (prin1-to-string arguments)))))))


(defmacro flycheck-eldev--test (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((flycheck-eldev-whitelist        nil)
        (flycheck-eldev-blacklist        nil)
        (flycheck-eldev-unknown-projects 'trust)
        (enabled-checkers                '(emacs-lisp elisp-eldev)))
    (while (keywordp (car body))
      (pcase (pop body)
        (:whitelist        (setf flycheck-eldev-whitelist        (pop body)))
        (:blacklist        (setf flycheck-eldev-blacklist        (pop body)))
        (:unknown-projects (setf flycheck-eldev-unknown-projects (pop body)))
        (:enable-checkdoc  (if (pop body)
                               (push 'emacs-lisp-checkdoc enabled-checkers)
                             ;; We don't use `emacs-lisp-checkdoc' unless explicitly enabled.
                             (setf enabled-checkers (delq 'emacs-lisp-checkdoc enabled-checkers))))))
    ;; Currently not testing if `flycheck-eldev-outside-temp-files' actually achieves what it promises, but at
    ;; least make sure that everything works regardless if that is set to t or nil.
    `(dolist (outside-temp-files '(nil t))
       (ert-info ((format "flycheck-eldev-outside-temp-files = %s" outside-temp-files))
         (let ((flycheck-checkers                   (--filter (memq it ',enabled-checkers) flycheck-checkers))
               (flycheck-disabled-checkers          nil)
               (flycheck-check-syntax-automatically nil)
               (flycheck-eldev-whitelist            ',flycheck-eldev-whitelist)
               (flycheck-eldev-blacklist            ',flycheck-eldev-blacklist)
               (flycheck-eldev-unknown-projects     ',flycheck-eldev-unknown-projects)
               (flycheck-eldev-outside-temp-files   outside-temp-files)
               (file                                (expand-file-name ,file flycheck-eldev--test-dir)))
           (with-temp-buffer
             (insert-file-contents file t)
             (setf default-directory (file-name-directory file))
             (emacs-lisp-mode)
             (flycheck-mode 1)
             ,@body))))))

(defmacro flycheck-eldev--test-with-temp-file (file content-creation &rest body)
  (declare (indent 2) (debug (sexp form body)))
  `(let ((file (expand-file-name ,file flycheck-eldev--test-dir)))
     (ignore-errors (delete-file file))
     (with-temp-file file
       ,content-creation)
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file file)))))

(defun flycheck-eldev--test-recheck ()
  (flycheck-buffer)
  (should (eq flycheck-last-status-change 'running))
  (let ((start-time (float-time)))
    (while (eq flycheck-last-status-change 'running)
      ;; Be generous, what if Eldev needs to install dependencies?
      (when (> (- (float-time) start-time) 30.0)
        (ert-fail "timed out"))
      (accept-process-output nil 0.02))
    (should (eq flycheck-last-status-change 'finished))))

(defun flycheck-eldev--test-expect-no-errors ()
  ;; Only exists for a better name.
  (flycheck-eldev--test-expect-errors))

(defun flycheck-eldev--test-expect-errors (&rest errors)
  (let ((actual-errors   flycheck-current-errors)
        (expected-errors errors))
    (while (or actual-errors expected-errors)
      (let ((actual   (pop actual-errors))
            (expected (pop expected-errors)))
        (cond ((and actual expected)
               (-when-let (regexp (plist-get expected :matches))
                 (unless (string-match-p regexp (flycheck-error-message actual))
                   (ert-fail (format "unexpected error %S: expected message matching '%s'" actual regexp)))))
              (actual
               (ert-fail (format "unexpected error: %S" actual)))
              (expected
               (ert-fail (format "expected error not detected: %S" expected))))))))


(flycheck-eldev-ert-defargtest flycheck-eldev-basics-1 (file)
                               ("project-a/project-a.el" "project-b/project-b.el" "project-b/project-b-util.el")
  (flycheck-eldev--test file
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

(flycheck-eldev-ert-defargtest flycheck-eldev-basics-2 (file)
                               ("project-a/Eldev" "project-b/Eldev")
  (flycheck-eldev--test file
    ;; Enable `checkdoc'.  Must be disabled at runtime (currently through our own hack)
    ;; for the test to pass.
    :enable-checkdoc t
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

(ert-deftest flycheck-eldev-self-1 ()
  (flycheck-eldev--test "../flycheck-eldev.el"
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

(ert-deftest flycheck-eldev-self-2 ()
  (flycheck-eldev--test "flycheck-eldev-test.el"
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

;; This indirectly tests that we see functions in `eldev' namespace from `Eldev' and
;; `Eldev-local'.  Unfortunately, we also see them in normal files, but I cannot think of
;; a robust way to avoid this currently.
(ert-deftest flycheck-eldev-self-3 ()
  (flycheck-eldev--test "../Eldev"
    ;; Enable `checkdoc'.  Must be disabled at runtime (currently through our own hack)
    ;; for the test to pass.
    :enable-checkdoc t
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

(flycheck-eldev-ert-defargtest flycheck-eldev-test-file-1 (file)
                               ("project-a/test/project-a.el" "project-b/test/project-b.el" "project-b/test/project-b-util.el")
  (flycheck-eldev--test file
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-no-errors)))

(ert-deftest flycheck-eldev-deactivating-1 ()
  (let ((flycheck-eldev-active nil))
    (flycheck-eldev--test "project-a/project-a.el"
      (flycheck-eldev--test-recheck)
      (flycheck-eldev--test-expect-errors '(:matches "dependency-a")))))

;; Test that removing dependencies gives errors immediately.
(ert-deftest flycheck-eldev-remove-dependency-1 ()
  (flycheck-eldev--test "project-a/project-a.el"
    (search-forward "(dependency-a \"1.0\")")
    (replace-match "")
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-errors '(:matches "dependency-a"))))

;; Test that adding unaccessible dependencies gives errors immediately.
(ert-deftest flycheck-eldev-add-dependency-1 ()
  (flycheck-eldev--test "project-a/project-a.el"
    (search-forward "(dependency-a \"1.0\")")
    (insert " (some-totally-bullshit-dependency)")
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-errors '(:matches "not available"))))

;; Test that faulty project initialization code is handled fine.
(ert-deftest flycheck-eldev-faulty-eldev-local-1 ()
  (flycheck-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert "this-variable-certainly-doesnt-exist")
    (flycheck-eldev--test "project-a/project-a.el"
      (flycheck-eldev--test-recheck)
      (flycheck-eldev--test-expect-errors '(:matches "cannot be initialized")))))

;; When checking the faulty `Eldev-local', we must not use it for initialization.
(ert-deftest flycheck-eldev-faulty-eldev-local-2 ()
  (flycheck-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert ";; -*- lexical-binding: t; -*-\nthis-variable-certainly-doesnt-exist")
    (flycheck-eldev--test "project-a/Eldev-local"
      (flycheck-eldev--test-recheck)
      (flycheck-eldev--test-expect-errors '(:matches "this-variable-certainly-doesnt-exist")))))

;; Test that `flycheck-eldev' really works on Eldev files if those are byte-compilable.
(ert-deftest flycheck-eldev-suspicious-eldev-local-1 ()
  (flycheck-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert ";; -*- lexical-binding: t; -*-\n(defun just-for-testing () (function-with-this-name-certainly-doesnt-exist))")
    (flycheck-eldev--test "project-a/Eldev-local"
      (flycheck-eldev--test-recheck)
      (flycheck-eldev--test-expect-errors '(:matches "function-with-this-name-certainly-doesnt-exist")))))


(ert-deftest flycheck-eldev-project-trusted-p-1 ()
  (let ((flycheck-eldev-whitelist        nil)
        (flycheck-eldev-blacklist        nil)
        (flycheck-eldev-unknown-projects 'dont-trust))
    (should-not (flycheck-eldev-project-trusted-p "~/foo"))))

(ert-deftest flycheck-eldev-project-trusted-p-2 ()
  (let ((flycheck-eldev-whitelist        nil)
        (flycheck-eldev-blacklist        nil)
        (flycheck-eldev-unknown-projects 'trust))
    (should (flycheck-eldev-project-trusted-p "~/foo"))))

(ert-deftest flycheck-eldev-project-trusted-p-3 ()
  (let ((flycheck-eldev-whitelist        '("~"))
        (flycheck-eldev-blacklist        nil)
        (flycheck-eldev-unknown-projects 'dont-trust))
    (should (flycheck-eldev-project-trusted-p "~/foo"))))

(ert-deftest flycheck-eldev-project-trusted-p-4 ()
  ;; Some ugliness because `file-in-directory-p' fails if `dir' doesn't exist.
  (let* ((foo                              user-emacs-directory)
         (parent                           (file-name-directory (directory-file-name foo)))
         (flycheck-eldev-whitelist        `(,parent))
         (flycheck-eldev-blacklist        `(,foo))
         (flycheck-eldev-unknown-projects 'trust))
    (skip-unless (and (file-directory-p foo) (not (equal parent foo))))
    (should-not (flycheck-eldev-project-trusted-p foo))
    (should-not (flycheck-eldev-project-trusted-p (expand-file-name "bar" foo)))))

(ert-deftest flycheck-eldev-project-trusted-p-5 ()
  (let* ((foo                              user-emacs-directory)
         (parent                           (file-name-directory (directory-file-name foo)))
         (flycheck-eldev-whitelist        `(,foo))
         (flycheck-eldev-blacklist        `(,parent))
         (flycheck-eldev-unknown-projects 'trust))
    (skip-unless (and (file-directory-p foo) (not (equal parent foo))))
    (should (flycheck-eldev-project-trusted-p foo))
    (should (flycheck-eldev-project-trusted-p (expand-file-name "bar" foo)))))


(ert-deftest flycheck-eldev-project-trust-1 ()
  (flycheck-eldev--test "project-a/project-a.el"
    :unknown-projects dont-trust
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-errors '(:matches "not trusted"))))

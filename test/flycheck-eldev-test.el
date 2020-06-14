(require 'flycheck-eldev)
(require 'ert)
(require 'dash)


(defvar flycheck-eldev--test-dir (file-name-directory (or load-file-name (buffer-file-name))))


(defmacro flycheck-eldev--test (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((flycheck-eldev-whitelist        nil)
        (flycheck-eldev-blacklist        nil)
        (flycheck-eldev-unknown-projects 'trust))
    (while (keywordp (car body))
      (pcase (pop body)
        (:whitelist        (setf flycheck-eldev-whitelist        (pop body)))
        (:blacklist        (setf flycheck-eldev-blacklist        (pop body)))
        (:unknown-projects (setf flycheck-eldev-unknown-projects (pop body)))))
    ;; Don't use `emacs-lisp-checkdoc'.
    `(let ((flycheck-checkers                   (--filter (memq it '(emacs-lisp elisp-eldev)) flycheck-checkers))
           (flycheck-disabled-checkers          nil)
           (flycheck-check-syntax-automatically nil)
           (flycheck-eldev-whitelist            ',flycheck-eldev-whitelist)
           (flycheck-eldev-blacklist            ',flycheck-eldev-blacklist)
           (flycheck-eldev-unknown-projects     ',flycheck-eldev-unknown-projects)
           (file                                (expand-file-name ,file flycheck-eldev--test-dir)))
       (with-temp-buffer
         (insert-file-contents file t)
         (setf default-directory (file-name-directory file))
         (emacs-lisp-mode)
         (flycheck-mode 1)
         ,@body))))

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


(ert-deftest flycheck-eldev-basics-1 ()
  (flycheck-eldev--test "project-a/project-a.el"
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

(ert-deftest flycheck-eldev-test-file-1 ()
  (flycheck-eldev--test "project-a/test/project-a.el"
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
      (insert "this is not a valid Lisp")
    (flycheck-eldev--test "project-a/project-a.el"
      (flycheck-eldev--test-recheck)
      (flycheck-eldev--test-expect-errors '(:matches "cannot be initialized")))))


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

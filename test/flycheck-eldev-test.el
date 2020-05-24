(require 'flycheck-eldev)
(require 'ert)
(require 'dash)


(defvar flycheck-eldev--test-dir (file-name-directory (or load-file-name (buffer-file-name))))


(defmacro flycheck-eldev--test (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  ;; Don't use `emacs-lisp-checkdoc'.
  `(let ((flycheck-checkers                   '(emacs-lisp))
         (flycheck-disabled-checkers          nil)
         (flycheck-check-syntax-automatically nil)
         (file                                (expand-file-name ,file flycheck-eldev--test-dir)))
     (with-temp-buffer
       (insert-file-contents file t)
       (setf default-directory (file-name-directory file))
       (emacs-lisp-mode)
       (flycheck-mode 1)
       ,@body)))

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
                   (ert-fail (format-message "unexpected error %S: expected message matching '%s'" actual regexp)))))
              (actual
               (ert-fail (format-message "unexpected error: %S" actual)))
              (expected
               (ert-fail (format-message "expected error not detected: %S" expected))))))))

(ert-deftest flycheck-eldev-basics-1 ()
  (flycheck-eldev--test "project-a/project-a.el"
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
    (insert " (some-totally-bulshit-dependency)")
    (flycheck-eldev--test-recheck)
    (flycheck-eldev--test-expect-errors '(:matches "not available"))))

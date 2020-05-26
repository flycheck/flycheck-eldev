;;; flycheck-eldev.el --- Eldev support in Flycheck  -*- lexical-binding: t -*-

;;; Copyright (C) 2020 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.9
;; Keywords:   tools, convenience
;; Homepage:   https://github.com/flycheck/flycheck-eldev
;; Package-Requires: ((flycheck "32") (dash "2.17") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses.

;;; Commentary:

;; Add Eldev support to Flycheck.

;;; Code:

(require 'flycheck)
(require 'dash)


(defvar flycheck-eldev-active t
  "Whether Eldev extension to Flycheck is active.")

(defvar flycheck-eldev-general-error
  "Eldev cannot be initialized; check dependency declarations and file `Eldev'"
  "Error shown when Eldev cannot be initialized.")

(defvar flycheck-eldev-incompatible-error
  "Package `flycheck-eldev' is no longer compatible with Flycheck; please report a bug"
  "Error shown if the extension is incompatible with future Flycheck.")


(defun flycheck-eldev-find-root (&optional from)
  "Get Eldev project root or nil, if not inside one.
If FROM is nil, search from `default-directory'."
  (-when-let (root (locate-dominating-file
                    (or from default-directory)
                    (lambda (dir) (or (file-exists-p (expand-file-name "Eldev" dir))
                                      (file-exists-p (expand-file-name "Eldev-local" dir))))))
    (expand-file-name root)))


;; Needed because of the advice autoloading at the end.
;;;###autoload
(defun flycheck-eldev--compute-working-directory (original checker &rest etc)
  "Use Eldev project root when checking Elisp.
If not inside an Eldev project or when `flycheck-eldev-active' is
nil, just call the original function."
  (or (when (and flycheck-eldev-active (eq checker 'emacs-lisp))
        (flycheck-eldev-find-root))
      (apply original checker etc)))

;;;###autoload
(defun flycheck-eldev--start-command-checker (original checker callback &rest etc)
  "Use Eldev instead of raw Emacs when appropriate."
  (if (and flycheck-eldev-active (eq checker 'emacs-lisp) (flycheck-eldev-find-root))
      (let* ((flycheck-emacs-lisp-load-path           nil)
             (flycheck-emacs-lisp-initialize-packages nil)
             (original-wrapper-function               flycheck-command-wrapper-function)
             (flycheck-command-wrapper-function
              (lambda (command)
                (funcall original-wrapper-function
                         (flycheck-eldev--generate-command-line command)))))
        (let* ((process         (apply original checker callback etc))
               (file-directory (file-name-directory (buffer-file-name))))
          ;; Hack: Eldev is run from the project root, but Emacs reports syntax errors
          ;; without a path.  Therefore, we reset directory from the root to where the
          ;; file is actually contained after the process is started.
          (process-put process 'flycheck-working-directory file-directory)
          process))
    (apply original checker callback etc)))

(defun flycheck-eldev--generate-command-line (command-line)
  ;; Although we heavily rewrite command line, it is still better to _rewrite_ rather than
  ;; completely replace it.  E.g. we need the filename buried somewhere inside the form,
  ;; which `flycheck-substitute-argument' doesn't support.  So, since we need rewriting
  ;; step anyway, let's take what native Elisp checker wants and massage it, so that we
  ;; get any future improvements for free (unless they break us, of course).
  (or (let* ((head          (cdr (-drop-last 2 command-line)))
             (tail          (-take-last 2 command-line))
             (filename      (cadr tail))
             ;; FIXME: Or is there a better way than getting it from the buffer?
             (real-filename (buffer-file-name))
             eval-forms)
        (while head
          (when (string= (pop head) "--eval")
            (if (string-match-p (rx "(" (or "setq" "setf") " package-user-dir") (car head))
                ;; Just discard, Eldev will take care of this.  Binding
                ;; `flycheck-emacs-lisp-package-user-dir' to nil would not be enough.
                (pop head)
              (push (pop head) eval-forms))))
        ;; Sanity check: fail if the standard checker wants something we don't expect.
        (when (and (string= (car tail) "--")
                   (--any (string-match-p (rx "(byte-compile") it) eval-forms)
                   (--any (string-match-p (rx "command-line-args-left") it) eval-forms))
          `(,(funcall flycheck-executable-find "eldev") "--color=never"
            "--setup"
            ,(flycheck-sexp-to-string
              `(advice-add #'eldev--package-dir-info :around
                           (lambda (original)
                             (eldev-advised
                              (#'insert-file-contents
                               :around
                               (lambda (original filename &rest arguments)
                                 ;; Ignore the original file for project initialization
                                 ;; purposes.  If `eldev-project-main-file' is specified,
                                 ;; this does nothing.
                                 (unless (file-equal-p filename ,real-filename)
                                   ;; Workaround, will probably go into Eldev itself:
                                   ;; `package-dir-info' chokes on unreadable files,
                                   ;; e.g. locks for buffers modified in Emacs.
                                   (ignore-errors (apply original filename arguments)))))
                              (funcall original)))))
            ;; If Eldev cannot be initialized, report a fake error that can be seen in UI.
            ;; FIXME: If there is an error in file `Eldev' we won't get here...
            "--setup"
            ,(flycheck-sexp-to-string
              `(advice-add #'eldev-load-project-dependencies :around
                           (lambda (original &rest etc)
                             (condition-case error
                                 (apply original etc)
                               ;; E.g. mistyped dependency name.
                               (eldev-missing-dependency
                                (let ((message (cdr error)))
                                  (while (keywordp (car message))
                                    (setf message (cddr message)))
                                  (eldev-print "%s:1:1: Error: %s"
                                               ,real-filename (apply #'eldev-format-message message)))
                                (signal 'eldev-quit 1))
                               ;; Handle all other errors generically.
                               (error
                                (eldev-print ,(format "%s:1:1: Error: %s"
                                                      filename flycheck-eldev-general-error))
                                (signal 'eldev-quit 1))))))
            ;; When checking project's main file, use the temporary as the main file instead.
            "--setup"
            ,(flycheck-sexp-to-string
              `(when (and eldev-project-main-file (file-equal-p eldev-project-main-file ,real-filename))
                 (setf eldev-project-main-file ,filename)))
            ;; Special handling for test files: load extra dependencies as if testing now.
            "--setup"
            ,(flycheck-sexp-to-string
              `(when (eldev-filter-files '(,real-filename) eldev-test-fileset)
                 (dolist (dependency (cdr (assq 'test eldev--extra-dependencies)))
                   (eldev-add-extra-dependencies 'exec dependency))))
            "exec" "--dont-require"
            ,(flycheck-sexp-to-string
              `(setf command-line-args-left (list "--" ,filename)))
            ,@(nreverse eval-forms))))
      ;; Fallback in case we fail outright.
      `(,(funcall flycheck-executable-find "emacs") "--batch" "--eval"
        ,(flycheck-sexp-to-string
          `(message "setup:1:1: Error: %s" ,flycheck-eldev-incompatible-error)))))


;; Make the package effectively active even without `require'.
;;;###autoload
(progn
  ;; We don't really define anything new, just hack what Flycheck already provides a bit.
  (advice-add #'flycheck-compute-working-directory :around #'flycheck-eldev--compute-working-directory)
  (advice-add #'flycheck-start-command-checker     :around #'flycheck-eldev--start-command-checker))


(provide 'flycheck-eldev)

;;; flycheck-eldev.el ends here

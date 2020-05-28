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
;;
;; For a project to be detected, it must contain file `Eldev' or
;; `Eldev-local' in its root directory, even if Eldev doesn’t strictly
;; require that.
;;
;; Features:
;;
;; * No additional steps to be performed from the command line, not
;;   even `eldev prepare'.
;;
;; * Project dependencies are seen by Flycheck in Emacs.  Similarly,
;;   if a package is not declared as a dependency of your project,
;;   Flycheck will complain about unimportable features or undeclared
;;   functions.
;;
;; * Everything is done on-the-fly.  As you edit your project’s
;;   dependency list in its main `.el' file, added, removed or
;;   mistyped dependency names immediately become available to
;;   Flycheck (there might be some delays due to network, as Eldev
;;   needs to fetch them first).
;;
;; * Additional test dependencies (see `eldev-add-extra-dependencies')
;;   are seen from the test files, but not from the main files.
;;
;; For the extension to have any effect, you need to install Eldev:
;;
;;     https://github.com/doublep/eldev#installation
;;
;; If Flycheck doesn’t seem to recognize dependencies declared in a
;; project, verify its setup (`C-c !  v').

;;; Code:

(eval-and-compile
  (require 'flycheck)
  (require 'dash))


(defvar flycheck-eldev-active t
  "Whether Eldev extension to Flycheck is active.")

(defvar flycheck-eldev-general-error
  "Eldev cannot be initialized; check dependency declarations and file `Eldev'"
  "Error shown when Eldev cannot be initialized.")

(defvar flycheck-eldev--byte-compilation-start-mark "--8<-- FLYCHECK BYTE-COMPILATION --8<--")


(defun flycheck-eldev-find-root (&optional from)
  "Get Eldev project root or nil, if not inside one.
If FROM is nil, search from `default-directory'."
  (-when-let (root (locate-dominating-file
                    (or from default-directory)
                    (lambda (dir) (or (file-exists-p (expand-file-name "Eldev" dir))
                                      (file-exists-p (expand-file-name "Eldev-local" dir))))))
    (expand-file-name root)))


(defun flycheck-eldev--verify (&rest _)
  (or (unless flycheck-eldev-active
        `(,(flycheck-verification-result-new
            :label "status" :message "Deactivated (see `flycheck-eldev-active')" :face '(bold warning))))
      (let ((root (flycheck-eldev-find-root)))
        `(,(flycheck-verification-result-new
            :label   "project root"
            :message (if root (abbreviate-file-name (directory-file-name root)) "[not detected]")
            :face    (if root 'success '(bold warning)))))))

(defun flycheck-eldev--enabled-p (&rest arguments)
  (let ((super (flycheck-checker-get 'emacs-lisp 'enabled)))
    (or (null super) (apply super arguments))))

(defun flycheck-eldev--predicate (&rest arguments)
  (and flycheck-eldev-active
       (flycheck-eldev-find-root)
       (let ((super (flycheck-checker-get 'emacs-lisp 'predicate)))
         (or (null super) (apply super arguments)))))

(defun flycheck-eldev--working-directory (&rest _)
  (flycheck-eldev-find-root))

(defun flycheck-eldev--build-command-line ()
  ;; If the standard Emacs Lisp checker provides a command line we don't expect, throw it
  ;; away and replace with one based on Flycheck 32.  Otherwise we rewrite the command
  ;; line provided by the standard checker, so we get any future improvements for free.
  (let* ((super         (let ((flycheck-emacs-lisp-load-path           nil)
                              (flycheck-emacs-lisp-initialize-packages nil))
                          (flycheck-checker-substituted-arguments 'emacs-lisp)))
         (head          (-drop-last 2 super))
         (tail          (-take-last 2 super))
         (filename      (cadr tail))
         (real-filename (buffer-file-name))
         eval-forms)
    (while head
      (when (string= (pop head) "--eval")
        (if (string-match-p (rx "(" (or "setq" "setf") " package-user-dir") (car head))
            ;; Just discard, Eldev will take care of this.  Binding
            ;; `flycheck-emacs-lisp-package-user-dir' to nil would not be enough.
            (pop head)
          (push (pop head) eval-forms))))
    (unless (and (string= (car tail) "--")
                 (--any (string-match-p (rx "(byte-compile") it) eval-forms)
                 (--any (string-match-p (rx "command-line-args-left") it) eval-forms))
      ;; If the command line is something we don't expect, use a failsafe.
      (setf eval-forms `(,(flycheck-emacs-lisp-bytecomp-config-form) ,flycheck-emacs-lisp-check-form)))
    ;; Explicitly specify various options in case a user has different defaults.
    `("--quiet" "--no-time" "--color=never"
      "--no-debug" "--no-backtrace-on-abort"
      "--as-is" "--load-newer"
      "--setup"
      ,(flycheck-sexp-to-string
        `(advice-add #'eldev--package-dir-info :around
                     (lambda (original)
                       (eldev-advised
                        (#'insert-file-contents
                         :around
                         (lambda (original filename &rest arguments)
                           ;; Ignore the original file for project initialization
                           ;; purposes.  If `eldev-project-main-file' is specified, this
                           ;; does nothing.
                           (unless (file-equal-p filename ,real-filename)
                             ;; Workaround, will probably go into Eldev itself:
                             ;; `package-dir-info' chokes on unreadable files,
                             ;; e.g. locks for buffers modified in Emacs.
                             (ignore-errors (apply original filename arguments)))))
                        (funcall original)))))
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
      "exec" "--load" "--dont-require" "--lexical"
      ,(flycheck-sexp-to-string `(eldev-output ,flycheck-eldev--byte-compilation-start-mark))
      ,(flycheck-sexp-to-string `(setf command-line-args-left (list "--" ,filename)))
      ,@(nreverse eval-forms))))

(defun flycheck-eldev--parse-errors (output checker buffer &rest _)
  (or (flycheck-parse-output output 'emacs-lisp buffer)
      ;; If there are no errors from Emacs
      (unless (string-match-p (regexp-quote flycheck-eldev--byte-compilation-start-mark) output)
        (let ((message (string-trim output)))
          ;; Don't add clarification to a few obvious errors.
          (unless (string-match-p (rx bos "Dependency " (1+ any) " is not available") message)
            (setf message (concat message "\n\n" flycheck-eldev-general-error)))
          `(,(flycheck-error-new-at 1 1 'error message
                                    :end-column (with-current-buffer buffer
                                                  (save-excursion
                                                    (save-restriction
                                                      (widen)
                                                      (goto-char 1)
                                                      (end-of-line)
                                                      (point))))
                                    :checker    checker
                                    :buffer     buffer))))))

(defun flycheck-eldev--filter-errors (errors &rest _)
  ;; Don't filter our own errors.
  (if (and errors (eq (flycheck-error-checker (car errors)) 'elisp-eldev))
      errors
    (flycheck-filter-errors errors 'emacs-lisp)))


;;;###autoload
(defun flycheck-eldev--initialize ()
  (add-to-list 'flycheck-checkers 'elisp-eldev)
  (flycheck-define-checker elisp-eldev
    "An Emacs Lisp syntax checker for files in Eldev projects.

This is based on the standard built-in Emacs Lisp checker.  This
checker differs in that it uses dependencies declared in Eldev
projects to build `load-path' and initialize the package manager.

See Info Node `(elisp)Byte Compilation'."
    :verify            flycheck-eldev--verify
    :modes             emacs-lisp-mode
    :enabled           flycheck-eldev--enabled-p
    :predicate         flycheck-eldev--predicate
    :working-directory flycheck-eldev--working-directory
    :command           ("eldev" (eval (flycheck-eldev--build-command-line)))
    :error-parser      flycheck-eldev--parse-errors
    :error-filter      flycheck-eldev--filter-errors
    :next-checkers     (emacs-lisp-checkdoc))
  ;; Deactivate 0.9 hacks.
  (advice-remove 'flycheck-compute-working-directory 'flycheck-eldev--compute-working-directory)
  (advice-remove 'flycheck-start-command-checker     'flycheck-eldev--start-command-checker))


;;;###autoload
(eval-after-load 'flycheck '(flycheck-eldev--initialize))


(provide 'flycheck-eldev)

;;; flycheck-eldev.el ends here

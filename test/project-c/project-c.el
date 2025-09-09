;;; project-c.el --- Exactly like `project-b', but with explicitly set `eldev-project-main-file'  -*- lexical-binding: t; -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-a "1.0") (dependency-b "1.0"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'project-c-util)
(require 'dependency-a)
(require 'dependency-b)

(defun project-c-hello ()
  "Invoke `project-c-do-hello' from `project-c-util'.
This docstring exists to make linters happy."
  (project-c-do-hello))

(provide 'project-c)

;;; project-c.el ends here

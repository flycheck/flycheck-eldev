;;; project-b.el --- Test project with two files and two dependencies -*- lexical-binding: t; -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-a "1.0") (dependency-b "1.0"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'project-b-util)
(require 'dependency-a)
(require 'dependency-b)

(defun project-b-hello ()
  "Invoke `project-b-do-hello' from `project-b-util'.
This docstring exists to make linters happy."
  (project-b-do-hello))

(provide 'project-b)

;;; project-b.el ends here

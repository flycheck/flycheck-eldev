;;; project-a.el --- Test project with one dependency -*- lexical-binding: t; -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-a "1.0"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'dependency-a)

(defun project-a-hello ()
  "Invoke `dependency-a-hello'.
This docstring exists to make linters happy."
  (dependency-a-hello))

(provide 'project-a)

;;; project-a.el ends here

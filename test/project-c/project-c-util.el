;;; -*- lexical-binding: t; -*-

(require 'dependency-a)
(require 'dependency-b)

(defun project-c-do-hello ()
  (dependency-a-hello))

(provide 'project-c-util)

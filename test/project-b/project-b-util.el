;;; -*- lexical-binding: t; -*-

(require 'dependency-a)
(require 'dependency-b)

(defun project-b-do-hello ()
  (dependency-a-hello))

(provide 'project-b-util)

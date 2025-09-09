;; -*- lexical-binding: t; -*-

(require 'test/project-c-util)

(ert-deftest project-c-test-hello ()
  (should (string= (project-c-hello) "Hello")))

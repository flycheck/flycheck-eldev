;; -*- lexical-binding: t; -*-

(require 'test/project-b-util)

(ert-deftest project-b-test-hello ()
  (should (string= (project-b-hello) "Hello")))

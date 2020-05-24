(require 'project-a)
(require 'ert)

(ert-deftest project-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))

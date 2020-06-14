(require 'test/test-util)

(ert-deftest project-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))

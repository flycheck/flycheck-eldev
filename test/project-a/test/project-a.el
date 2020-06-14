;; Using loading root to test that `flycheck-eldev' handles this too.
(require 'test-util)

(ert-deftest project-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))

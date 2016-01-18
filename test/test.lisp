; Simple tests with RT
(require "server" "../src/server.lisp")
(eval-when (:compile-toplevel)
    (ql:quickload :lisp-unit))

(lisp-unit:define-test smoke-test
  (lisp-unit:assert-equal "<html>" (subseq (handle-tickets) 0 6)))

(lisp-unit:run-tests)

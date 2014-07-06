(in-package :symta)

#|
(defun prof ()
  (sb-sprof:profile-call-counts "SYMTA")
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :flat
                            :loop nil)
    (repl "f [@x@x@x@x@x@x@x@x@x] -> x")
    ;;($load "tests/db.st")
    ;;(repl "map [x@_]~>x (age [_ 23])")
    ))
|#

#|
(load "symta.lisp")
|#

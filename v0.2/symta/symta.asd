(defpackage :symta-system (:use :cl :asdf))

(in-package :symta-system)

;;(eval-when (:load-toplevel :execute)
;;  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(defsystem :symta
  :name "symta"
  :description "Symta Lisp-system"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi :babel :libc :snv :gfx :sdl)
  :serial t
  :components
    ((:file "symta")      ; root definitions
     (:file "tree")       ; list base structure
     (:file "list")       ; list advance structures
     (:file "symbol")     ; symbol system
     (:file "reader")     ; input routines
     (:file "printer")    ; pretty printing routines
     (:file "eval")       ; evaluator
     (:file "matcher")    ; matching macros
     (:file "defs")       ; built-in functions
     ;;(:file "init")       ; rest of base library and environment
     ))

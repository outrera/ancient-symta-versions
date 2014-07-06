(defpackage :symta-system
  (:use :cl :asdf))

(in-package :symta-system)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(defsystem :symta
  :name "symta"
  :description "Symta Lisp-system"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi :babel :trivial-shell :trivial-garbage
               :sdl :sdl-image :sdl-mixer)
  :components
  ((:module "symta" :serial t
    :components
      ((:file "host")       ; hosting system specifc routines
       (:file "util")       ; implementation helpers
       (cffi-grovel:grovel-file "png-grovel")
       (:file "gfx")        ; graphics routines
       ;;(:file "audio")      ; audio routines
       (:file "root")       ; root definitions
       (:file "tree")       ; list base structure
       (:file "list")       ; list advance structures
       (:file "pkg")        ; package system
       (:file "io-root")    ; io-root definitions
       (:file "io-printer") ; list pretty printing routines
       (:file "io-reader")  ; list input routines
       (:file "eval")       ; list evaluation macros
       (:file "matcher")    ; list matching macros
       (:file "defs")       ; built-in functions
       (:file "init")       ; rest of base library and environment
       )))
  )


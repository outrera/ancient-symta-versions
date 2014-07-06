(defsystem libc
  :name "libc"
  :description "C/C++ and POSIX exports"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi)
  :serial t
  :components ((:file "libc")))

(defsystem snv
  :name "snv"
  :description "Common defs used by my packages"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi :libc)
  :serial t
  :components ((:file "snv")))

(defsystem gfx
  :name "gfx"
  :description "Graphics manipulation routines"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi :libc :snv :zlib)
  :serial t
  :components
    ((:file "gfx")
     (:file "png")
     (:file "pcx")
     ))

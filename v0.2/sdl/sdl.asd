(defsystem sdl
  :name "sdl"
  :description "SDL FFI extends Gfx package with interactive graphics and sound routines"
  :license "MIT"
  :author "snv"
  :version "0.1"
  :depends-on (:cffi :libc :snv :gfx :trivial-garbage)
  :serial t
  :components ((:file "sdl")))

(defpackage #:sdl-system (:use #:cl #:asdf))
(in-package #:sdl-system)

(defsystem sdl
    :depends-on (cffi)
    :components
    ((:module "sdl"
	      :components
	      ((:file "package")
	       (:file "library")
	       (:file "util")
	       (:file "cffi-util")
	       (:file "cffi-translate")
	       (:file "endian")
	       (:file "version")
	       (:file "stdinc")
	       (:file "timer")
	       (:file "error")
	       (:file "rwops")
	       (:file "audio")
	       (:file "cdrom")
	       (:file "joystick")
	       (:file "active")
	       (:file "keysym")
	       (:file "mouse")
	       (:file "events")
	       (:file "syswm")
	       (:file "video")
	       (:file "sdl"))
	      :serial t)
     (:module "build"
	      :components
	      ((:static-file "sdlswig.i")))))

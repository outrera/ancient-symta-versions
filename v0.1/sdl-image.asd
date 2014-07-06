;;; -*- lisp -*-

(defpackage #:sdl-image-system
  (:use #:cl #:asdf))

(in-package #:sdl-image-system)

(defsystem sdl-image
    :depends-on (cffi sdl)
    :components
    ((:module "sdl-image"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "image" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("image"))
	       ))))

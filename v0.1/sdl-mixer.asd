;;; -*- lisp -*-

(defpackage #:sdl-mixer-system (:use #:cl #:asdf))
(in-package #:sdl-mixer-system)

(defsystem sdl-mixer
    :depends-on (cffi sdl)
    :components
    ((:module "sdl-mixer"
	      :components
	      (;(:static-file "SDL_mixer.dll")
	       ;(:static-file "ogg.dll")
	       ;(:static-file "smpeg.dll")
	       ;(:static-file "vorbis.dll")
	       ;(:static-file "vorbisfile.dll")
           (:file "package")
	       (:file "library" :depends-on ("package"))
 	       (:file "mixer" :depends-on ("package"))
	       (:file "sdl-mixer" :depends-on ("package" "library"))))))

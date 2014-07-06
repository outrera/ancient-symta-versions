
(in-package #:sdl-mixer)

(defclass chunk (sdl::foreign-object) ()
  (:default-initargs
   :gc t
    :free #'sdl-mixer::free-chunk))

(defclass music (sdl::foreign-object) ()
  (:default-initargs
   :gc t
    :free #'sdl-mixer::free-music))


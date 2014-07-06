;;(require :babel)
;;(require :sb-sprof)
;;(require :iolib)
;;(require :trivial-garbage)

(defpackage :symta (:use :cl :host :util :babel))
(defpackage :|st|)
(in-package :symta)


;;(proclaim '(optimize (speed 3)))

;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)
;;                   (compilation-speed 0)))


(defparameter  *env*              nil)   ; code walker environment
(defparameter  *reader-macros*    nil)   ; reader's pattern tree
(defparameter  *operators*        nil)   ; operators for printer and reader
(defparameter  *read-input*       nil)   ; beginning of input stream
(defparameter  *cl-pkg*          "st")   ; CL package for symta's symbols


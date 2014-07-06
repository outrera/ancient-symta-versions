(defpackage :symta (:use :cl :babel :libc :snv :gfx))
(defpackage :|st| (:export |do| |_if| |_fn| |_q| |set_l| |:|))
(in-package :symta)


;;(proclaim '(optimize (speed 3)))

;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)
;;                   (compilation-speed 0)))


(defparameter  *env*              nil)   ; code walker environment

;; symta's package for predefined symbols
(defparameter *pkg* "st")

;; current sub-package, we store symbols in
(defparameter *sub-pkg* nil)


(defun d (x)
  (format t "<%~a%>" x)
  x)


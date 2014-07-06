(in-package :symta)

(defparameter *sym-decor* #'glb-decor) ; symbol decorator
(defparameter *pkgs* nil)
(defparameter *types* (make-hash-table :test 'equal))

(setf (gethash "i" *types*) 'fixnum)
(setf (gethash "f4" *types*) 'single-float)
(setf (gethash "f8" *types*) 'double-float)
(setf (gethash "is" *types*) #(simple-array fixnum))
(setf (gethash "f4s" *types*) #(simple-array single-float))
(setf (gethash "f8s" *types*) #(simple-array double-float))

(defun type-sym? (x) (gethash x *types*))



(defun set-prop (sym prop-name value)
  (setf (get (intern sym :|st|) (intern prop-name)) value))
(defun get-prop (sym prop-name)
  (get (intern sym :|st|) (intern prop-name)))


;; our custom gensym version
(defvar *unisym-counter* 0)
(defun unisym (&optional (name "G"))
  (incf *unisym-counter*)
  (format nil "~a~a&" name *unisym-counter*))

(defun unisym? (x)
  (and (sym? x) (> (length x) 1)
       (char= (aref x (- (length x) 1)) #\&)))

(defmacro with-unisyms (names &body body)
  `(let ,(loop for name in names collect
              `(,name (unisym ,(string name))))
     ,@body))

(defun env-sym? (sym &optional (env *env*))
  (when (symbolp sym)
    (setf sym (symbol-name sym)))
  (when (sym? sym)
    (ns-get env sym)))

(defun env-push (args &optional (env *env*))
  ($map (fn (x) (setf env (ns-set env x 1)))
        args)
  env)

(defun macro? (s)
  (unless (sym? s) (return-from macro? nil))
  (when (not (env-sym? s)) ; macros can be shadowed by local name
    (get-prop s "macro")))

(defun checker-sym? (x)
  (unless (sym? x) (return-from checker-sym? nil))
  (and (> (length x) 0)
       (equal #\? (aref x (- (length x) 1)))))

(defun new-sym (src name)
  (unless (str? name) (setf name (sq-to-str name)))
  (meta-set (ns-set nil "Src" src) name)
  name)


(defun get-package (x)
  (if-bind p (find-package x)
    p
    (make-package x :use '(|st|))))

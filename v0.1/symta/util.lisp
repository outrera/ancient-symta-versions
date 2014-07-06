;; Host independent utils go here

(in-package :cl-user)
(defpackage :util
  (:use :cl :trivial-shell)
  (:export
       group
       with-gensyms
       fn
       function-name
       bind
       while
       if-bind
       while-bind
       map-each
       to-kw
       to-float
       pos-if
       split-if
       implode-string
       explode
       alpha?
       digit?
       whitespace?
       gensym?
       align
       true
       false
       package-symbols
       maximum
       infix
       mapred
       tree-depth
       tree-leaves
       purge-var
       load-file-into-string
       getenv
       shell
       ))

(in-package :util)


;;(proclaim '(optimize speed))


(defmacro with-gensyms (names &body body)
  `(let ,(loop for name in names collect `(,name (gensym ,(string name))))
     ,@body))

(defmacro while (expr &body body)
  (with-gensyms (var)
    `(do ((,var ,expr ,expr))
         ((not ,var))
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-args-names (as)
    (remove-if #'null (mapcar (lambda (a)
                                (cond ((consp a) (first a))
                                      ((case a ((&rest &key &body) t)) nil)
                                      ((symbolp a) a)))
                              as))))

(defmacro fn (args &body body)
  `(lambda ,args
     (declare ,@(mapcar (lambda (x) `(ignorable ,x))
                        (lambda-args-names args)))
     ,@body))

(defun function-name (f)
  (multiple-value-bind (x y name)
      (function-lambda-expression f)
    (declare (ignorable x) (ignorable y))
    (if (symbolp name) name)))

;; group list elements by n
;; (group 2 '(1 2 3 4 5 6 7)) => ((1 2) (3 4) (5 6) (7))
;; (group 2 '(1 2 3 4 5 6 7) :pad nil) => ((1 2) (3 4) (5 6) (7 NIL))
(defun group (n xs &key (pad :dont-pad))
  (let ((out nil))
    (while xs
      (let (l)
        (dotimes (_ n)
          (if xs
              (progn (push (car xs) l)
                     (pop xs))
              (unless (eql pad :dont-pad) (push pad l))))
        (push (nreverse l) out)))
    (nreverse out)))


(defmacro bind (args value-form &body body)
  `(destructuring-bind ,args ,value-form
     (declare ,@(mapcar (fn (x) `(ignorable ,x)) (lambda-args-names args)))
     ,@body))

(defmacro if-bind (var test &body then-else)
  `(let ((,var ,test))
     (declare (ignorable ,var))
     (if ,var
         ,(first then-else)
         ,(second then-else))))

(defmacro while-bind (var expr &body body)
  `(do ((,var ,expr ,expr))
       ((not ,var))
     ,@body))

(defmacro map-each ((element list) &body body)
  `(mapcar #'(fn (,element) ,@body) ,list))

(defun to-kw (s)
  (if (symbolp s) (setf s (symbol-name s)))
  (intern s "KEYWORD"))

(defun pos-if (val seq &key (start 0))
  (if (functionp val)
      (position-if val seq :start start)
      (position val seq :start start)))

(defun split-if (&rest args)
  "Usage: `(split-if ,@prediactes-or-elements sequence)"
  (when (null (cdr args))
    (return-from split-if (cons #\  args)))
  (labels
    ((split-rest (&rest args)
       (bind (seq sep &rest rest-seps) args
         (loop as i = 0 then (1+ j)
            as j = (pos-if sep seq :start i)
            collect (let ((ss (subseq seq i j)))
                      (if rest-seps
                          (apply #'split-rest ss rest-seps)
                          ss))
            while j))))
    (apply #'split-rest (reverse args))))


(defun implode-string (str &optional (sep ""))
  (reduce (fn (a b) (concatenate 'string a (string sep) b)) str))

(defun explode (object)
  (if (symbolp object) (setf object (symbol-name object)))
  (coerce object 'list))


(defun alpha? (x)
  (if (and (characterp x) (alpha-char-p x)) x))

(defun digit? (x)
  (if (and (characterp x) (digit-char-p x)) x))

(defun whitespace? (x)
  (or (equal x #\Space)
      (equal x #\Tab)
      ;(equal x #\Newline)
      ))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(declaim (inline align))
(defun align (m v)
  (declare (optimize (safety 0) (speed 3))
           (fixnum m) (fixnum v))
  (let ((r (the fixnum (rem v m))))
    (if (/= r 0)
        (the fixnum (+ v (the fixnum (- m r))))
        v)))


(defun true (x) (declare (ignorable x)) t)
(defun false (x) (declare (ignorable x)) t)


(defun package-symbols (package)
  (unless (packagep package)
    (setq package (find-package package))
    (unless package (return-from package-symbols nil)))
  (let ((r nil))
    (with-package-iterator (next-symbol `(,package) :internal :external)
      (loop (multiple-value-bind (more? symbol) (next-symbol)
              (if more?
                  (push symbol r)
                  (return-from package-symbols r)))))))

;; return item with maximum score
(defun maximum (p xs)
  (unless xs (return-from maximum nil))
  (let* ((cur (car xs)) (cur-score (funcall p cur)))
    (loop as x in (cdr xs) do
         (let ((score (funcall p x)))
           (when (> score cur-score)
             (setf cur       x
                   cur-score score))))
    cur))

(defun infix (sep xs)
  (labels ((rec (ys xs) (if xs
                            (rec `(,(car xs) ,sep ,@ys) (cdr xs))
                            (nreverse ys))))
    (if xs (rec (list (car xs)) (cdr xs)))))

(defmacro mapred (f g l)
  `(reduce ,g (mapcar ,f ,l)))

(defun tree-depth (x)
  (if (atom x) 0 (+ 1 (reduce #'max (mapcar #'tree-depth x)))))

(defun tree-leaves (x)
  (if (atom x) 1 (reduce #'+ (mapcar #'tree-leaves x))))

(defun purge-var (var)
  (makunbound var)
  (unintern var))


(defun load-file-into-string (file)
  (with-output-to-string (out)
    (with-open-file (in file :direction :input)
      (loop with l while (setf l (read-line in nil nil))
	   do (progn (fresh-line out)
		     (format out "~a" l))))))



(defun getenv (name &optional default)
  (or (trivial-shell:get-env-var name) default))

(defun shell (command)
  (split-if #\Newline (shell-command command)))


#|
(load "util.lisp")
|#

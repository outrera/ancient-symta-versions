(in-package :symta)

(defparameter *prn-pretty*   t)
(defparameter *prn-sym-esc*  t)
(defparameter *prn-max-elem* 150)
(defparameter *prn-width*    80)
(defparameter *prn-depth*    0)
(defparameter *prn-indent*   0)

;; escape characters
(defun escape-chs (o ic &key (enclose "") (pred #'false))
  (assert (stringp o) nil "escape-chs: string expected")
  (let* ((el (if (consp enclose) (first enclose) enclose))
         (er (if (consp enclose) (second enclose) enclose)) 
         (e  (= (length o) 0))
         (ec (aref er 0))
         (r (make-array '(0) :element-type 'character
                        :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s r)
      (loop as c across o do
        (progn (when (funcall pred c) (setf e t))
               (format s "~a"
                       (cond ((eql c ec)
                              (setf c (if (char= c #\") "\\\"" "\\`")))
                             ((eql c ic) "\\$")
                             ((eql c #\Newline) "\\n")
                             (t c))))))
    (if e
        (concatenate 'string el r er)
        r)))

(defun prn-sym (o)
  (cond
    ((string= o "")  "``")
    ((string= o (string #\Newline))  "`\\n`")
    ((or (string= o "=") (string= o ";") (string= o "::")  (string= o "|"))
     o)
    (t (escape-chs o  nil :enclose "`"
         :pred (if *prn-sym-esc*
                   (if (not (/sym-head? (aref o 0)))
                       #'true
                       (fn (x) (not (/sym-body? x))))
                   #'false)))))

(defun prn-str (o)
  (escape-chs o #\$ :enclose '("“" "”") :pred #'true))

(defun prn-sq (xs &key (l "(") (r ")") (dots nil) (sort nil))
  (declare (ignorable sort))
  ;; FIXME: check against *prn-depth* limit
  (unless xs (return-from prn-sq (concatenate 'string l r)))
  (let* ((*prn-depth*    (+ *prn-depth* 1))
         (*prn-indent*   (+ *prn-indent* 1))
         (xs (fold ""
                   (fn (a b)
                     (if (string= a "")
                         (prn b)
                         (concatenate 'string a " " (prn b))))
                   xs)))
    (if dots (setf xs (concatenate 'string xs " ...")))
    (setf xs (concatenate 'string l xs r))
    xs))

(defun remove-zeroes (x)
  (let ((i (- (length x) 1)))
    (while (and (eql (aref x i) #\0)
                (not (eql (aref x (- i 1)) #\.)))
      (decf i))
    (subseq x 0 (+ i 1))))

#|(defun form-to-op (x)
  (and (lst? x) (stringp (1st x)) (ns-get *operators* (1st x))))

(defun op-power (x)
  (let ((o (form-to-op x)))
    (cond (o (mapred #'car #'max (op-binds o)))
          (t 99))))

(defun op-arity (x)
  (if-bind x (form-to-op x)
    (if-bind bs (op-binds x)
      (let ((a (reduce (fn (x y) (concatenate 'list x y))
                       (mapcar #'cdr bs))))
        (if (every (fn (x) (case x ((:l :r) t))) a)
            a)))))|#

(defun self-prints? (xs found)
  (unless xs (return-from self-prints? found))
  (let ((x (lhd xs))
        (xs (ltl xs)))
    (if (lst? x)
        (if (equ (1st x) "_prn")
            (self-prints? xs (2nd x))
            (self-prints? xs found))
        nil)))

; print object to string
(defun prn (o)
  (cond
    ((and (quote? o) (= (len o) 2) (stringp (2nd o))) (prn-str (2nd o)))
    ((stringp o) (prn-sym o))
    ((lst? o)
     (let* ((x  (1st o))
            #|(y  (2nd o))
            (z  (3rd o))
            (op (form-to-op o))
            (n  (if op (op-match op)))
            (ar (if op (op-arity o)))
            (lar (length ar))|#
            )
       #|(when (and *prn-pretty* op (not (eql x (st |=|))) (= lar (- (len o) 1)))
         (case lar
           (1 (let* ((*prn-depth* (+ *prn-depth* 1))
                     (va (op-arity y))
                     (a  (prn y))
                     (a  (if (and va (/= (length va) 1))
                             (concatenate 'string "(" a ")")
                             a)))
                (return-from prn (if (eql (car ar) :r)
                                     (concatenate 'string n a)
                                     (concatenate 'string a n)))))
           (2 (let* ((*prn-depth* (+ *prn-depth* 1))
                     (a (prn y))
                     (b (prn z)))
                (if (> (op-power o) (op-power y))
                    (setf a (concatenate 'string "(" a ")")))
                (if (> (op-power o) (op-power z))
                    (setf b (concatenate 'string "(" b ")")))
                (return-from prn (concatenate 'string a n b))))))|#
       (unless (= 2 (len o)) (setf x nil))
       ($case x
         (|[]| (prn-sq (2nd o) :l "[" :r "]"))
         (|<:>| (prn-sq (2nd o) :l "<:" :r ">"))
         (|<>| (prn-sq (2nd o) :l "<" :r ">"))
         (|\\| (prn-sq (2nd o) :l "{" :r "}"))
         (otherwise
          (let* ((n *prn-max-elem*)
                 (o (take (+ n 1) o))
                 (dots (> (len o) n)))
            (if-bind p (and (not dots) (self-prints? o nil))
              (concatenate 'string "$(" (funcall p) ")")
              (prn-sq (take n o) :dots dots :sort t)
              ))))))
    ((gs? o) (let ((r nil)
                   (n *prn-max-elem*))
               (while (and o (> n 0))
                 (setf r (suf (gs-lhd o) r))
                 (setf o (gs-ltl o))
                 (decf n))
               (prn-sq r :dots o :sort t)))
    ((num? o) (let* ((v (if (minusp o) (- o) o))
                     (x (if (floatp v)
                            (remove-zeroes (format nil "~30$" v))
                            (format nil "~a" v))))
                ;; FIXME: convert exponent to "^" power
                (if (minusp o) (setf x (format nil "~~~a" x)))
                x))
    ((symbolp o)
     (cond ((null o) "ø")
           ((eql o T) "√")
           (t (symbol-name o) #|(error "prn #~a: symbols are disallowed" o)|#)))
    ((functionp o)
     (concatenate 'string
      "$(fn Name:" (if-bind n (function-name o) (prn (symbol-name n)))
      " Adr:" (format nil "#x~x" (snv::object-address o))
      ")"))
    ;((op? o)     (op-match o))
    ((typep o 'SB-SYS:SYSTEM-AREA-POINTER)
     (format nil "$(ptr #x~x)" (cffi::pointer-address o)))
    ((hash-table-p o) (prn (unhash o)))
    (t (format nil "~A" o)) ; FIXME: get rid of this
    ))

(defun pp (o) ; pretty-print
  (format t "~a~%" (sq-to-str (prn o))))

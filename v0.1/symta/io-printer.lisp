(in-package :symta)


(defparameter *prn-pretty*   t)
(defparameter *prn-max-elem* 150)
(defparameter *prn-width*    80)
(defparameter *prn-depth*    0)
(defparameter *prn-indent*   0)


;; escape characters
(defun escape-chs (o &key (enclose "") (pred #'false))
  (unless (str? o) (error "escape-chs: string expected"))
  (let ((e   "")
        (s   "")
        (chs (as-list o))
        (e1  (1st enclose)))
    ($map (fn (c)
            (when (funcall pred c) (setf e enclose))
            (setf s (conc s
                          (cond ((eql c e1)          (conc "\\" enclose))
                                ((eql c #\Newline)   "\\n")
                                ((eql c #\")         "\\\"")
                                ((eql c #\\)         "\\\\")
                                (t (format nil "~a" c))))))
         chs)
    (conc e s e)))

(defun prn-sym (o)
  (if (= (len o) 0) (return-from prn-sym "``"))
  (escape-chs o :enclose "`"
                :pred (if (not (equ (lhd o) #'sym-hd))
                          #'true
                          (fn (x) (not (symbol-char? x))))))

(defun prn-str (o)
  (conc "\"" (escape-chs o) "\""))

(defun pad-sq-elem (align item-len x)
  (let ((pad (- item-len (len x))))
    (case align
      (:left (conc x (dup pad #\Space)))
      (:right (conc (dup pad #\Space) x))
      (otherwise (let ((a (truncate pad 2))
                       (b (ceiling pad 2)))
                   (conc (dup a #\Space) x (dup b #\Space)))))))

(defun prn-sq-line (align ws x)
  (suf #\Newline 
       (reduce #'conc (mapcar (fn (x w) (pre #\Space (pad-sq-elem align w x)))
                              x ws))))

(defun sorted-list? (xs)
  (let ((prev nil))
    (all (fn-k (x)
           (and (lst? x) (let ((h (lhd x)))
                           (when (and (str? h) (lte prev h))
                             (setf prev h)
                             t))))
         xs)))

(defun prn-sq (s &key (l "(") (r ")") (align :left) (dots nil)
               (sort nil))
  ;; FIXME: check against *prn-depth* limit
  (unless s (return-from prn-sq (conc l r)))

  (when (and *prn-pretty* sort (sorted-list? s))
    (setf l "["  r "]")
    (setf s ($map (fn (x) (st (= $($intern (1st x)) $(2nd x)))) s))
    )

  (let* ((*prn-depth*    (+ *prn-depth* 1))
         (*prn-indent*   (+ *prn-indent* 1))
         (xs             (mapcar (fn (x) (split #\Newline (prn x)))
                                 (sq-to-list s)))
         (free-space     (max 0 (- *prn-width* *prn-indent*)))
         (item-len       (+ 1 (max 1 (maximum #'identity
                                              (mapcar (fn (x) (len (1st x)))
                                                      xs)))))
         (ipl            (max 1 (truncate free-space (+ item-len 1))))
         (ipl            (min (len s) ipl))
         (nl             (find-if (fn (x) (> (len x) 1))
                                  xs))
         (xs             (sq-to-list (reduce #'conc xs)))
         (o              nil))
    (setf o
      (reduce #'conc
        (if (or nl (= ipl 1))
            (mapcar (fn (x) (conc " " x (lst #\Newline))) xs)
            (let* ((gs (group ipl xs :pad ""))
                   (ws (mapcar (fn (g) (mapcar #'len g)) gs))
                   (ws (reduce (fn (x y) (mapcar #'max x y))
                               ws)))
              (mapcar (fn (x) (prn-sq-line align ws x))
                      gs)))))
    (setf o (rtl (ltl o)))
    (if dots (setf o (conc o " ...")))
    (setf o (conc l o r))
    o))

(defun prn-blk (o)
  (let ((r "{"))
    (setf o (ltl o))
    (while o
      (let ((hd (lhd o))
            (tl (ltl o)))
        (setf r (conc r (prn-sq hd :l "" :r (if tl "; " ""))))
        (setf o tl)))
    (conc r "}")))

(defun remove-zeroes (x)
  (let ((i (- (length x) 1)))
    (while (and (eql (aref x i) #\0)
                (not (eql (aref x (- i 1)) #\.)))
      (decf i))
    (subseq x 0 (+ i 1))))

(defun cut-match (a b)
  (if (and a b (equ (lhd a) (lhd b)))
      (cut-match (ltl a) (ltl b))
      (if a nil b)))

(defun form-to-op (x)
  (and (lst? x) (sym? (1st x)) (ns-get *operators* (sym-path-name (1st x)))))

(defun op-power (x)
  (let ((x (form-to-op x)))
    (if x
        (mapred #'car #'max (op-binds x))
        99)))

(defun op-arity (x)
  (if-bind x (form-to-op x)
    (if-bind bs (op-binds x)
      (let ((a (reduce (fn (x y) (concatenate 'list x y))
                       (mapcar #'cdr bs))))
        (if (every (fn (x) (case x ((:l :r) t))) a)
            a)))))

; print object to string
(defun prn (o)
  (cond
    ((pt? o)
     (let* ((x  (1st o))
            (y  (2nd o))
            (z  (3rd o))
            (op (form-to-op o))
            (n  (if op (op-match op)))
            (ar (if op (op-arity o)))
            (lar (length ar)))
       (when (and *prn-pretty* op (not (eql x (st |=|))) (= lar (- (len o) 1)))
         (case lar
           (1 (let* ((va (op-arity y))
                     (a  (prn y))
                     (a  (if (and va (/= (length va) 1))
                             (conc "(" a ")")
                             a)))
                (return-from prn (if (eql (car ar) :r)
                                     (conc n a)
                                     (conc a n)))))
           (2 (let ((a (prn y))
                    (b (prn z)))
                (if (> (op-power o) (op-power y))
                    (setf a (conc "(" a ")")))
                (if (> (op-power o) (op-power z))
                    (setf b (conc "(" b ")")))
                (return-from prn (conc a n b))))))
       ($case x
         (lst (prn-sq (ltl o) :l "[" :r "]"))
         (fn  (conc (prn-sq y :l "{" :r "")
                    " -> "
                    (prn-sq z :l "" :r "}")))
         (|=| (let* ((a (conc (prn y) "="))
                     (b (prn z))
                     (b (split #\Newline b))
                     (c (conc a (lhd b)))
                     (pad (dup (len a) #\Space))
                     (b (pre c ($map (fn (x) (conc pad x)) (ltl b)))))
                (unsplit #\Newline b)))
         ;;(blk (prn-blk o))
         (otherwise
          (let ((n *prn-max-elem*))
            (prn-sq (take n o) :dots (> (len o) n) :sort t))))))
    ((gs? o) (let ((r nil)
                   (n *prn-max-elem*))
               (while (and o (> n 0))
                 (setf r (suf (gs-lhd o) r))
                 (setf o (gs-ltl o))
                 (decf n))
               (prn-sq r :dots o :sort t)))
    ((str? o)  (prn-str o))
    ((arrayp o) (if (> (length o) *prn-max-elem*)
                    (setf o (subseq o 0 (+ *prn-max-elem* 1))))
                (prn (lify o)))
    ((symbolp o)
     (cond ((null o) "n")
           ((eql o t) "y")
           (t (let* ((p (split #\; (symbol-name o))))
                (if (= (len p) 1)
                    (1st p)
                    (let* ((c (cut-match *cur-pkg* p))
                           (r (sq-to-str (unsplit #\; (if c c p)))))
                      (prn-sym r)))))))
    ((ch? o) (if (eql o #\Space)
                 "\\Space"
                 (drop 1 (format nil "~@c" o))))
    ((functionp o)
     (conc "[:Fn name=" (prn (function-name o))
           " adr=" (if-bind x (object-address o) (format nil "#x~x" x) "no")
           "]"))
    ((op? o)     (op-match o))
    ((kleene? o) (prn (st (kleene $(kleene-value o)))))
    ((num? o)    (if (numberp o)
                     (let* ((v (if (minusp o) (- o) o))
                            (x (if (floatp v)
                                 (remove-zeroes (format nil "~30$" v))
                                 (format nil "~a" v))))
                       ;; FIXME: convert exponent to "^" power
                       (if (minusp o) (setf x (format nil "~~~a" x)))
                       x)
                     (prn-sq o))) ; FIXME: get rid of this
    ((sdl::gfx-p o)
     (format nil "[:Gfx w=~a h=~a adr=~a]"
             (sdl::gfx-w o)
             (sdl::gfx-h o)
             (if-bind x (object-address o)
               (format nil "#x~x" x) "no")))
    (t "[:InvalidType]") ; FIXME: get rid of this
    ))

(defun pp (o) ; pretty-print
  (format t "~a~%" (sq-to-str (prn o))))

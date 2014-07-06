(in-package :symta)


;; fold bytes of specified encoding into string
(defun fold-enc (seq &key (encoding :utf-8))
  ;; use (list-character-encodings) to get available ones
  (let ((*default-character-encoding* encoding)
        (l (sq-to-list seq)))
    (unless (every (fn (x) (and (numberp x) (>= x 0) (< x 256))) l)
      (return-from fold-enc ""))
    (octets-to-string (coerce l '(vector (unsigned-byte 8))))))

;; unfold string into bytes of specified encoding
(defun unfold-enc (s &key (encoding :utf-8))
  (unless (sym? s) (return-from unfold-enc nil))
  (let ((*default-character-encoding* encoding)
        (s (sq-to-str s)))
    (coerce (string-to-octets s) 'vector)))


(defparameter  /base   nil)   ; beginning of input stream
(defparameter  /in     nil)   ; current position inside input
(defparameter  /src    nil)   ; source, from where input comes
(defparameter  /res    nil)   ; result
(defparameter  /row    nil)   ; current row
(defparameter  /col    nil)   ; current column
(defparameter  /off    nil)   ; current offset inside input
(defparameter  /last   nil)   ; last processed char

(defun /sym-head? (x)
  (and (or (alpha? x) (eql x #\_) (eql x #\?))
       (not (eq x #\ø))
       (not (eq x #\√))))

(defun /sym-body? (x)
  (or (/sym-head? x) (digit? x)))

(defun /num-body? (x)
  (or (digit? x) (eq x #\.)))

(defun /top-char () (lhd /in))
(defun /next-char ()
  (let ((x (/top-char)))
    (if (eq x #\newline)
        (progn (setf /col 0)
               (incf /row))
        (incf /col))
    (incf /off)
    (setf /last x)
    (setf /in (ltl /in))
    x))

(defun ws? (x) (case x ((#\space #\newline) t)))

(defun /skip-ws ()
  (let ((c (/top-char)))
    (cond ((ws? c) (/next-char) (/skip-ws))
          ((and (eq c #\/) (eq (2nd /in) #\/))
           (/next-char)
           (/next-char)
           (while (and (/top-char) (not (eq (/top-char) #\newline)))
             (/next-char))
           (/skip-ws))
          ((and (eq c #\/) (eq (2nd /in) #\*))
           (/next-char)
           (/next-char)
           (let ((o 1))
             (while (plusp o)
               (unless /in (/error "`/*`: missing `*/`"))
               (let ((a (/next-char))
                     (b (1st /in)))
                 (cond
                   ((and (eql a #\*) (eql b #\/)) (/next-char) (decf o))
                   ((and (eql a #\/) (eql b #\*)) (/next-char) (incf o))))))
           (/skip-ws)))))

(defun /error (msg &rest args)
  (error "~a:~a,~a: ~a" /src /row /col (apply #'format nil msg args)))

(defun /take-prefix (p)
  (rec r ((ys nil))
    (let ((y (/top-char)))
      (if (funcall p y)
          (r (cons (/next-char) ys))
          (coerce (nreverse ys) 'simple-vector)))))

(defun /location (match)
  (let ((l (len match)))
    (vector (vector "Col" (- /col l))
            (vector "Off" (- /off l))
            (vector "Row" /row)
            (vector "Src" /src)
            )))

(defun /prn-src (src)
  (unless src (return-from /prn-src ""))
  (format nil "~a:~a,~a:"
              (ns-get src "Src")
              (ns-get src "Row")
              (ns-get src "Col")))

(defun /expect (c)
  (if (eq (/top-char) c)
      (/next-char)
      (/error "Expected `~a`; Got `~a`" c (or (/top-char) "EOF"))))

(defun /rev (xs)
  (list-to-sq (nreverse xs)))

(defmacro try (var expr fail &body body)
  `(let ((,var ,expr))
     (if (eq ,var :fail)
         ,fail
         (progn ,@body))))

(defparameter /ops
  (sort (mapcar #'str-to-sq
          '(":" "+" "-" "*" "/" "%" "^" "." "," "'"
            "~" "$" "!" "@" "~@" "~@!" "\\" "&"
            "≥≤" "≤≥" "≤" "≥" "≤≤" "≥≥"
            "=" "|" "::" ";"))
        #'> :key #'len))

(defun /op-hlp (ops)
  (/skip-ws)
  (when (or (alpha? (/top-char)) (digit? (/top-char)))
    (return-from /op-hlp :fail))
  (let* ((op (find-if (fn (x) (equ x (take (len x) /in)))
                      /ops))
         (op (if op (find-if (fn (x) (equ x op)) ops))))
    (unless op (return-from /op-hlp :fail))
    (dotimes (i (len op)) (/next-char))
    (new-sym (/location op) op)))

(defmacro /op (&rest ops)
  `(/op-hlp ',(mapcar #'str-to-sq ops)))

(defun interp (l e r) ; interpolate expression into string
  (setf l (if (str-empty? l)
              (st (_sconc $e))
              (st (_sconc $l $e))))
  (unless (str-empty? r) 
    (setf l (if (equ (1st r) "_sconc")
                (conc l (ltl r))
                (suf r l))))
  l)

(defun /read-string (ic d s e)
  (let ((l nil))
    (while t
      (let ((c (/next-char)))
        (cond
          ((eq c #\\)
           (setf c (/next-char))
           (cond
             ((eq c #\n) (setf l (suf #\newline l)))
             ((eq c #\t) (setf l (suf #\tab l)))
             ((eq c #\\) (setf l (suf #\\ l)))
             ((or (eq c #\n) (eq c ic) (eq c e) (eq c s))
              (setf l (suf c l)))
             ((eq c nil) (/error "EOF in string"))
             (t (/error "Invalid escape code: ~a" c))))
          ((eq c s) (incf d) (setf l (suf c l)))
          ((eq c e)
           (decf d)
           (if (< d 0)
               (return-from /read-string (st (|\\| $(sq-to-str l))))
               (setf l (suf c l))))
          ((eq c ic) ;interpolate
           (try it (/term) (/error "`$`: missing argument")
             (let ((r (/read-string ic d s e)))
               (return-from /read-string
                 (interp (st (|\\| $(sq-to-str l))) it r)))))
          ((eq c nil) (/error "EOF in string"))
          (t (setf l (suf c l))))))))

(defun /string ()
  (case (/top-char)
    ;;(#\" (/next-char) (/read-string #\$ 0 nil #\"))
    (#\“ (/next-char) (/read-string #\$ 0 #\“ #\”))
    (#\` (2nd (/read-string nil 0 nil (/next-char))))
    (otherwise :fail)))

(defun /list ()
  (let* ((ch (/top-char))
         (op (find-if (fn (x) (eq (char x 0) ch))
                      '("()" "{}" "[]" "<>" "«»")))
         (end (if op (aref op 1)))
         (row nil)
         (col nil)
         (hd nil))
    (unless op (return-from /list (/string)))
    (setf row /row)
    (setf col /col)
    (/expect ch)
    (when (eq (/top-char) #\:)
      (/expect #\:)
      (setf op (concatenate 'string (subseq op 0 1)  ":" (subseq op 1 2))))
    (when (not (string= op "()"))
      (when (string= op "«»") (setf op "\\"))
      (setf hd (new-sym (/location nil) (copy-seq op))))
    (rec r ((xs nil))
      (try x (/expr) (progn (if (eq (/top-char) end)
                                (/next-char)
                                (error "~a:~a,~a: unclosed `~a`" /src row col ch))
                            (if hd (lst hd (/rev xs)) (/rev xs)))
        (r (cons x xs))))))

(defun /line () ; reads single input line
  (rec r ((xs nil))
    (try x (/expr) (progn (if-bind it (/top-char)
                            (/error "Unexpected `~a`" it))
                          (/rev xs))
      (push x xs)
      (/skip-ws)
      (if (and (eq /last #\newline) (not (eq (/top-char) #\=)))
          (/rev xs)
          (r xs)))))


(defun hex-digit? (x)
  (or (digit? x) (alpha? x)))

(defun /num ()
  (cond ((digit? (/top-char))
         (rec r ((i t) (ys nil))
           (let ((y (/top-char)))
             (cond
               ((and (eql y #\.) i (digit? (ind /in 1))) (r nil (cons (/next-char) ys)))
               ((digit? y) (r i (cons (/next-char) ys)))
               (t (read-from-string (coerce (nreverse ys) 'string)))))))
        ((eq (/top-char) #\ø) (/next-char) (return-from /num nil))
        ((eq (/top-char) #\√) (/next-char) (return-from /num t))
        ((eq (/top-char) #\#)
         (/expect #\#)
         #|(when (eq (/top-char) #\y)
           (/next-char)
           (return-from /num t))
         (when (eq (/top-char) #\n)
           (/next-char)
           (return-from /num nil))|#
         (when (hex-digit? (/top-char))
           (return-from /num
             (read-from-string
              (concatenate 'string
                "#x" (coerce (sq-to-list (/take-prefix #'hex-digit?)) 'string)))))
         (when (eq (/top-char) #\{)
           (return-from /num
             (lst "#" (2nd (/list)))))
         (/error "`#`: invalid unexpected `~a`" (/top-char)))
        (t :fail)))

(defun /delim ()
  (try d (/op "=" "|" "::" ";") (/list)
    d))

(defun /term ()
  (/skip-ws)
  (let ((x (/top-char)))
    (if (/sym-head? x)
        (let ((m (/take-prefix #'/sym-body?)))
          (new-sym (/location m) m))
        (try n (/num) (/delim)
          (when (/sym-head? (/top-char))
            (setf n (st (* $n $(/dot)))))
          n))))

(defmacro /lop (name down &rest ops) ; left-biased op
  `(defun ,name ()
     (try a (,down) :fail
       (rec r ((e a))
         (try o (/op ,@ops) e
           (try b (,down) (/error "`~a`: missing right operand" o)
             (r (vector o e b))))))))

(defmacro /rop (name down &rest ops) ; right-biased op
  `(defun ,name ()
     (try a (,down) :fail
       (try o (/op ,@ops) a
         (try b (,name) (/error "`~a`: missing right operand" o)
           (vector o a b))))))

(defmacro /prop (name down &rest ops) ; prefix op
  `(defun ,name ()
     (try o (/op ,@ops) (,down)
       (try a (,name) (/error "`~a`: missing operand" o)
         (if (and (numberp a) (equal o "~")) ;dangerous hack
             (- a)
             (vector o a))))))

(/prop /gs   /term "&")
(/lop  /dot  /gs   "." "," "'")
(/prop /pref /dot  "~" "\\" "$" "!" "@" "~@" "~@!")
(/lop  /pow  /pref "^")
(/lop  /mul  /pow  "*" "/" "%")
(/lop  /add  /mul  "+" "-")
(/lop  /rel  /add  "≥≤" "≤≥" "≤" "≥" "≤≤" "≥≥")
(/rop  /bnd  /rel  ":")


(defun /expr () 
  (/bnd))

(defun /read (xs)
  (let* ((/src "<REPL>")
         (/base (str-to-sq xs))
         (/in /base)
         (/off 0)
         (/row 0)
         (/col 0)
         (/last nil))
    (/line)))

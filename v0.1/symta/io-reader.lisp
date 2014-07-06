(in-package :symta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  OLD BOILERPLATE FOR LEXER
;;  SHOULD BE REWRITTEN ASAP!!!
;;

(defun fs (&key (val nil) (ns nil))
  (set-type (lst val ns) (st |Fs|)))

(defun fs-val (fs) (1st fs))
(defun fs-ns (fs) (2nd fs))

(defun fs-set (root path val)
  ($case (get-type root)
    (|Fs|
      (unless path (return-from fs-set (fs :val val :ns (fs-ns root))))
      (let ((name (lhd path)) 
            (rest (ltl path)))
        (when (kleene? name) ; hack to implement kleen-star on trees
          (setf root (fs-set root rest val))
          (setf rest (pre (kleene-value name) rest))
          (setf name :**))
        (let ((ns (fs-ns root)))
          (when (functionp name)
            (return-from fs-set
              (fs :val (fs-val root)
                  :ns (ns-set ns :// (pre (lst name (fs-set nil rest val))
                                          (ns-get ns ://))))))
          (let* ((next (or (ns-get ns name) (fs))))
            ;; assigning <> deletes file
            (fs :val (fs-val root)
                :ns (ns-set ns name (fs-set next rest val)))))))
    (otherwise (fs-set (fs) path val))))

(defun fs-star (p root path match-len)
  (or (and path (equ (lhd path) p)
           (let ((m (fs-star p root (ltl path) (+ match-len 1))))
             (if (1st m) m)))
      (fs-match root path match-len)))

;; longest match backtracer
(defun fs-try (ns n path len kleene)
  (let ((l nil)
        (m (lst nil 0)))
    (if-bind it (ns-get ns n) (push (cons n it) l))
    (if-bind it (ns-get ns ://)
      ($map (fn (x)
              (let ((fun (1st x)))
                (when (equ n fun) (push (cons fun (2nd x)) l))))
            it))
    (mapc (fn (x)
            (let ((nm (if kleene
                          (fs-star (car x) (cdr x) path len)
                          (fs-match (cdr x) path len))))
              (if (and (1st nm) (>= (2nd nm) (2nd m)))
                  (setf m nm))))
          l)
    m))

;; we use our Fs as a regexp-pattern tree
(defun fs-match (root path &optional (match-len 0))
  ($case (get-type root)
    (|Fs|
      (unless path (return-from fs-match (lst (fs-val root) match-len)))
      (let ((name (lhd path))
            (rest (ltl path))
            (ns   (fs-ns root))
            (m    nil)
            (sm   nil))
        (setf m (fs-try ns name rest (+ match-len 1) nil))
        (setf sm (if-bind it (ns-get ns :**)
                   (fs-try (fs-ns it) name path match-len t)
                   (lst nil 0)))
        (if (1st m)
            (if (>= (2nd m) (2nd sm)) m sm)
            (if (1st sm) sm (lst (fs-val root) match-len)))))
    (otherwise (lst nil 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           READER
;;

(defun rd-error (seq msg &rest args)
  (let* ((r (take (- (len *read-input*) (len seq)) *read-input*))
         (r (split #\Newline r)))
    (error "~a,~a: ~a" (len r) (len (rhd r)) (apply #'format nil msg args))))

(defstruct op match name binds finalizer loc args cbs)
(defun op? (o) (typep o 'op))
(defun op-type? (o v) (and (op? o) (equ (op-match o) v)))

(defun cng-args (o v)
  (let ((o (copy-op o)))
    (setf (op-args o) v)
    o))

(defun cng-loc (o v)
  (let ((o (copy-op o)))
    (setf (op-loc o) v)
    o))

(defun cng-cbs (o v)
  (let ((o (copy-op o)))
    (setf (op-cbs o) v)
    o))

(defun set-reader-macro (path macro)
  (setf *reader-macros* (fs-set *reader-macros* path macro)))

(defun get-reader-macro (path)
  (fs-match *reader-macros* path))

(defmacro def-reader-macro (cases &body body)
  (unless (consp cases) (setf cases (list cases)))
  `(progn
     ,@(mapcar (fn (x)
                 (if (stringp x) (setf x (format nil x)))
                 `(set-reader-macro ,x (fn (match seq stack) ,@body)))
               cases)))

(defun read-expr (seq stack)
  (unless seq (return-from read-expr nil))
  (let* ((match     (get-reader-macro seq))
         (macro     (1st match))
         (match-len (2nd match)))
    (funcall macro (take match-len seq) (drop match-len seq) stack)))

(defun read-toplevel (seq &key (single nil))
  (unless (plusp (len seq)) (return-from read-toplevel nil))
  (let ((r  nil)
        (l  nil)
        (nl (format nil "~%"))
        (def-flag nil))
    (while-bind it (read-expr seq l)
      (bind (new-seq new-l) it
        (cond
          ((op-type? (rhd new-l) nl)
           (when (and (> (len l) 0)
                      (not (case (lhd new-seq) ((#\Space #\Newline) t))))
             (setf r (if def-flag
                         (suf l r)
                         (conc r (split (op-of-type ";") l))))
             (if single (return-from read-toplevel (cons r new-seq)))
             (setf l nil)
             (setf def-flag nil)))
          (t (if (op-type? (rhd new-l) "->")
                 (setf def-flag t))
             (setf l new-l)))
        (setf seq new-seq)))
    (setf r (if def-flag
                (suf l r)
                (conc r (split (op-of-type ";") l))))
    (if single (return-from read-toplevel (cons r nil)))
    r))

(defun $read (seq)
  (let ((*read-input* seq))
    ($map #'parse (read-toplevel *read-input*))))

(def-reader-macro nil ; root (default case)
  (rd-error seq "Unexpected character: `~a`" (lhd seq)))

(def-reader-macro ((lst #'sym-hd (kleene #'sym-ch)))
  (list seq (suf ($intern match) stack)))

(def-reader-macro ((lst #'digit (kleene #'digit)))
  (setf match (sq-to-list match))
  (setf stack (suf (read-from-string (coerce match 'string)) stack))
  (if (symbol-char? (lhd seq))
      (read-operator "*" seq stack)
      (list seq stack)))

(defun hexdigit (k x)
  (funcall k (or (digit? x) (case x ((#\a #\b #\c #\d #\e #\f
                                      #\A #\B #\C #\D #\E #\F) t)))))

(def-reader-macro ((lst #\# #\x #'hexdigit (kleene #'hexdigit)))
  (list seq (suf (read-from-string (sq-to-str match)) stack)))

(def-reader-macro ((lst #\\ (fn-k (x) (not (eql x #\Newline)))
                        (kleene #'sym-ch)))
  (setf match (pre #\# match))
  (list seq (suf (read-from-string (sq-to-str match)) stack)))

(def-reader-macro "//"
  (setf seq (if-bind it (pos #\Newline seq) (drop it seq)))
  (list seq stack))

(def-reader-macro "/*"
  ;; FIXME: save comments as metadata
  (let ((opened (list seq)))
    (while opened
      (unless seq (rd-error (car opened) "missing `*/`" ))
      (let ((a (1st seq))
            (b (2nd seq)))
        (cond
          ((and (eql a #\*) (eql b #\/))
           (pop opened)
           (setf seq (drop 2 seq)))
          ((and (eql a #\/) (eql b #\*))
           (push seq opened)
           (setf seq (drop 2 seq)))
          (t (setf seq (ltl seq)))))))
  (list seq stack))

(defun read-list (seq stack end)
  (let ((beg seq)
        (l   nil))
    (while-bind it (read-expr seq l)
      (bind (new-seq new-l) it
        (when (op-type? (rhd new-l) end)
          (return-from read-list (list new-seq (suf l stack))))
        (setf seq new-seq)
        (setf l new-l)))
    (rd-error beg "Missing `~a`" (sq-to-str end))))

(def-reader-macro "("
  (read-list seq stack ")"))

(def-reader-macro "["
  (bind (seq new-s) (read-list seq stack "]")
    (list seq (suf (st (lst $$(rhd new-s))) stack))))


(defun op-of-type (type) (fn-k (op) (op-type? op type)))


(defun read-blk (seq stack &key name)
  (bind (seq new-s) (read-list seq stack "}")
    (let* ((xs (rhd new-s))
           (xs (if name (pre name xs) xs))
           (xs (split (op-of-type ";") xs))
           (xs (pre (st |namedBlk|) xs)))
      (list seq (suf xs stack)))))

(def-reader-macro "{"  (read-blk seq stack :name (st r)))
(def-reader-macro "{:" (read-blk seq stack))


;;(defun read-double-list (seq stack name)
;; )
;;(def-reader-macro "%{" (read-blk seq stack nil))

;; interpolate expression into string
(defun interp (l e r how)
  (st (_slst $$(if (> (len l) 0) (lst l) nil)
             $(if (eql how #\$) e (st (|asStr| $e)))
             $$(if (> (len r) 0) (lst r) nil))))

(defun read-string (seq stack eof-ends endp &key (depth 0) (subst nil))
  (let ((beg seq)
        (l   nil))
    (while t
      (let ((c (lhd seq)))
        (setf seq (ltl seq))
        (if (funcall endp c)
            (return-from read-string (list seq (suf (sq-to-str l) stack))))
        (cond
          ((eql c #\\)
           (setf c   (lhd seq))
           (setf seq (ltl seq))
           (case c
             (#\Newline )
             (#\n (setf l (suf #\Newline l)))
             (#\$ (setf l (suf #\$ l)))
             (#\@ (setf l (suf #\@ l)))
             (t (setf l (suf c l)))))
          ((and subst (or (eql c #\$) (eql c #\@)))
           (if-bind it (read-expr seq nil)
             (bind (new-seq e) it
               (bind (new-seq r) (read-string new-seq nil eof-ends endp
                                              :depth (+ depth 1) :subst t)
                 (return-from read-string
                   (list new-seq
                         (suf (interp (sq-to-str l) (lhd e) (lhd r) c)
                              stack)))))
             (if eof-ends
                 (return-from read-string
                   (list seq (suf (sq-to-str l) stack))))
             (rd-error beg "EOF in string")))
          (t (setf l (suf c l))))
        (unless seq
          (if eof-ends
              (return-from read-string (list seq (suf (sq-to-str l) stack))))
          (rd-error beg "EOF in string"))))))

(def-reader-macro "\""
  (read-string seq stack nil (fn (x) (eql #\" x)) :subst t))

(def-reader-macro "\`"
  (bind (seq stack) (read-string seq stack nil (fn (x) (eql #\` x)) :subst nil)
    (let ((hd (rhd stack))   (tl (rtl stack)))
      (list seq (suf ($intern hd) tl)))))

(defun read-operator (op seq stack)
  (list seq (suf (cng-loc (ns-get *operators* (sq-to-str op)) seq) stack)))

(defun merge-binds (bs)
  (if bs
      (let* ((x  (car bs))
             (bs (cdr bs))
             (y  (car bs)))
        (if (eql (car x) (car y))
            (cons (append (list (car x)) (cdr x) (cdr y))
                  (merge-binds (cdr bs)))
            (cons x (merge-binds bs))))))

(defun order-binds (bs)
  (merge-binds (sort bs (fn (x y) (< (car x) (car y))))))


(defun process-opts (rs os)
  (unless os (return-from process-opts rs))
  (let* ((o (car os))
         (h (first o))
         (p (second o)))
    (process-opts
     (append (case h
               (:lr `((,p :l) (,p :r)))
               (:rl `((,p :l) (,(- p 1) :r)))
               (otherwise `((,p ,h))))
             rs)
     (cdr os))))


(defun def-operator (match &rest opts)
  (set-reader-macro match #'read-operator)
  (let* ((binds (process-opts nil opts))
         (fin (second (assoc 0 binds)))
         (binds (remove-if (fn (x) (eql (first x) 0)) binds)))
    (setf *operators*
          (ns-set *operators* match
                  (make-op :match match
                           :name ($intern match)
                           :finalizer fin
                           :binds (order-binds binds))))))

(defun fold-rec (xs s)
  (unless xs (return-from fold-rec s))
  (let* ((x  (lhd xs))
         (xs (ltl xs)))
    ;; doesnt need folding?
    (unless (op? x) (return-from fold-rec (fold-rec xs (suf x s))))
    (let* ((bs (op-cbs x))
           (x  (cng-cbs x (cdr bs)))
           (b  (car bs))
           (v  'none))
      (case b
        (:l (if s (setf v (rhd s)
                        s (rtl s))))
        (:r (when xs
                  (setf v (lhd xs))
                  (if (and (op? v) (op-cbs v))
                      (let ((r (fold-rec xs nil)))
                        (setf v (lhd r))
                        (setf xs (ltl r)))
                      (setf xs (ltl xs)))))
        (:del (return-from fold-rec (fold-rec xs s)))
        (otherwise (return-from fold-rec (fold-rec xs (suf x s)))))
      (when (eql v 'none)
        (let ((n (if (eql b :l) "left" "right")))
          (rd-error (op-loc x) "`~a` has no ~a operand" (op-match x) n)))
      (setf x (cng-args x `((,b ,v) ,@(op-args x))))
      (fold-rec (pre x xs) s))))

(defun fold-cbs (e)
  (cond
    ((op? e) (cng-args e (mapcar (fn (x)
                                   (list (first x) (fold-cbs (second x))))
                                 (op-args e))))
    ((fin? e) e)
    (t (fold-rec ($map #'fold-cbs e) nil))))

(defun turn-cbs (e o)
  (cond
    ((op? e)
     (setf e (cng-args e (mapcar (fn (x)
                                   (list (first x) (turn-cbs (second x) o)))
                                 (op-args e))))
     (if-bind it (rest (assoc o (op-binds e)))
       (cng-cbs e it)
       e))
    ((fin? e) e)
    (t ($map (fn (x) (turn-cbs x o)) e))))

(defun normalize-op-parens (e)
  (unless (and (lst? e) (= (len e) 1)) (return-from normalize-op-parens e))
  (let ((e (normalize-op-parens (1st e))))
    (if (op? e) e (lst e))))

(defun finalize-parse (e &key (under-op nil))
  (cond ;; we do finalization bottom-up
    ((op? e) (let* ((name (op-name e))
                    (args (op-args e))
                    (uo   (and (assoc :l args) (assoc :r args)))
                    (args (mapcar (fn (x)
                                    (list (first x)
                                          (finalize-parse (second x)
                                                          :under-op uo)))
                                 args))
                    (l    (assoc :l args))
                    (r    (assoc :r args))
                    (f    (op-finalizer e))
                    (out  (conc (lst name)
                                (if l (lst (second l)))
                                (if r (lst (second r))))))
               (mapcar (fn (x)
                         (mapcar (fn (x)
                                   (unless (assoc x args)
                                     (rd-error
                                      (op-loc e)
                                      "`~a` caused binding power conflict"
                                      (op-match e))))
                                 (cdr x)))
                       (op-binds e))
               (if f (funcall f #'identity out e) out)))
    ((fin? e) e)
    (t (if under-op
           (let ((e (normalize-op-parens e)))
             (if (lst? e)
                 ($map #'finalize-parse e)
                 (finalize-parse e)))
           ($map #'finalize-parse e)))))

(defun used-orders-rec (e)
  (cond ((op? e) (mapcar #'first (op-binds e)))
        ((lst? e) (reduce #'append (mapcar #'used-orders-rec
                                           (sq-to-list e))))))

(defun used-orders (e)
  (remove-if #'zerop (remove-duplicates (sort (used-orders-rec e) #'>))))

(defun parse (e)
  (let ((orders (used-orders e)))
    (dolist (o orders)
      (setf e (fold-cbs (turn-cbs e o))))
    (finalize-parse e)))

(defun def-operators (ops)
  (setf *operators* nil)
  (mapc (fn (x) (apply #'def-operator x)) ops))

(defun dangling-op (k e o)
  (declare (ignorable k) (ignorable e) (ignorable o))
  (rd-error (op-loc o) "unexpected `~a`" (op-match o)))

(defun neg-imm (k e o)
  (declare (ignorable k) (ignorable e) (ignorable o))
  (funcall k (if (numberp (2nd e)) (- (2nd e)) e)))

(defun fin-short-fn (k e o)
  (declare (ignorable k) (ignorable e) (ignorable o))
  (let ((a (2nd e))
        (b (3rd e)))
    (st (fn ($a) (do $b)))))

(defun fin-msg (k e o)
  (declare (ignorable k) (ignorable e) (ignorable o))
  (let ((a (2nd e))
        (b (3rd e)))
    (funcall k (if (and (integerp a) (integerp b) (>= b 0))
                   (read-from-string (format nil "~a.~a" a b))
                   e))))

(defmacro def-fin (name &body body)
  `(defun ,name (k e o)
     (declare (ignorable k) (ignorable e) (ignorable o))
     (let ((a (2nd e))
           (b (3rd e)))
       (declare (ignorable a) (ignorable b))
       ,@body)))


(defun fin-const (c) (fn (k e o) (funcall k c)))

;; FIXME: use relative precedence
(progn
  (def-operators `(
    (,(format nil "~%")   (:del 99)) ; newline
    (" "                  (:del 99)) ; space
    (,(format nil "\\~%") (:del 99)) ; escaped newline

    ;; dangling syntax elements
    (")"  (,#'dangling-op 0))
    ("}"  (,#'dangling-op 0))
    ("]"  (,#'dangling-op 0))

    ;; curry args
    ("?"   (,(fin-const (st ?  )) 0))
    ("??"  (,(fin-const (st ?? )) 0))
    ("???" (,(fin-const (st ???)) 0))

    ("->" (,(fin-const (st |->|)) 0))
    ("|"  (,(fin-const (st |\||)) 0))
    ("|>" (,(fin-const (st |\|>|)) 0))
    (";"  (,(fin-const (st |;|)) 0))
    ("&&" (,(fin-const (st |&&|)) 0))
    ("||" (,(fin-const (st |\|\||)) 0))
    ("::" (,(fin-const (st |::|)) 0))

    ("y"  (,(fin-const t  ) 0))
    ("n"  (,(fin-const nil) 0))

    ("&"  (:r  86))  ; auto gensym for quasiquote

    ("**" (:rl 84)) ; function composition

    ("."  (:lr 80) (,#'fin-msg 0)) ; postfix-style funcall

    (","  (:lr 80)) ; array index

    ;; these should all have same precedence
    ("~"  (:r 75) (,#'neg-imm 0))
    ("'"  (:r 75))  ; quote
    ("$"  (:r 75))  ; unquote
    ("@"  (:r 75))  ; incut
    ("!"  (:r 75))  ; revise (treat operand differently)
    ("#"  (:r 75))  ; quasiquote
    ("?:" (:r 75))  ; solve for arg (sugar for bruters)

    ("^"  (:lr 65))   ; power
    ("*"  (:lr 60))   ; mul
    ("/"  (:lr 60))   ; div
    ("%"  (:lr 60))   ; mod
    ("%%" (:lr 60))   ; truncating div
    (">>" (:lr 60))   ; bit-shift left
    ("<<" (:lr 60))   ; bit-shift right
    ("+"  (:lr 55))   ; add
    ("-"  (:lr 55))   ; sub

    (".." (:lr 50)) ; range
    ("++" (:lr 50)) ; replication

    ("==" (:lr 40)) ; equal
    ("!=" (:lr 40)) ; not equal
    ("<"  (:lr 40))
    (">"  (:lr 40))
    ("<=" (:lr 40))
    (">=" (:lr 40))

    (":"  (:rl 30)) ; bind

    ("~>" (:rl 20) (,#'fin-short-fn 0)) ; shorthand for fn

    ("=:" (:rl 16)) ; set

    ("="  (:lr 14))
    ))
  )


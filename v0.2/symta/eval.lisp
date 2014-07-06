(in-package :symta)

(defparameter *src-depth* 0)     ; current macroexpansion depth
(defparameter *src-limit* 1000)  ; macroexpansion depth limit
(defparameter *src-source* nil)

(defparameter *eval-types* nil)


(defparameter *symtbl* (make-hash-table :test 'equal))
(defparameter *symcount* 1)
(defun symnum (x)
  (if-bind n (gethash x *symtbl*)
    n
    (setf (gethash x *symtbl*) (incf *symcount*))))


(defmacro with-decls (xs body)
  (setf xs
        (mapcar (fn (x)
                  (let ((type (first x)))
                    (if (arrayp type) (setf type (coerce type 'list)))
                    `(,(first x) ,(intern (second x) (third x)))))
                xs))
  `(locally (declare ,@xs) ,body))



(defun lambda? (x) (and (consp x) (eql (car x) 'lambda)))
(defun cl-fn? (x) (or (fn-sym? x) (lambda? x)))




(defun expand-pipe (es prev)
  (unless es (return-from expand-pipe prev))
  (let* ((u (unisym))
         (e (lhd es))
         (b (lhd e)))
    (when (bnd? b)
      (setf e (st ($(2nd b) (|<>| ($(3rd b) = $$(ltl e)))))))
    (st (_let (($u $(if prev
                        (st ($$e $prev))
                        e)))
          $(expand-pipe (ltl es) u)))))

(defun process-pipe (e)
  (when (quote? e) (return-from process-pipe e))
  (let* ((es ($split (st |\||) e))
         (r (if (> (len es) 1)
                (expand-pipe es nil)
                (1st es))))
    r))

(defun process-bpipe (e)
  (when (quote? e) (return-from process-bpipe e))
  (if-bind p (pos (st |::|) e)
    (let ((l (take p e))
          (r (drop (+ p 1) e)))
      (st ($$l $(process-bpipe r))))
    e))
(defun normalize-keywords (as ks xs)
  (unless xs (return-from normalize-keywords (conc as ks)))
  (let ((x  (lhd xs))
        (xs (ltl xs)))
    (if (bnd? x)
        (setf ks (conc ks (lst (to-kw (2nd x)) (3rd x))))
        (setf as (suf x as)))
    (normalize-keywords as ks xs)))

(defun currable? (s)
  (not (or (equ s "=")
           (equ s "_fn")
           (equ s "_if")
           (equ s "<>")
           (equ s "<:>")
           ;;(equ s "<clean>")
           (equ s "_assign")
           (equ s "\\"))))

(defun find-curries (bs es l)
  (when (fin? es) (return-from find-curries es))
  (let* ((s (lhd es))
         (s (or l
                (and (or (and (sym? s) (plusp (length s)))
                     (and (lst? s) (= (len es) 1)))
                 (or (not (sym? s))
                     (and (not (alpha? (aref s 0)))
                          (currable? s)))))))
    ($map (fn (e)
            (let ((n ($case e (|?| 0) (|??| 1) (|???| 2))))
              (cond (n (let ((x (nth n bs)))
                         (unless x
                           (dotimes (i (+ n 1))
                             (setf x (nth i bs))
                             (unless x
                               (setf x (unisym))
                               (setf (nth i bs) x))))
                         x))
                    (s (find-curries bs e (equ (lhd es) "[]")))
                    (t e))))
          es)))

(defun process-curries (expr &key (finish #'identity))
  (if (fin? expr) (return-from process-curries (funcall finish expr)))
  (let* ((as (list nil nil nil))
         (e  (find-curries as expr nil)))
    (unless (car as) (return-from process-curries (funcall finish expr)))
    (setf as (list-to-sq (remove-if #'null as)))
    (st (_fn $as $(funcall finish e)))))



(defun process-arrow (e)
  (if-bind a (and (not (quote? e))
                  (not (esc? e))
                  (has-arrow? e))
    (let* ((d (meta-set (meta-get a)
                        (copy-seq "_set"))))
      (setf e (st ($d $e)))))
  e)


(defun expand-syms (es)
  ($map (fn (e)
          (if-bind x (and (sym? e)
                          (not (fn-sym? e))
                          (macro? e))
            (funcall x nil)
            e))
        es))



(defun pse-path (e)
  (if (fin? e)
      (cons nil e)
      (let ((u (unisym))
            (o (1st e))
            (v (3rd e))
            (p (pse-path (2nd e))))
        (when (and (fn-sym? v) (equal o (st |.|)))
          (setf v (st (|\\| $v))))
        (cons (st (($u $v) $$(car p)))
              (st ($o $(cdr p) (do $u)))))))

(defun process-side-effects (e)
  (unless (or (fin? e) (equ (lhd e) "="))
    (let ((p (pos #'unesc? e)))
      (when p
        (let ((x (2nd (ind e p)))
              (post nil))
          (when (unesc? x)
            (setf x (2nd x))
            (setf post t))
          (setf x (pse-path x))
          (setf e (cng e p (cdr x)))
          (setf e (if post
                      (let ((u (unisym)))
                        (st (_let (($u $(cdr x)))
                              (do (|_assign| $(cdr x) $e)
                                  $u))))
                      (st (|_assign| $(cdr x) $e))))
          (if (car x) (setf e (st (_let $(car x) $e))))))))
  e)

(defun expand-prepare (e)
  (expand-syms
   (process-pipe
    (process-bpipe
     (process-side-effects
      (process-curries
       (process-arrow e)))))))

(defun apply-macro (s macro args)
  (let ((*src-source* (or (ns-get (meta-get s) "Src")  *src-source*)))
    (funcall macro args)))

(defun list-sexp? (x) (and (consp x) (eq (car x) 'list)))

(defun st-apply-hlp (xs)
  (unless xs (return-from st-apply-hlp nil))
  (let ((x (car xs))
        (xs (cdr xs)))
    (while (and (list-sexp? x) (list-sexp? (car xs)))
      (setf x `(list ,@(cdr x) ,@(cdr (car xs))))
      (setf xs (cdr xs)))
    (cons x (st-apply-hlp xs))))

(defmacro st-apply (f &rest xs)
  (setf xs (mapcar (fn (x)
                     (if (and (consp x) (eq (car x) '|st|::|glb-@|))
                         (let ((x (second x)))
                           (if (and (consp x) (eq (car x) 'cl-lst))
                               `(list ,@(cdr x))
                               `(sq-to-list ,x)))
                         `(list ,x)))
                   xs))
  (setf xs (st-apply-hlp xs))
  (setf xs (if (> (length xs) 1)
               `(concatenate 'list ,@xs)
               (car xs)))
  `(apply ,f ,xs))

;; macro unfolder
(defun expand-normal (expr)
  (let ((b (lhd expr)))
    (when (and (bnd? b) (unesc? (2nd b)))
      (setf expr (st (_assign $(2nd (2nd b)) ($(3rd b) $$(ltl expr)))))))
  (setf expr (expand-prepare expr))
  (let ((x  (lhd expr))
        (xs (ltl expr)))
    (if-bind it (macro? x)
      (expand (apply-macro x it xs))
      (progn
        (when (lst? x) (setf x (expand-prepare x)))
        (while-bind it (and (lst? x) (fn-sym? x) (macro? (1st x)))
          (setf x (apply-macro x it (ltl x))))
        (when (lst? x) (setf x (expand x)))
        (setf xs ($map #'expand (normalize-keywords nil nil xs)))
        (setf expr
              (if (any #'incut? xs)
                  (expand-normal
                   (st (cl (st-apply ($"@" $x)
                                     $$($map (fn (x) (lst "@" x)) xs)))))
                  (pre x xs)))
        expr
        ))))


(defmacro kw-unbound? (a) `(eq ,a :unbound))
(defmacro kw-unbound () :unbound)
(defun defaulted-kw (k d) (st (_if (cl (kw-unbound? $k)) $d $k)))

(defun expand-kws (ks ds body)
  (unless ks (return-from expand-kws body))
  (let ((k (lhd ks))  (ks (ltl ks))
        (d (lhd ds))  (ds (ltl ds)))
    (st ((_fn ($k) $(expand-kws ks ds body)) $d))))
(defun compile-to-cl-kw (kw) `(,(intern kw *pkg*) (kw-unbound)))



(defun expand (expr)
  (unless (lst? expr)
    (return-from expand expr))
  (assert (< *src-depth* *src-limit*) nil "macroexpansion depth limit exceed")
  (let ((*src-depth* (+ *src-depth* 1)))
    ($list-case expr
      (_q (x) expr) ; dont expand quoted stuff
      (set_l (p v) (st (set_l $p $(expand v))))
      (cl_esc (x) expr)
      (_fn (args body)
        (let ((hd (lhd args)))
          (if (lst? hd)
             (let* ((ks   ($map (fn (x) (1st x)) hd))
                    (ks2  ($map #'1st hd)) ; keywords
                    (ds   ($map #'2nd hd)) ; default values
                    (as   (conc ks (ltl args)))
                    (args (pre ks (ltl args)))
                    (*env*(env-push as *env*))
                    (ds   ($map #'defaulted-kw ks ds))
                    (body (expand-kws ks2 ds body)))
               (st (_fn $args $(expand body))))
             (let* ((*env* (env-push args *env*)))
               (st (_fn $args $(expand body)))))))
      (otherwise (expand-normal expr)))))

(defun imm? (e) (or (fin? e) (_fn? e) (esc? e)))
(defun literal? (e) (or (fin? e) (esc? e)))


(defmacro cl-lst (&rest xs)
  (if (< 1 (length xs) (lbp))
      `(vector ,@xs)
      `(list-to-sq (list ,@xs))))

(defmacro _dot (s c i &rest as)
  `(funcall (funcall ,c ,i ,s) ,@as))

(defun compile-to-cl-funcall (e)
  (let ((f  (car e))
        (as (cdr e)))
    (cond
      ((not as) f)
      ((lambda? f) e)
      ((and (consp f) (eq (first f) 'function) (fn-sym? (second f)))
       (case (second f)
         (|st|::|glb-[fn]| `(cl-lst ,@as))
         (|st|::|glb-_dot| `(_dot ,@as))
         (|st|::|glb-_pkg|
           (let* ((p (first as))
                  (s (second as))
                  (r (intern s (get-package p))))
             (if (fn-sym? s)
                 `#',r
                 r)))
         (|st|::|glb-c| `(funcall ,@as))
         (|st|::|glb-goto|
          (let ((a (first as)))
            (unless (sym? a) (error "goto: argument `~a` is not a symbol" a))
            `(go ,(intern a))))
         (|st|::|glb-label|
          (let ((a (first as)))
            (unless (sym? a) (error "goto: argument `~a` is not a symbol" a))
            (intern a)))
         (otherwise `(,(second f) ,@as)))
       )
      (t `(funcall ,f ,@as)))))

(defun compile-tagbody (xs)
  (let* ((v (gensym))
         (has-tags nil)
         (ys (mapcar (fn (x)
                       (let ((r (compile-to-cl x)))
                         (if (and (sym? x) (not (fn-sym? x)))
                             `(identity ,r)
                             (progn (when (and (symbolp r) r (not (eql r t)))
                                      (setf has-tags t))
                                    r))))
                     xs)))
    (if has-tags
        `(let ((,v))
           (tagbody ,@(butlast ys) (setf ,v ,@(last ys)))
           ,v)
        `(progn ,@ys))))

(defun compile-to-cl-form (expr)
  ($list-case expr
    (_fn (args body)
       (let* ((ks (1st args))
              (ks (if (lst? ks) ks))
              (as (if ks (st ($$ks $$(ltl args))) args))
              (args  (mapcar (fn (a) (intern a *pkg*))
                             (sq-to-list (if ks (ltl args) args))))
              (ks (if ks (cons '&key (mapcar #'compile-to-cl-kw
                                             (sq-to-list ks)))))
              (*env* (env-push as *env*)))
         `(lambda (,@args ,@ks) ,(compile-to-cl body))))
    (_if (&rest xs)
         (let ((args (mapcar #'compile-to-cl xs)))
           `(if ,(first args) ,(second args) ,(third args))))
    (_q (x) (if (or (stringp x) (numberp x))
                x
                `(quote ,x)))
    (set_l (p v)
      (let ((v (compile-to-cl v)))
        (if (env-sym? p)
            `(setq ,(intern p *pkg*) ,v)
            (let ((s (intern (local-to-global p) *pkg*)))
              `(locally (declare (special ,s))
                 ,(if (fn-sym? p)
                      `(setf (symbol-function ',s) ,v)
                      `(setq ,s ,v)))))))
    (cl_esc (c) (compile-to-cl-esc (2nd c)))
    (do (&rest as) (compile-tagbody as))
    (name_fn (name fn)
      `(named-fn ,(2nd name) ,@(cdr (compile-to-cl fn))))
    (otherwise
     (compile-to-cl-funcall (mapcar #'compile-to-cl (sq-to-list expr))))))

;; TODO: implement unbound symbols
(defun compile-to-cl-atom (e)
  (cond
    ((not (sym? e)) e)
    ((env-sym? e) (intern e *pkg*))
    ;; gensyms cant ref global values
    ((unisym? e) (error "Symbol references nothing: ~a" e))
    (t (let ((s (intern (local-to-global e) *pkg*)))
         (if (fn-sym? e)
             `#',s
             s)))))

(defun compile-to-cl (e)
  (if (lst? e)
      (compile-to-cl-form e)
      (compile-to-cl-atom e)))

(defun compile-to-cl-esc-atom (e)
  (cond ((eql e (st _q)) 'quote)
        ((sym? e) (if (env-sym? e)
                      (intern e *pkg*)
                      (intern (string-upcase e) "SYMTA")))
        (t e)))

(defun compile-to-cl-esc-form (e)
  ($list-case e
    (esc (x) x)
    (|@| (x) ($compile x))
    (|\\| (x) `',(compile-to-cl-esc x))
    (|.| (p s) (intern (string-upcase s) (string-upcase p)))
    (|:| (k v) `(,(intern (string-upcase k) "KEYWORD") ,(compile-to-cl-esc v)))
    (otherwise (let (ys ks (xs (mapcar #'compile-to-cl-esc (sq-to-list e))))
                 (loop as x in xs do (if (and (consp x) (keywordp (car x)))
                                         (setf ks `(,@x ,@ks))
                                         (push x ys)))
                 (when ks (setf xs `(,@(nreverse ys) ,@ks)))
                 xs))))
       

(defun compile-to-cl-esc (e)
  (if (fin? e)
      (compile-to-cl-esc-atom e)
      (compile-to-cl-esc-form e)))

(defun $compile (expr)
  (let ((e (expand expr)))
    (compile-to-cl e)))

(defun %eval (expr)
  (let ((c ($compile expr)))
    (eval `(locally
             #|(declare (optimize (compilation-speed 0)
                                (speed 3)
                                (space 0)
                                ;;(safety 0)
                                (debug 0)
                                ))|#
             (muffle-compiler-note
               (muffle-compiler-warning
                 ,c))))))

(defun $evalq (string)
  ($compile (/read string)))

(defun $eval (seq &key (silent nil) (src "<REPL>"))
  (if (stringp seq) (setf seq (str-to-sq seq)))
  (let* ((/src src)
         (/base seq)
         (/in /base)
         (/off 0)
         (/row 0)
         (/col 0)
         (/last nil))
    (while /in
      (let ((x (/line)))
        (let ((r ($map #'%eval (if (fnd "=" x) (vector x) ($split ";" x)))))
          (unless silent (pp (rhd r)))))))
  nil)

#|
(defmacro expanded (string)
  (let* ((*read-input* (as-list string))
         (seq          *read-input*)
         (os           nil))
    (while-bind it (read-toplevel seq :single t)
      (setf seq (cdr it))
      (let* ((expr (parse (lhd (car it))))
             (c    ($compile expr)))
        (push c os)))
    `(progn (muffle-compiler-note
              (muffle-compiler-warning
                (progn ,@os))))))
|#

(defun $load (path)
  (format t "Loading ~S~%" path)
  ($eval (fold-enc (load-file-bytes path)) :silent t :src path))

(in-package :symta)

(defparameter *mm-declares* nil)


(defun simple-arglist? (ys xs)
  (unless xs (return-from simple-arglist? t))
  (let ((x (lhd xs)))
    (if (coma? x) (setf x (2nd x)))
    (and (sym? x)
         (not (find x ys))
         (simple-arglist? (cons x ys) (ltl xs)))))

(defun checked-cmp (a b)
  (if (fn? b)
      (st (c $b $a))
      (st (≥≤ $a (do $b)))))

(defun expand-suff (xs e)
  (cond (xs (st (_let ($(lhd xs)) $(expand-suff (ltl xs) e))))
        (t  e)))

(defun checked-incut-match (size ob pb chk p bs ps xs cont fail suff)
  ;; FIXME: do-chk duplicates checked-arg functionality
  (declare (ignorable ob))
  (with-unisyms (n e r |B|)
    (let* ((tail   nil)
           (l      nil)
           (sn     (intern n "st"))
           (se     (intern e "st"))
           (bt     (st (_if (cl (< $sn $se))
                            (do (set_l $n (cl (+ $sn 1)))
                                (goto (\\ $|B|)))
                            $fail)))
           (do-chk (fn (do-bt)
                     (if chk
                         (st (do (|_assign| $pb $(checked-cmp l p))
                                 (_if $pb
                                      $tail
                                      $(if do-bt bt fail))))
                         tail))))
      (setf l (if chk
                  (if (wildcard? ob) (unisym) ob)
                  p))
      (unless (or chk (wildcard? ob))
        (setf suff (st (($ob $l) $$suff))))
      (unless (or ps size) ; no need for backtracing
        (setf tail (expand-suff suff (funcall cont bs)))
        (return-from checked-incut-match
          (st (_let (($l $xs)
                     $$(if (not (msg? pb)) (st (($pb n)))))
                $(funcall do-chk nil)))))
      (setf tail (expand-suff suff (checked-ltl bs ps r cont (if size fail bt))))
      (st (_let (($n $(or size 0)) ($e (len $xs)) ($l n) ($r n)
                 $$(if (not (msg? pb)) (st (($pb n)))))
            (do (label (\\ $|B|))
                (set_l $l (t $n $xs))
                (set_l $r (d $n $xs)) ; SPEED: move it after l=p check
                ;;(say $l "  " $r)
                $(funcall do-chk (not size))))))))

(defun checked-incut (p bs ps xs cont fail)
  ;; paren-less form:
  ;;   ob:pb:@xs
  ;; normalized form:
  ;;   @(ob:pb:xs)
  (let* (chk    ; dont bind p, just check against bound var
         pref
         suff
         size
         (p (2nd p))
         (p (1st p)) (ob (2nd p))
         (p (3rd p)) (pb (2nd p))
         (p (3rd p)))
    (if (wildcard? pb) (setf pb (unisym)))
    ($list-case p
      (|.| (a b)
        (setf size a)
        (setf p    b)))
    (cond
      ((fn? p) (setf chk t))
      ((wildcard? p) (setf p (unisym)))
      ((and (not chk) (sym? p)) (if (fnd p bs)
                                    (setf chk t)
                                    (setf bs (pre p bs))))
      (t (setf chk t)))
    (if (var-sym? ob) (setf bs (pre ob bs)))
    (if (var-sym? pb) (setf bs (pre pb bs)))
    (conc pref (checked-incut-match size ob pb chk p bs ps xs cont fail suff))))

(defun free-sym? (p bs) (and (sym? p) (not (fnd p bs))))
(defun simple-incut? (p bs)
  (when (incut? p)
    (let* ((p (2nd p))
           (p (1st p)) (ob (2nd p))
           (p (3rd p)) (pb (2nd p))
           (p (3rd p)))
      (and (wildcard? ob) (wildcard? pb) (free-sym? p bs)
           (not (fn? p))))))
(defun single-incut? (ps bs)
  (and (= (len ps) 1) (simple-incut? (1st ps) bs)))
(defun unincut-target (x) (3rd (3rd (1st (2nd x)))))

(defun checked-ltl (bs ps l cont fail &key (check t))
  (unless ps (return-from checked-ltl (st (_if $l $fail $(funcall cont bs)))))
  (let ((p  (lhd ps))
        (ps (ltl ps)))
    (if (incut? p)
        (return-from checked-ltl (checked-incut p bs ps l cont fail)))
    (let* ((x    (unisym))
           (xs   (unisym))
           (cont (fn (bs)
                   (if (single-incut? ps bs)
                       (st (_let (($(unincut-target (1st ps)) (ltl $l)))
                             $(funcall cont bs)))
                       (st (_let (($xs (ltl $l)))
                             $(checked-ltl bs ps xs cont fail))))))
           (r    nil))
      (setf r (cond ((wildcard? p) (funcall cont bs))
                    ((free-sym? p bs) (st (_let (($p (lhd $l)))
                                            $(funcall cont (pre p bs)))))
                    (t (st (_let (($x (lhd $l)))
                             $(checked-arg bs p x cont fail))))))
      ;; FIXME: move this to `cont`
      (if check (st (_if $l $r $fail)) r))))

;NOTE: will fail if a is nil
(defun norm-unesc (x)
  (when (unesc? x)
    (with-unisyms (a)
      (setf x (st (do (_fn ($a) (_if ($"n" (≥≤ $a (do $(2nd x))))
                                     (_if $a $a $t)
                                     n)))))))
  x)

(defun checked-lst-fn (ob pb x ps)
  (setf x (norm-unesc x))
  (let* ((p (st (_parse ((|:| $ob (|:| $pb $x)))))))
    (pre p ps)))

(defun match-str-incut (v ps)
  (conc (map 'vector
             (fn (x) (st (|\\| $(string x))))
             (2nd v))
        ps))

;; normalize and expand incuts of composite data
(defun checked-lst-normalize (ps)
  (unless ps (return-from checked-lst-normalize))
  (let*((v  (lhd ps))
        (ps (checked-lst-normalize (ltl ps)))
        (x (progn
            (when (or (fn? v) (unesc? v))
              (return-from checked-lst-normalize
                (checked-lst-fn (st _) (st _) v ps)))
            ($list-case v
              (|@| (v)
                (when (str? v)
                  (return-from checked-lst-normalize
                    (match-str-incut v ps)))
                (when (and (unesc? v) (str? (2nd v)))
                  (setf v (st (|!| (|<>| ((|[]| $(match-str-incut (2nd v) ps))))))))
                (st (|@| ((|:| _ (|:| _ $(norm-unesc v)))))))
              (|:| (b1 v)
                (when (or (fn? v) (unesc? v))
                  (return-from checked-lst-normalize
                    (checked-lst-fn (st _) b1 v ps)))
                ($list-case v
                  (|@| (v) (st (|@| ((|:| _ (|:| $b1 $(norm-unesc v)))))))
                  (|:| (b2 v)
                    (when (or (fn? v) (unesc? v))
                      (return-from checked-lst-normalize
                        (checked-lst-fn b1 b2 v ps)))
                    ($list-case v
                      (|@| (v) (st (|@| ((|:| $b1 (|:| $b2 $(norm-unesc v)))))))
                      ))))))))
    (pre (or x v) ps)))

(defun kw? (x)
  (and (bnd? x) (equ (1st (3rd x)) "{}")))

(defmacro xs? (x)
  `(typecase ,x
     (array t)
     (cons  t)
     (gs    t)))

(defun checked-lst (bs ps a cont fail)
  (let* ((ps (checked-lst-normalize ps)))
    (cond
      ((not ps) (checked-eq bs nil a cont fail))
      (t (st (_if (cl (|xs?| $a))
                $(if (single-incut? ps bs)
                     (st (_let (($(unincut-target (1st ps)) $a))
                           $(funcall cont bs)))
                     (checked-ltl bs ps a cont fail :check nil))
                $fail))))))

(defun checked-eq (bs p a cont fail)
  (declare (ignorable bs))
  (st (_if $(checked-cmp a p)
           $(funcall cont bs)
           $fail)))

(defun checked-bnd-eq (bs p a cont fail)
  (let ((v (2nd p))
        (p (3rd p))
        (u (unisym)))
    (if (unesc? p) (setf p (2nd p)))
    (st (_let (($u $(checked-cmp a p)))
          (_if $u
               $(checked-arg bs v u cont fail)
               $fail)))))

(defun checked-bnd (bs ps a cont fail)
  (let* ((x (1st ps))
         (y (2nd ps))
         (cont (fn (bs)
                 (if (fn? x)
                     (checked-eq bs x a cont fail)
                     (checked-arg bs x a cont fail)))))
    (if (fn? y)
        (checked-eq bs y a cont fail)
        (checked-arg bs y a cont fail))))

(defun checked-quasi (bs p a cont fail)
  (labels ((process-quasi (o)
             (cond ((quote? o) (st '$o))
                   ((unquote? o) (2nd o))
                   ((lst? o) (st (|[]| $($map #'process-quasi o))))
                   (t (st (quote $o))))))
      (let ((p (process-quasi p)))
        (checked-arg bs p a cont fail))))


(defun checked-fn (bs p a cont fail)
  ;; (ob:pb:x)
  (setf p (lhd p))
  (let* ((ob (2nd p)) (p (3rd p))
         (pb (2nd p)) (p (3rd p))
         (u (if (var-sym? pb) pb (unisym))))
    (st (_let (($u $(checked-cmp a p)))
          (_if $u
               $(if (var-sym? pb)
                    (checked-arg bs ob a cont fail)
                    (let ((cont (fn (bs) (checked-arg bs ob a cont fail))))
                      (checked-arg bs pb u cont fail)))
               $fail)))))


(defun checked-type (bs p a cont fail)
  (declare (ignorable fail))
  (st (do (|:| $p $a)
          $(funcall cont (pre (2nd p) bs)))))

(defun checked-arg (bs p a cont fail)
  (let ((hd (lhd p)))
    ($case hd
      (_parse (checked-fn bs (2nd p) a cont fail))
      (|\\| (checked-quasi bs (2nd p) a cont fail))
      (|:| (checked-bnd bs (ltl p) a cont fail))
      (|,| (checked-type bs p a cont fail))
      (|.| (st (_let (($(2nd p) (set $p $a))) $(funcall cont bs))))
      (|[]| (checked-lst bs (2nd p) a cont fail))
      (otherwise
       (cond
         ((wildcard? p) (funcall cont bs))
         ;;((fn-sym? p) (checked-eq bs p a cont fail))
         ((free-sym? p bs) (st (_let (($p $a))
                                 $(funcall cont (pre p bs)))))
         ((bnd? hd) (checked-bnd-eq bs hd a cont fail))
         (t (checked-eq bs p a cont fail)))))))

(defun checked-args (bs ps as cont fail)
  (unless ps (return-from checked-args (funcall cont bs)))
  (let* ((a    (lhd as))
         (as   (ltl as))
         (p    (lhd ps))
         (ps   (ltl ps))
         (cont (fn (bs) (checked-args bs ps as cont fail))))
    (checked-arg bs p a cont fail)))


(defun merge-lambs (as ls fail)
  (unless ls (return-from merge-lambs nil))
  (let* ((l    (lhd ls))
         (ls   (ltl ls))
         (ps   (1st l))
         (cont (fn (bs) (2nd l)))
         (ps   (checked-lst-normalize ps))
         (chk  (checked-args nil ps as cont fail)))
    (if ls
        (with-unisyms (|u|)
          (st (_let (($|u| (_fn () $chk)))
                $(merge-lambs as ls (st (c $|u|))))))
        chk)))

(defun filter-keywords-hlp (ks rs as)
  (unless as (return-from filter-keywords-hlp (cons ks rs)))
  (let* ((a  (lhd as))
         (as (ltl as)))
    (if (kw? a)
        (let* ((x (2nd a))
               (y (2nd (3rd a)))
               (a (lst x y)))
          (filter-keywords-hlp (suf a ks) rs as))
        (filter-keywords-hlp ks (suf a rs) as))))

(defun filter-keywords (ks rs ls)
  (unless ls (return-from filter-keywords (cons ks rs)))
  (let* ((l  (lhd ls))
         (as (lhd l))
         (r  (filter-keywords-hlp nil nil as))
         (ks (conc ks (car r)))
         (rs (suf (lst (cdr r) (2nd l)) rs)))
    (filter-keywords ks rs (ltl ls))))

(defun arg-syms (args) ($map (fn (x) (unisym)) args))

(defun construct-mm (name ls)
  (let* ((xs (filter-keywords nil nil ls))
         (ks (car xs))
         (ls (cdr xs))
         (as (arg-syms (lhd (maximum (fn (x) (len (1st x)))
                                     (sq-to-list ls)))))
         (mm (merge-lambs as ls nil))
         (as (if ks (pre ks as) as)))
    (when *mm-declares*
      (setf mm (st (cl (|with-decls| $(list-to-sq *mm-declares*)
                                     (|@| $mm))))))
    ;;(when *mm-declares*
    ;;  (setf mm (st (cl (locally (declare $$(list-to-sq *mm-declares*))
    ;;                            (|@| $mm))))))
    (when name (setf mm (st (cl (block $name ($"@" $mm))))))
    (st (_fn $as $mm))))

;; combine separate funargs into a multimethod
(defun lambs-to-multimethod (ls &key (clean nil) (name nil) (macro nil))
  (setf ls (rev ls))
  (setf ls ($map (fn (x)
                   (let ((x (split-body x)))
                     (case (len x)
                       (0 (lst nil t))
                       (1 (suf t x))
                       (2 x)
                       (otherwise (st ($(lhd x) (do $$(ltl x))))))))
                 ls))
  (when macro
    (setf ls ($map (fn (l)
                     (st (((|[]| $(lhd l)))
                          (do $$(ltl l)))))
                     ls)))
  (unless (listp name) (setf name (list name)))
  (let* ((*mm-declares* nil)
         (r (construct-mm (car name) ls)))
    (if name (setf r (st (name_fn (_q $(intern (car name) "st")) $r))))
    (if (and (not clean) name)
        (setf r (st (_let ($$(list-to-sq (mapcar (fn (x) (st ($x n)))
                                                 name)))
                      (do (set_l $(car name) $r)
                          $$(list-to-sq
                             (mapcar (fn (x)
                                       (st (set_l $x $(car name))))
                                     (cdr name)))
                  )))))
    r))


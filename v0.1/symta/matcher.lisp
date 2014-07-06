(in-package :symta)

;;                  !!! DONT BE AFRAID !!!


(defun fn? (x) (and (not (wildcard? x))
                    (or (fn-sym? x) (blk? x) (_fn? x))))

(defparameter |st|::|*;st;getParserInputSize*| (fn (k name) nil))

;; makes sure our `==` won't catch user's `?`
;; FIXME: are we sanitized everything?
(defun checked-sanity (p)
  (if (or (sym? p) (fn? p)) p (setf p (st (do $p)))))


(defun get-parser-input-size (x)
  (if (sym? x) (funcall (deco |getParserInputSize|) #'identity x)))


(defun expand-suff (xs e)
  (cond (xs (st (_let ($(lhd xs)) $(expand-suff (ltl xs) e))))
        (t  e)))

(defun checked-parse (p in a bs ps l cont fail s suff)
  (if (or (not in) (wildcard? in))
      (setf in (unisym)))
  (setf bs (pre in (pre a bs)))
  (let* ((xs (unisym))
         (cont (fn (bs) (st (_let (($xs (drop $s $l)))
                               $(checked-ltl bs ps xs cont fail))))))
    (st (_let (($in (take $s $l))
               $$(if (not (msg? a)) (st (($a n)))))
           (_if (|=:| $a (== $in $p))
                $(expand-suff suff (funcall cont bs))
                $fail)))))

(defun checked-incut-match (ob pb chk p bs ps xs cont fail suff)
  ;; FIXME: do-chk duplicates checked-arg functionality
  (declare (ignorable ob))
  (with-unisyms (n e r c cx bt)
    (let* ((tail   nil)
           (l      nil)
           (reset  (st (do (set_l $n (cl + $n 1))
                           ($c n))))
           (btfn   (st (_fn () (_if (cl < $n $e) $reset $fail))))
           (do-chk (fn (do-bt)
                     (if chk
                         (st (do (|=:| $pb (|==| $l $p))
                                 (_if $pb
                                      $tail
                                      $(if do-bt (lst bt) fail))))
                         tail))))

       (setf l (if chk
                   (if (wildcard? ob) (unisym) ob)
                   p))

       (unless (or chk (wildcard? ob))
         (setf suff (pre (lst ob l) suff)))

       (unless ps ; no need for backtracing
         (setf tail (expand-suff suff (funcall cont bs)))
         (return-from checked-incut-match
           (st (_let (($l $xs)
                      $$(if (not (msg? pb)) (st (($pb n)))))
                  $(funcall do-chk nil)))))

       (setf tail (expand-suff suff (checked-ltl bs ps r cont (lst bt))))
       (st
        (_let (($n 0) ($e (len $xs)) ($c n) ($l n) ($r n)
               $$(if (not (msg? pb)) (st (($pb n)))))
          (_let (($bt $btfn))
            (do (cont (_fn ($cx) (set_l $c $cx)))
                (set_l $l (take $n $xs))
                (set_l $r (drop $n $xs)) ; SPEED: move it after l=p check
                ;;(say $l "  " $r)
                $(funcall do-chk t))))))))


(defun checked-incut (p bs ps xs cont fail)
  ;; paren-less form
  ;;   ob:pb:@!xs
  ;;   ob:pb:@[l++!f]
  ;; normalized form:
  ;;   @(ob:pb:!xs)
  ;;   @(ob:pb:[l++!f])

  ;; FIXME: if size hint gets changed, we are in trouble
  ;; FIXME: pass whole input to a hinting function to get match length
  ;; FIXME: try doing binary search
  (let* (chk    ; dont bind p, just check against bound var
         count
         chk-count
         size-hint
         eb
         pref
         suff
         elt-size
         (p (2nd p))
         (p (1st p)) (ob (2nd p))
         (p (3rd p)) (pb (2nd p))
         (p (3rd p)))
    (if (wildcard? pb) (setf pb (unisym)))
    ($list-case p
      (lst (x)
        ($list-case x
          (|++| (c e)
            (setf count c)
            (setf p e)
            ($list-case p
              (|@| (v)
                (setf chk t)
                (setf p   v))
              (otherwise
               (unless (sym? p)
                 ($list-case p (|!| (v) (setf p v)))
                 (with-unisyms (x)
                   (setf p (st (fn ((lst $x)) (|==| $x $p)))))
                 (setf chk t)))))))
      (|!| (v)
        (setf chk t)
        (setf p   v)))

    (when (fn? p) (setf chk t))
    (when chk (setf size-hint
                    (if (sym? p) (get-parser-input-size p))))

    (unless chk
      (if (sym? p)
          (if (fnd p bs)
              (setf chk t)
              (unless (wildcard? p)
                (setf bs (pre p bs))))
          (setf chk t)))

    (when count
      ;; FIXME: allow !count:elem
      (if (wildcard? ob) (setf ob (unisym)))

      ($list-case count
        (|!| (v) 
          (setf chk-count t)
          (setf count v))
        (otherwise
         (if (or (not (sym? count)) (fnd count bs))
             (setf chk-count t))))

      (unless size-hint (setf size-hint 1))
      (unless chk-count (setf size-hint nil))

      (when size-hint
        (setf elt-size size-hint)
        ;; FIXME: move `*` to pref
        ;; FIXME: use (cl * ..)
        (setf size-hint (if (and (numberp count) (numberp size-hint))
                            (* size-hint count)
                            (st (* $size-hint $count)))))

      (if (and (not chk) (symbolp p))
          (setf eb p))

      (let* ((es (unisym)))
        (setf p
          (cond
            ((not chk)
             (if chk-count
               (st (_fn ($es) (_if (|==| (len $es) $count)
                                   (lst $es)
                                   n)))
               (progn
                 (unless (wildcard? count)
                   (setf suff (pre (st ($count (len $pb))) suff)))
                 (if (wildcard? p)
                     (st (_fn ($es) (lst $es)))
                     (st |_matchDups|)))))
            ((and chk-count elt-size)
             (st (_fn ($es) (|_matchArrayFNS| $count $elt-size $p $es))))
            (chk-count
             (st (_fn ($es) (|_matchArrayFN| $count $p $es))))
            (elt-size
             (unless (wildcard? count)
               (setf suff (pre (st ($count (len $pb))) suff)))
             (st (_fn ($es) (|_matchArrayFS| $elt-size $p $es))))
            (t
             (unless (wildcard? count)
               (setf suff (pre (st ($count (len $pb))) suff)))
             (st (_fn ($es) (|_matchArrayF| $p $es))))
            ))
        (setf suff (pre (st ($pb (lhd $pb))) suff))
        (setf chk t)
        ))

    (if (wildcard? p) (setf p (unisym)))

    (conc pref
          (if (and size-hint (not (fnd p bs)))
              (checked-parse p ob pb bs ps xs cont fail size-hint suff)
              (checked-incut-match ob pb chk p bs ps xs cont fail suff)))))

(defun free-sym? (p bs) (and (sym? p) (not (fnd p bs))))
(defun simple-incut? (p bs)
  (when (incut? p)
    (let* ((p (2nd p))
           (p (1st p)) (ob (2nd p))
           (p (3rd p)) (pb (2nd p))
           (p (3rd p)))
      (and (wildcard? ob) (wildcard? pb) (free-sym? p bs)
           (not (fn? p))))))
(defun single-incut? (ps bs) (and (= (len ps) 1) (simple-incut? (1st ps) bs)))
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


(defun checked-lst-fn (ob pb x ps)
  (let* ((p (st (|!| ((|:| $ob (|:| $pb $(2nd x))))))))
    (pre p ps)))


;; normalize and expand incuts of composite data
(defun checked-lst-normalize (ps)
  (unless ps (return-from checked-lst-normalize))
  (let*((p  (lhd ps))
        (ps (checked-lst-normalize (ltl ps)))
        (x  ($list-case p
              (|!| (v) (st (|!| ((|:| _ (|:| _ $v))))))
              (|@| (v)
                (when (str? v)
                  (return-from checked-lst-normalize (conc (as-list v) ps)))
                (st (|@| ((|:| _ (|:| _ $v))))))
              (|++| (l e) (st (|@| ((|:| _ (|:| _ (lst $p)))))))
              (|:| (b1 v)
               (when (unesc? v)
                 (return-from checked-lst-normalize
                   (checked-lst-fn (st _) b1 v ps)))
                ($list-case v
                  (|@| (v) (st (|@| ((|:| _ (|:| $b1 $v))))))
                  (|++| (l e) (st (|@| ((|:| _ (|:| $b1 (lst $v)))))))
                  (|:| (b2 v)
                    (when (unesc? v)
                      (return-from checked-lst-normalize
                        (checked-lst-fn b1 b2 v ps)))
                    ($list-case v
                      (|@| (v) (st (|@| ((|:| $b1 (|:| $b2 $v))))))
                      (|++| (l e) (st (|@| ((|:| $b1 (|:| $b2 (lst $v)))))))
                      )))))))
    (pre (or x p) ps)))

(defun checked-lst (bs ps a cont fail)
  (let* ((ps (checked-lst-normalize ps))
         (ks (keep (fn-k (x) (kw? x)) ps))
         (ps (keep (fn-k (x) (not (kw? x))) ps))
         (c cont)  (k nil))
    (when ks
      (setf cont
            (fn (bs)
              (cond (ks (setf k (lhd ks) ks (ltl ks))
                        (let* ((u (unisym)) (x (2nd k)) (y (3rd k))
                               (x (if (sym? x) (sym-path-name x) x)))
                          (st (_let (($u (bget $x $a)))
                                $(checked-arg bs y u cont fail)))))
                    (t (funcall c bs))))))
    (cond
      ((and ks (not ps)) (funcall cont bs))
      ((eql (len ps) 0) (checked-eq bs nil a cont fail))
      (t (st (_if (lst? $a)
                $(if (single-incut? ps bs)
                     (st (_let (($(unincut-target (1st ps)) $a))
                           $(funcall cont bs)))
                     (checked-ltl bs ps a cont fail :check nil))
                $fail))))))

(defun checked-eq (bs p a cont fail)
  (declare (ignorable bs))
  (st (_if (|==| $a $(checked-sanity p))
           $(funcall cont bs)
           $fail)))

(defun checked-bnd-eq (bs p a cont fail)
  (let ((v (2nd p))
        (p (3rd p))
        (u (unisym)))
    (if (unesc? p) (setf p (2nd p)))
    (st (_let (($u (|==| $a $(checked-sanity p))))
          (_if $u
               $(checked-arg bs v u cont fail)
               $fail)))))

(defun checked-bnd (bs ps a cont fail)
  (let* ((x (1st ps))  (y (2nd ps))
         (cont (fn (bs)
                 (if (fn? x)
                     (checked-eq bs x a cont fail)
                     (checked-arg bs x a cont fail)))))
    (if (fn? y)
        (checked-eq bs y a cont fail)
        (checked-arg bs y a cont fail))))

(defun checked-quasi (bs p a cont fail)
  (labels ((process-quasi (o)
             (cond ((interp? o) (2nd o))
                   ((quasi? o) (st (quote $o)))
                   ((lst? o) (st (lst $$($map #'process-quasi o))))
                   (t (st (quote $o))))))
      (let ((p (process-quasi (2nd p))))
        (checked-arg bs p a cont fail))))

(defun checked-fn (bs p a cont fail)
  ;; (ob:pb:x)
  (setf p (lhd p))
  (let* ((ob (2nd p)) (p (3rd p))
         (pb (2nd p)) (p (3rd p))
         (u (unisym)))
    (st (_let (($u (|==| $a $(checked-sanity p))))
          (_if $u
               $(let ((cont (fn (bs) (checked-arg bs ob a cont fail))))
                  (checked-arg bs pb u cont fail))
               $fail)))))

(defun checked-arg (bs p a cont fail)
  (cond
    ((lst? p)
     (let ((hd (lhd p))
           (tl (ltl p)))
       ($case hd
         (|!| (checked-fn bs (lhd tl) a cont fail))
         (\#  (checked-quasi bs p a cont fail))
         (|:| (checked-bnd bs tl a cont fail))
         (|.| (let ((cs (process-curries p)))
                (if (eql cs p)
                    (st (_let (($(2nd p) (set $p $a))) $(funcall cont bs)))
                    (checked-eq bs cs a cont fail))))
         (lst (checked-lst bs tl a cont fail))
         (otherwise
          (cond
            ((bnd? hd) (checked-bnd-eq bs hd a cont fail))
            (t (checked-eq bs p a cont fail)))))))
    ((wildcard? p) (funcall cont bs))
    ((checker-sym? p) (checked-eq bs p a cont fail))
    ((free-sym? p bs) (st (_let (($p $a))
                            $(funcall cont (pre p bs)))))
    (t (checked-eq bs p a cont fail))))

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
        (with-unisyms (u)
          (st (_let (($u (_fn () $chk)))
                    $(merge-lambs as ls (lst u)))))
        chk)))

(defun filter-keywords-hlp (ks rs as)
  (unless as (return-from filter-keywords-hlp (cons ks rs)))
  (let* ((a  (lhd as))
         (as (ltl as)))
    (if (kw? a)
        (filter-keywords-hlp (suf (ltl a) ks) rs as)
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
(defun construct-mm (ls)
  (let* ((xs (filter-keywords nil nil ls))
         (ks (car xs))
         (ls (cdr xs))
         (as (arg-syms (lhd (maximum (fn (x) (len (1st x)))
                                     (sq-to-list ls)))))
         (mm (merge-lambs as ls nil))
         (as (if ks (pre ks as) as)))
    (st (_fn $as $mm))))

;; combine separate funargs into a multimethod
(defun lambs-to-multimethod (ls &key (clean nil) (name nil))
  (setf ls (rev ls))
  (setf ls ($map (fn (x)
                   (let ((x (split (st ->) x)))
                     (case (len x)
                       (0 (st (n y)))
                       (1 (st ($$x y)))
                       (2 x)
                       (otherwise (st ($(lhd x) (do $$(ltl x))))))))
                 ls))
  (let* ((mm (construct-mm ls))
         (r (if name
                (st (name_fn (|'| $name) $mm))
                mm))
         (r (if (or clean (not name))
                r
                (st (_let (($name n))
                      (set_l $name $r))))))
    ;;(pp r)
    (st (|asFn| $r))))


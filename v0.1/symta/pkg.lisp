(in-package :symta)

(defun glb-decor (x) (concatenate 'string "*" x "*"))
(defparameter *sym-decor* #'glb-decor) ; symbol decorator
(defparameter *pkgs* nil)

;; symta's package for predefined symbols
(defparameter *base-pkg* (lst "" "st"))

;; current package, we are working with
(defparameter *cur-pkg* *base-pkg*)



(defun str-path (str) (split #\; str))
(defun sym-path (sym) (str-path (symbol-name sym)))
(defun sym-path-name (sym) (sq-to-str (rhd (sym-path sym))))
(defun path-str (path) (funcall *sym-decor* (sq-to-str (unsplit #\; path))))
(defun path-intern (path)
  (let ((s (intern (path-str path) *cl-pkg*)))
     (unless (boundp s) (setf (symbol-value s) nil))
      s))

(defun pkg-relate (type name &key (breaked nil))
  (setf name (sq-to-str name))
  (if (eql (rhd name) #\;) (setf name (sq-to-str (rtl name))))
  (let* ((s  (path-intern *cur-pkg*))
         (rs (ns-get (symbol-value s) type)))
    (if breaked
        (let ((p (str-path name)))
          (unless (fnd p rs) ;; dont add multiple entries
            (setf (symbol-value s)
                  (ns-set (symbol-value s) type (pre p rs)))
            nil))
        (setf (symbol-value s)
              (ns-set (symbol-value s) type (ns-set rs name t))))))

(defun shades-imports (what is)
  (unless is (return-from shades-imports nil))
  (let* ((i  (lhd is))
         (s  (path-intern i))
         (es (ns-get (symbol-value s) "exports")))
    (if (ns-get es what)
        i
        (shades-imports what (ltl is)))))

(defun shaded? (path)
  (if (> (len path) 1) (error "FIXME: add shading for multicomponent paths"))
  (setf path (rhd path))
  (let* ((s  (path-intern *cur-pkg*))
         (is (ns-get (symbol-value s) "imports")))
    (shades-imports path is)))

(defun $intern (x)
  (let ((path (split #\; (sq-to-str x))))
    (when (/= (len (lhd path)) 0) ;; relative path?
      (let* ((pkg (shaded? path)))
        (setf path (conc (if pkg pkg *cur-pkg*) path))))
    (let ((glb-sym (path-intern path)))
      (let* ((*sym-decor* #'identity)
             (s (path-intern path)))
        (setf (symbol-value s) glb-sym) ;; for access to global value
        s))))

(defun get-path-value (path)
  (let ((f (find-symbol (path-str path) *cl-pkg*)))
    (if f (symbol-value f))))

(defun set-path-value (path value)
  (setf (symbol-value (path-intern path)) value))

(defun del-sym (name)
  (unintern (path-intern (suf "properties" (str-path name))))
  (unintern ($intern name))
  (let ((*sym-decor* #'identity))
    (unintern (path-intern (suf "properties" (str-path name))))
    (unintern ($intern name))))

(defun set-prop (sym prop-name value)
  (let ((ps (path-intern (suf "properties" (sym-path sym)))))
    (setf (symbol-value ps)
          (ns-set (symbol-value ps) prop-name value)))
  value)

(defun get-prop (sym prop-name)
  (let ((ps (path-intern (suf "properties" (sym-path sym)))))
    (ns-get (symbol-value ps) prop-name)))

(defun rm-prop (sym prop-name)
  (let ((ps (path-intern (suf "properties" (sym-path sym)))))
    (setf (symbol-value ps)
          (ns-rm (symbol-value ps) prop-name))))

(defun def-sym (sym &key (val nil))
  (setf (symbol-value sym) val))

(defun purge-package (pkg)
  (let ((s (path-intern pkg)))
    (makunbound s)
    (unintern s)))

(eval-when (:load-toplevel :execute)
  (progn
    ($map #'purge-package *pkgs*)
    (setf *pkgs* (lst *base-pkg*)))
  )


;; our custom gensym version
(defun unisym (&optional (name "G"))
  (gensym (sq-to-str (conc ";t;" name))))

(defmacro with-unisyms (names &body body)
  `(let ,(loop for name in names collect
              `(,name (unisym ,(string-downcase (string name)))))
     ,@body))

(defun env-sym? (sym &optional (env *env*))
  (ns-get env sym))

(defun env-push (args &optional (env *env*))
  ($map (fn (x) (setf env (ns-set env x 1)))
        args)
  env)

(defun macro? (s)
  (unless (sym? s) (return-from macro? nil))
  (if (not (env-sym? s)) ; macros can be shadowed by local name
      (get-prop s "macro")))

(defun checker-sym? (x)
  (unless (sym? x) (return-from checker-sym? nil))
  (let ((name (symbol-name x)))
    (and (> (length name) 0)
         (eql #\? (aref name (- (length name) 1))))))

(defun wildcard? (p) (eql p (st _)))


(in-package :cl-user)
(defpackage :snv
  (:use :cl :cffi :libc)
  (:export
   list-directory
   file-exists-p
   directory-pathname-p
   file-pathname-p
   pathname-as-directory
   pathname-as-file
   walk-directory
   pwd
   cd
   ls
   muffle-compiler-note
   muffle-compiler-style-warning
   muffle-compiler-warning
   current-function-name
   named-fn
   object-address
   string-to-utf8
   utf8-to-string
   ;;function-arglist
   bytes
   load-file-bytes
   save-file-bytes
   snarf-file
   atake adrop acmp acpy acat each
   avg
   u1 u2 u4 u8
   s1 s2 s4 s8
   cptr
   vec fb cw
   f+ f- f* f/ f<< f>>
   u+ u- u* u/ u<< u>> umod uxor uand uior unot uneg
   sc u4-u1 u1-u4

   group
   with-gensyms
   fn
   function-name
   bind
   bind-struct
   while
   rec
   if-bind
   while-bind
   map-each
   to-kw
   to-float
   pos-if
   split-if
   split
   unsplit
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
   displace
   rng
   printable-char-p
   hd
   equal-case

   %ind %the ; remove these
   % %new %ptr %clone %crop %put %get %arr %off
   raw lsb msb utf8 deser
   ser-arr ser-dup ser-lsb ser-msb ser-utf8 ser
   save-file load-file
   times collect
   ))
(in-package :snv)

(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))




(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))


    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))


(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))

(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun pathname-as-file (name)
  "Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

#|
(defun folder-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))
|#

(defun bytes (n)
  (make-array n :element-type '(unsigned-byte 8)))

(defun load-file-bytes (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((b (bytes (file-length stream))))
      (read-sequence b stream)
      b)))

(defun save-file-bytes (path bytes)
  (with-open-file (stream path :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence bytes stream)))


(defun snarf-file (file)
  ;; encoding-resistant file reader.  You can't use FILE-LENGTH
  ;; because in the presence of variable-length encodings (and DOS
  ;; linefeed conventions) the length of a file can bear little resemblance
  ;; to the length of the string it corresponds to.  Reading each line 
  ;; like this wastes a bunch of space but does solve the encoding
  ;; issues.
  (with-open-file (in file :direction :input)
    (loop for read = (read-line in nil nil)
          while read
          for i upfrom 1
          collect read into lines
          sum (length read) into len
          finally (return
                   (let ((huge (make-string (+ len i))))
                     (loop with pos = 0
                           for line in lines
                           for len = (length line)
                           do (setf (subseq huge pos) line
                                    (aref huge (+ pos len)) #\Newline
                                    pos (+ pos len 1))
                           finally (return huge)))))))
    

(defun pwd () (namestring (truename ".")))
(defun cd (path)
  (unless (case (aref path 0) (#\/ t) (#\\ t))
    (setf path (concatenate 'string (pwd) path)))
  (let ((dst (pathname-as-directory (pathname path))))
    (when (folder-p (namestring dst))
      (libc::chdir path) ;;change program working directory
      (setf *default-pathname-defaults* dst) ;;change where SBCL loads files
      t)))
(defun ls (&optional path)
  (unless path (setf path "."))
  (unless (case (aref path 0) (#\/ t) (#\\ t))
    (setf path (concatenate 'string (pwd) path)))
  (mapcar #'namestring (list-directory path)))


#|
(defun open-temporary-file ()
  (or
  ;;#+allegro (make-temp-file-name)
  ;;#+clisp (mkstemp)
   (loop thereis (open (format nil "TEMP-~D" (random 100000))
                       :direction :probe :if-exists nil
                       :if-does-not-exist :create))))
|#





(defmacro muffle-compiler-note (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro muffle-compiler-style-warning (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext::style-warning))
       ,@body))

(defmacro muffle-compiler-warning (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext::warning))
       ,@body))

(defun exit ()
  #+sbcl (sb-ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #+allegro (excl:exit))

(defmacro current-function-name ()
  (or #+SBCL `(caaddr (sb-debug::backtrace-as-list))
      ;#+ACL (excl::external-fn_symdef)
      ''unnamed))

#+SBCL (defmacro named-fn (name args &body body)
         `(SB-INT:NAMED-LAMBDA ,name ,args ,@body))
#-SBCL (defmacro named-fn (name args &body body)
         `(fn ,args ,@body))

;;(defun function-arglist (x)
;;  #+sbcl (sb-kernel:%simple-fun-arglist x))


#|
(defmacro lexically-bound-p (symbol)
  `(handler-case (or ,symbol t)
     (unbound-variable (c)
       (declare (ignorable c))
       nil)))


(defmacro lexically-bound-p (variable &environment env)
  (eq :lexical (sb-cltl2:variable-information variable env)))

(defmacro symbol-macro-bound-p (variable &environment env)
  (eq :symbol-macro (sb-cltl2:variable-information variable env)))

(defun get-env-vars (env)
  (mapcar #'car (sb-c::lexenv-vars env)))

(defun env-boundp (sym env)
  (find sym (get-env-vars env)))

;;(eql (type-of (sb-kernel:make-null-lexenv)) env)
;;(defmacro lexixally-bound-p (sym &environment env)
;;  `(identity ',(find sym (mapcar #'car (sb-c::lexenv-vars env)))))

(defmacro env-vars (&environment extern-env)
  (get-env-vars extern-env))

|#




;; On 32-bit machines lower three bits of address word usually contain type tag.
;; Example:
;; 000    even-fixnum
;; 001    defstruct-instance-pointer
;; 010    character
;; 011    cons-cell (and NIL)
;; 100    odd-fixnum
;; 101    function-pointer
;; 110    other-immediate-1
;; 111    other-pointer
(defmacro object-address (object)
  (let ((x (gensym)))
    `(let ((,x ,object))
       (muffle-compiler-note
         (locally (declare (optimize (safety 0) (speed 3)))
           (sb-kernel:get-lisp-obj-address ,x))))))


#|
;; We can use sb-sys:with-pinned-objects to lock GC from moving them
(defun object-address (object)
   "Given an arbitrary object, return an integer (should be fixnum in
many cases, unknown how Xerox and Gold Hill deal with this) which is
unique to that object, generally its actual pointer address"
   (flet ((abut (x y)
           (+ y (* x (expt 10 (1+ (floor (log y 10))))))))
     (or
      #+CCL      (ccl::%address-of object)
      #+CMU     (kernel:get-lisp-obj-address object)
      #+SBCL    (sb-kernel:get-lisp-obj-address object)
                 ;; (sb!kernel:get-lisp-obj-address object)
      #+LISPWORKS (sys:object-address object)
      #+(AND EXCL (NOT (AND ALLEGRO-VERSION>= (VERSION>= 5 0))))
                 (excl::pointer-to-fixnum object)
      #+(AND EXCL ALLEGRO-VERSION>= (VERSION>= 5 0))
                 (excl::pointer-to-address object)
      #+SYMBOLICS       (si:%pointer object)
      #+CORAL   (ccl::%ptr-to-int object)
      #+GOLD-HILL (multiple-value-call #'abut (sys:%pointer object))
      #+HP      (prim:@inf object)
      #+IBCL    (si:address object)
      #+KCL     (si:address object)
      #+LUCID   (lucid::%pointer object)
      #+PYR     0
      #+TI      (si:%pointer object)
      #+VAXL    (system::%sp-pointer->fixnum object)
      #+XEROX   (abut (il::\\hiloc object) (il::\\loloc object))
      ;; ECL? CORMAN?
      (extract-pointer object))))

;;; If the above fails, something like this might work in your Lisp
(defun extract-pointer (object)
   (values
    (parse-integer
     (with-output-to-string (s)
       (print-unreadable-object (object s :identity t)))
     :start 6
     :radix 16.
     :junk-allowed t))) 
|#


#|
(defun collect-all-objects-of-type (type)
  (assert (not (eq type 'cons)) () "unable to count conses as we're
collecting them to list. Collect conses to vector!")
  (let (obj-list)
    (loop for space in '(:static :dynamic :read-only)
       do (sb-vm::map-allocated-objects
           (lambda (o tag bytes)
             (declare (ignorable tag bytes))
             (when (typep o type)
               (pushnew o obj-list)))
           space))
    obj-list))
|#




(defmacro with-gensyms (names &body body)
  `(let ,(loop for name in names collect `(,name (gensym ,(string name))))
     ,@body))


(defmacro times (v n &body body)
  (with-gensyms (c)
    `(do ((,v 0 (1+ ,v))
          (,c ,n))
         ((>= ,v ,c) NIL)
       (declare (type fixnum ,v ,c)
                (ignorable ,v))
       ,@body)))



(defmacro while (expr &body body)
  (with-gensyms (var)
    `(do ((,var ,expr ,expr))
         ((not ,var))
       ,@body)))

(defmacro rec (name binds &body body)
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

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


(defmacro bind-struct (type struct fs &body b)
  (let ((s (gensym)))
    `(let ((,s ,struct))
       (symbol-macrolet
           ,(mapcar (lambda (x)
                      (let* ((v (if (listp x) (first x) x))
                             (x (if (listp x) (second x) x))
                              (a (intern (concatenate 'string (symbol-name type) "-" (symbol-name x))
                                         (symbol-package type))))
                        `(,v (,a ,s))))
                    fs)
       ,@b))))

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



;; (split #\. "abc.def") -> ("abc" "def")
(defun split (sep xs)
    (loop for i = 0 then (1+ j)
          as j = (position sep xs :start i)
          collect (subseq xs i j)
          while j))

(defun unsplit (sep xs)
  (setf sep (string sep))
  (reduce (fn (a b) (concatenate 'string a sep b)) (cdr xs)
          :initial-value (or (car xs) "")))


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
(defun false (x) (declare (ignorable x)) nil)


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


(defun displace (start size array)
  (make-array size
              :element-type (array-element-type array)
              :displaced-to array :displaced-index-offset start))


(defun atake (n xs)
  (if (>= n (length xs))
      xs
      (multiple-value-bind (a start) (array-displacement xs)
        (if a
            (displace start n a)
            (displace 0 n xs)))))

(defun adrop (n xs)
  (when (< n (length xs))
    (multiple-value-bind (a start) (array-displacement xs)
      (if a
          (displace (+ start n) (- (length xs) n) a)
          (displace n (- (length xs) n) xs)))))

(defun acmp (xs ys)
  (let ((l (min (length xs) (length ys))))
    (dotimes (i l)
      (when (/= (aref xs i) (aref ys i))
        (return-from acmp nil)))
    t))

(defun acpy (d s)
  (let ((l (min (length d) (length s))))
    (dotimes (i l)
      (setf (aref d i) (aref s i))))
  d)

(defmacro each (e a &body b)
  (let ((i (gensym))
        (x (gensym)))
    `(let ((,x ,a))
       (symbol-macrolet ((,e (aref ,x ,i)))
         (times ,i (length ,x)
           ,@b))
       nil)))


(defun acat (a b)
  (concatenate `(simple-array ,(array-element-type a) (*)) a b))

(defmacro avg (&rest xs)
  `(truncate (+ ,@xs) ,(length xs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rng (a b)
    (if (< a b)
        (loop as i from a to b collect i)
        (loop as i from a downto b collect i))))

(defmacro collect (v s e &body body)
  `(mapcar (lambda (,v) ,@body) (rng ,s ,e)))

(defun printable-char-p (c)
  (or (<= #x20 (char-code c) #x7E)))

(defun hd (s bs)
  (let ((s (min s (length bs))))
    (dotimes (y (truncate (+ s 15) 16))
      (format t "~4,'0X |" (* y 16))
      (dotimes (x 16)
        (let ((i (+ (* y 16) x)))
          (when (= (rem i 16) 8)  (format t " "))
          (if (< i s)
              (format t " ~2,'0X" (aref bs i))
              (format t "   "))))
      (format t " |")
      (dotimes (x 16)
        (let* ((i (+ (* y 16) x)))
          (format t "~a" (if (and (< i s) (<= #x20  (aref bs i) #x7E))
                             (code-char (aref bs i))
                             #\Space))))
      (format t "~%"))))

(defmacro equal-case (key &body handlers)
  (let ((k (gensym "k")))
    `(let ((,k ,key))
       (cond ,@(mapcar (fn (h)
                         (if (equal (car h) 'otherwise)
                             `(t ,@(cdr h))
                             `((equal ,(car h) ,k) ,@(cdr h))))
                       handlers)))))


(deftype u1 () '(unsigned-byte 8))
(deftype u2 () '(unsigned-byte 16))
(deftype u4 () '(unsigned-byte 32))
(deftype u8 () '(unsigned-byte 64))
(deftype s1 () '(signed-byte 8))
(deftype s2 () '(signed-byte 16))
(deftype s4 () '(signed-byte 32))
(deftype s8 () '(signed-byte 64))
(deftype cptr () 'sb-sys:system-area-pointer)
;(deftype cptr () '(sb-alien:alien (* t)))

;; fetch bytes
(defmacro fb (n x) `(ldb (byte 8 ,(* n 8)) ,x))

;; create a word from bytes
(defmacro cw (&rest xs)
  `(the u4 (logior ,@(mapcar (lambda (x n) `(ash (the (integer 0 255) ,x) ,(* 8 n)))
                             xs (rng 0 (- (length xs) 1))))))
 

(defmacro vec (size type &rest dup)
  `(make-array ,size :element-type ',type
                     :initial-element ,(if dup (car dup) 0)))

(defun string-to-utf8 (string) (sb-ext:string-to-octets string))
(defun utf8-to-string (bytes)
  (sb-ext:octets-to-string
   (coerce bytes '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)))))




;; unsigned mod32 arithmetic

(declaim (inline u+) (ftype (function (u4 u4) u4) u+))
(defun u+ (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (+ a b)))
#+sbcl (define-compiler-macro u+ (a b) `(ldb (byte 32 0) (+ ,a ,b)))

(declaim (inline u-) (ftype (function (u4 u4) u4) u-))
(defun u- (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (- a b)))
#+sbcl (define-compiler-macro u- (a b) `(ldb (byte 32 0) (- ,a ,b)))

(declaim (inline u*) (ftype (function (u4 u4) u4) u*))
(defun u* (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (* a b)))
#+sbcl (define-compiler-macro u* (a b) `(ldb (byte 32 0) (* ,a ,b)))

(declaim (inline u/) (ftype (function (u4 u4) u4) u/))
(defun u/ (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (truncate a b)))
#+sbcl (define-compiler-macro u/ (a b) `(ldb (byte 32 0) (truncate ,a ,b)))

(declaim (inline umod) (ftype (function (u4 u4) u4) umod))
(defun umod (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (mod a b)))
#+sbcl (define-compiler-macro umod (a b) `(ldb (byte 32 0) (mod ,a ,b)))

(declaim (inline uxor) (ftype (function (u4 u4) u4) uxor))
(defun uxor (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logxor a b)))
#+sbcl (define-compiler-macro uxor (a b) `(ldb (byte 32 0) (logxor ,a ,b)))

(declaim (inline uand) (ftype (function (u4 u4) u4) uand))
(defun uand (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logand a b)))
#+sbcl (define-compiler-macro uand (a b) `(ldb (byte 32 0) (logand ,a ,b)))

(declaim (inline uior) (ftype (function (u4 u4) u4) uior))
(defun uior (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logior a b)))
#+sbcl (define-compiler-macro uior (a b) `(ldb (byte 32 0) (logior ,a ,b)))

(declaim (inline u<<) (ftype (function (u4 (unsigned-byte 5)) u4) u<<))
(defun u<< (num count)
  (declare (type u4 num))
  (declare (type (unsigned-byte 5) count))
  (ldb (byte 32 0) (ash num count)))
#+sbcl (define-compiler-macro u<< (num count)
         `(logand #xffffffff (ash (the u4 ,num) (the (unsigned-byte 5) ,count))))


(declaim (inline u>>) (ftype (function (u4 (unsigned-byte 5)) u4) u>>))
(defun u>> (num count)
  (declare (type u4 num))
  (declare (type (unsigned-byte 5) count))
  (ldb (byte 32 0) (ash num (the (signed-byte 6) (- count)))))
#+sbcl (define-compiler-macro u>> (num count)
         `(logand #xffffffff (ash (the u4 ,num) (the (signed-byte 6) (- (the (unsigned-byte 5) ,count))))))


(declaim (inline unot) (ftype (function (u4) u4) unot))
(defun unot (x)
  (declare (type u4 x))
  (ldb (byte 32 0) (lognot x)))
#+sbcl (define-compiler-macro unot (num) `(ldb (byte 32 0) (lognot ,num)))

(declaim (inline uneg) (ftype (function (u4) u4) uneg))
(defun uneg (x)
  (declare (type u4 x))
  (ldb (byte 32 0) (- x)))
#+sbcl (define-compiler-macro uneg (num) `(ldb (byte 32 0) (lognot ,num)))


(defmacro f+ (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (+ (the fixnum ,x) (the fixnum ,y))))))

(defmacro f- (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (- (the fixnum ,x) (the fixnum ,y))))))

(defmacro f* (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (* (the fixnum ,x) (the fixnum ,y))))))

(defmacro f/ (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (truncate (the fixnum ,x) (the fixnum ,y))))))


(defmacro f>> (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum (- ,y)))))))


(defmacro f<< (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum ,y))))))


(defmacro size-in-array (type)
  (let* ((n 100000)
         (o (with-output-to-string (s)
              (let ((*standard-output* s)
                    (*trace-output* s))
                (eval `(time (make-array ,n :element-type ',type))))))
         (o (loop for i = 0 then (1+ j)
                  as j = (position #\Newline o :start i)
                  collect (subseq o i j)
                  while j))
         (o (or #+sbcl (caddr (reverse o))
                #+clozure (cadr (reverse o))
                (error "size-in-array: your platform is not supported")))
         (o (coerce (remove-if (lambda (x) (char= x #\,)) (coerce o 'list))
                    'string))
         (o (truncate (read-from-string o) n)))
    o))


(defun %grow (s l)
  (declare ((SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) s)
           (fixnum l))
  (let* ((n (length s))
         (d (vec (* 2 (+ n l)) u1)))
    (dotimes (i n) (setf (aref d i) (aref s i)))
    d))

(defmacro %new (&rest as) `(cons (vec ,(if as (car as) 0) u1) 0))
(defmacro %ptr (x) `(cons (the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) ,x) 0))
(defun %clone (x) (cons (car x) (cdr x)))
(defun %crop (x) 
  (let* ((d (vec (cdr x) u1))
         (s (car x)))
    (dotimes (i (length d))
      (setf (aref d i) (aref s i)))
    d))
(defmacro %arr (x) `(the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (car ,x)))
(defmacro %off (x) `(the fixnum (cdr ,x)))

(defun %put (p s)
  (let* ((d (%arr p))
         (i (%off p))
         (n (+ i (length s))))
    (when (< (length d) n)
      (let ((nd (vec (* 2 n) u1)))
        (acpy nd d)
        (setf d nd)))
    (loop as v across s
       do (progn (setf (aref d i) v)
                 (incf i)))
    (setf (car p) d
          (cdr p) i)
    p))

(defun %get (p n)
  (let* ((s (%arr p))
         (j (%off p))
         (l (- (length s) j))
         (d (vec (min n l) u1)))
    (dotimes (i (length d))
      (setf (aref d i) (aref s j))
      (incf j))
    (setf (cdr p) j)
    d))


(defmacro % (&rest as)
  (let ((n 1)
        (p nil)
        (v (gensym))
        (g (gensym))
        (a (gensym))
        (i (gensym)))
    (when (numberp (first as))
      (setf n  (car as)
            as (cdr as)))
    (setf p  (car as)
          as (cdr as))
    `(let* ((,g ,p)
            (,a (%arr ,g))
            ,@(when as `((,v ,(car as))))
            (,i (the fixnum (cdr ,g))))
       ,@(when as
           `((when (<= (length ,a) ,i)
               (setf ,a (the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (%grow ,a 1)))
               (setf (car ,g) ,a))))
       (prog1 (aref ,a ,i)
         ,@(when as `((setf (aref ,a ,i) (logand #xFF ,v))))
         ,@(unless (eql n 0) `((setf (cdr ,g) (the fixnum (+ ,i ,n)))))))))


(defmacro raw  (n s f)
  (with-gensyms (x)
    `(let ((,x (%get ,s ,n)))
       (if (= (length ,x) ,n)
           ,x
           ,f))))

(defun lsbmsb (n s f &key lsb)
  (with-gensyms (tmp)
    `(let ((,tmp (raw ,n ,s ,f)))
       (logior ,@(mapcar (lambda (i) `(ash (aref ,tmp ,i)
                                           ,(* (if lsb i (- n 1 i)) 8)))
                         (rng 0 (- n 1)))))))

(defmacro lsb  (n s f) (lsbmsb n s f :lsb t))
(defmacro msb  (n s f) (lsbmsb n s f :lsb nil))
(defmacro utf8 (n s f) `(utf8-to-string (raw ,n ,s ,f)))


(defmacro deser (stream fields &body body)
  (let* ((s (gensym))
         (b (gensym))
         (l (gensym))
         (f `(error "~a: failed to deserialize stream" (current-function-name))))
    (when (eql (first body) :on-fail)
      (setf f (second body))
      (setf body (cddr body)))
    (let ((bs (mapcar (lambda (x)
                        (when (numberp (second x))
                          (setf x (cons (car x) (cons 'lsb (cdr x)))))
                        (let* ((h (car x))
                               (b `(,@(cdr x) ,s (funcall ,l)))
                               (g (gensym))
                               (c (gensym)))
                          (when (consp h)
                            (setf b `(let ((,g ,b)
                                           (,c ,(second h)))
                                       (unless (if (arrayp ,c)
                                                   (acmp ,g ,c)
                                                   (equal ,g ,c))
                                         ,f)))
                            (setf h g))
                          `(,h ,b)))
                      fields)))
      `(block ,b
         (let* ((,l (lambda () ,f))
                (,s ,stream)
                (,s (if (arrayp ,s) (%ptr ,s) ,s)) ;in case user passes raw array
                ,@bs)
           (declare (ignorable ,@(mapcar #'first bs)))
           ,@body)))))


(defmacro ser-arr  (s &rest as)
  (let* ((ys (car (last as)))
         (as (butlast as))
         (y (gensym)))
    (if (and (= (length as) 1) (eq (car as) 1))
        `(%put ,s ,ys)
        (progn (when (numberp (car as))
                 (setf as (cons 'lsb as)))
               `(loop as ,y across ,ys
                   do (,(intern (concatenate 'string "SER-" (symbol-name (car as))))
                        ,s
                        ,@(cdr as)
                        ,y))))))

(defmacro ser-dup (s n &rest x)
  (when (numberp (car x))
    (setf x (cons 'lsb x)))
  `(loop as i from 0 below ,n
      do (,(intern (concatenate 'string "SER-" (symbol-name (car x))))
           ,s
           ,@(cdr x))))


(defun ser-lsbmsb (stream n val &key lsb)
  (with-gensyms (s v)
    `(let ((,s ,stream)
           (,v ,val))
       ,@(mapcar (lambda (i) `(% ,s (ash ,v ,(- (* (if lsb i (- n 1 i)) 8)))))
                 (rng 0 (- n 1))))))

(defmacro ser-lsb  (s n v) (ser-lsbmsb s n v :lsb t))
(defmacro ser-msb  (s n v) (ser-lsbmsb s n v :lsb nil))
(defmacro ser-utf8 (s v) `(ser-arr ,s 1 (string-to-utf8 ,v)))


(defmacro ser (stream &body fields)
  (with-gensyms (s)
    `(let* ((,s ,(if (eq stream t) `(%new) stream)))
       ,@(mapcar (lambda (x)
                   (let* (;(n (car x))
                          (x (cdr x)))
                     (when (numberp (car x))
                       (setf x (cons 'lsb x)))
                     `(,(intern (concatenate 'string "SER-" (symbol-name (car x))))
                        ,s
                        ,@(cdr x))))
                 fields)
       ,@(when (eq stream t) `((%crop ,s)))
       )))


;; sequence coerce
(defmacro sc (elem-type seq) `(coerce ,seq '(simple-array ,elem-type (*))))

(defun save-file (path data)
  (with-open-file (stream path
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence data stream))
  nil)

(defun load-file (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (vec (file-length stream) u1)))
      (read-sequence bytes stream)
      bytes)))


(defmacro u4-u1-m (n x msb)
  (with-gensyms (o i c p)
    `(let* ((,p ,x)
            (,o (vec (* ,n (length ,p)) u1))
            (,i 0))
       (times ,c (length ,p)
         ,@(loop as j below n collect
                `(progn (setf (aref ,o ,i) (ldb (byte 8 ,(if msb (* 8 (- n 1 j)) (* 8 j)))
                                                (aref ,p ,c)))
                        (incf ,i))))
       ,o)))

;; breaks array `s` of u4 into `c` interleaved channels of `w` bytes each
(defun u4-u1 (c s &key (w 1) (msb t))
  (declare (optimize (safety 0) (speed 3))
           (fixnum c w)
           (type (SIMPLE-ARRAY u4 (*)) s))
  (when (and (= w 1) (< c 5))
    (return-from u4-u1
      (if msb
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u4-u1-m ,i s t))))
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u4-u1-m ,i s nil)))))))
  (let* ((d (vec (f* c (length s)) u1))
         (h (f/ (length s) w))
         (i 0)
         (j 0)
         (wc (f* w c)))
    (declare (fixnum wc i j))
    (macrolet ((m (msb)
                 `(times y h
                    (times x w
                      (let ((v (aref s i)))
                        (times k c
                          (setf (aref d j) (ldb (byte 8 ,(if msb
                                                             `(f* 8 (f- (f- c 1) k))
                                                             `(f* 8 k)))
                                                v))
                          (incf j w)))
                      (incf i)
                      (decf j wc)
                      (incf j))
                    (incf j (f* w (f- c 1))))))
      (if msb (m t) (m nil)))
    d))

(defmacro u1-u4-m (n x msb)
  (with-gensyms (o i c p)
    `(let* ((,p ,x)
            (,o (vec (truncate (length ,p) ,n) u4))
            (,c 0))
       (declare (fixnum ,c))
       (times ,i (length ,o)
         (setf (aref ,o ,i)
               (logior ,@(collect j 0 (- n 1)
                                  `(u<< (aref ,p (prog1 ,c (incf ,c)))
                                        ,(if msb (* 8 (- n 1 j))  (* 8 j)))))))
       ,o)))

;; construct array of u4 from `c` interleaved channels from `s` of `w` bytes each
(defun u1-u4 (c s &key (w 1) (msb t))
  (declare (optimize (safety 0) (speed 3))
           (fixnum c w)
           (type (SIMPLE-ARRAY u1 (*)) s))
  (when (and (= w 1) (< c 5))
    (return-from u1-u4
      (if msb
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u1-u4-m ,i s t))))
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u1-u4-m ,i s nil)))))))
  (let* ((h (truncate (truncate (length s) c) w))
         (d (vec (* w h) u4))
         (i 0)
         (j 0))
    (declare (fixnum h i j))
    (macrolet ((m (msb)
                 `(times y h
                    (times k c
                      (times x w
                        (let ((v (aref s i)))
                          (setf (aref d j) (uior (aref d j)
                                                 (u<< v ,(if msb
                                                             `(* 8 (- c k 1))
                                                             `(* 8 k)))))
                          (incf i)
                          (incf j)))
                      (decf j w))
                    (incf j w))))
      (if msb (m t) (m nil))
      d)))


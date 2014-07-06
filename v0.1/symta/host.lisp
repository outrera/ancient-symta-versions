;; Host dependent utils go here

(in-package :cl-user)
(defpackage :host
  (:use :cl)
  (:export
   list-directory
   file-exists-p
   directory-pathname-p
   file-pathname-p
   pathname-as-directory
   pathname-as-file
   walk-directory
   directory-p
   file-p
   load-file-bytes
   save-file-bytes
   snarf-file
   muffle-compiler-note
   muffle-compiler-style-warning
   muffle-compiler-warning
   named-fn
   object-address
   ;;function-arglist
   ))
(in-package :host)
   

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

(defun directory-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))



(defun load-file-bytes (name)
  (with-open-file (in name :direction :input :element-type '(unsigned-byte 8))
    (let* ((n (file-length in))
           (a (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref a i) (read-byte in)))
      a)))

(defun save-file-bytes (name seq)
  (with-open-file (out name :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (dotimes (i (length seq))
      (write-byte (aref seq i) out))))


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

#+SBCL (defmacro named-fn (name args &body body)
         `(SB-INT:NAMED-LAMBDA ,name ,args ,@body))
#-SBCL (defmacro named-fn (name args &body body)
         `(fn ,args ,@body))

;;(defun function-arglist (x)
;;  #+sbcl (sb-kernel:%simple-fun-arglist x))

(defun object-address (x)
  #+sbcl (sb-kernel:get-lisp-obj-address x)
  #+lispworks (sys:object-address x))



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


#|
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

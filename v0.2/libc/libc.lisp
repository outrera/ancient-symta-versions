(defpackage :libc
  (:use :cl :cffi)
  (:export struct foreign-slot-type -> p+ malloc free strlen strcpy
           memset memcmp memmove memcpy
           path-parts folder-p file-p)
  )
(in-package :libc)


;(deftype cptr () 'sb-sys:system-area-pointer)

;; FIXME: trivial-garbage can't work directly with unwrapped pointers.
;(defstruct wrapping (ptr (null-pointer) :type cptr))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *structs* (make-hash-table :test 'equal)))

(defmacro struct (name-and-options &body fields)
  (let ((name name-and-options)
        (slots (loop as x in fields collect `(,(first x) ,(second x))))
        (fields (loop as x in fields collect
                     (if (listp (second x)) ; pointer to value?
                         `(,(first x) ,(first (second x)) ,@(cdr (cdr x)))
                         x))))
    (when (listp name) (setf name (first name)))
    `(progn (setf (gethash ',name *structs*) ',slots)
            (defcstruct ,name-and-options ,@fields))))

(defun foreign-slot-type (struct-name slot-name)
  (second (assoc slot-name (gethash struct-name *structs*))))

(defmacro -> (type base &rest slots)
  (loop as slot in slots do
    (progn (setf base `(foreign-slot-value ,base ',type ',slot))
           (setf type (foreign-slot-type type slot))
           (when (listp type) (setf type (second type))) ;; pointer to
           ))
  base)

(defmacro p+ (p v) `(cffi:make-pointer (+ (cffi:pointer-address ,p) ,v)))


(define-symbol-macro path-separator (or #+cffi-features:windows #\\
                                        #\/))

(defcfun "malloc" :pointer (count :int))
(defcfun "free" :void (ptr :pointer))

(defcfun "tmpnam" :string (buffer :pointer))

(defcfun "system" :int (command :string))
(defcfun "unlink" :int (path :string))
(defcfun "chdir"  :int (path :string))

(defcfun "opendir" :pointer (dirname :string))
(defcfun "closedir" :int (dirp :pointer))

(defcfun "popen" :pointer (command :string) (mode :string))
(defcfun "pclose" :int (stream :pointer))
(defcfun "fopen" :pointer (filename :string) (mode :string))
(defcfun "fclose" :int (stream :pointer))
(defcfun "fflush" :int (stream :pointer))
(defcfun "fread" :int (ptr :pointer) (size :int) (nitems :int) (stream :pointer))
(defcfun "fwrite" :int (ptr :pointer) (size :int) (nitems :int) (stream :pointer))
(defcfun "ftell" :long (stream :pointer))
(defcfun "fseek" :int (stream :pointer) (offset :long) (whence :int))

(defcfun "fgets" :pointer (buffer :string) (size :int) (stream :pointer))

(defcfun "strlen" :int (string :pointer))
(defcfun "strcpy" :pointer (dst :pointer) (src :pointer))
(defcfun "memset" :pointer (b :pointer) (c :int) (len :int))
#+darwin
(defcfun ("memset_pattern4" memset_pattern4) :pointer (b :pointer) (pattern :pointer) (len :int))
(defcfun "memcpy" :pointer (dst :pointer) (src :pointer) (count :int))
(defcfun "memmove" :pointer (dst :pointer) (src :pointer) (count :int))
(defcfun "memcmp" :pointer (a :pointer) (b :pointer) (count :int))

(defcfun "getenv" :string (name :string))
(defcfun "setenv" :int (name :string) (value :string) (overwrite :int))
(defcfun "unsetenv" :int (name :string))


(define-symbol-macro SEEK_SET 0)
(define-symbol-macro SEEK_CUR 1)
(define-symbol-macro SEEK_END 2)


(define-symbol-macro S_IFMT          #o170000)         ; type of file mask
(define-symbol-macro S_IFIFO         #o010000)         ; named pipe (fifo)
(define-symbol-macro S_IFCHR         #o020000)         ; character special
(define-symbol-macro S_IFDIR         #o040000)         ; directory
(define-symbol-macro S_IFBLK         #o060000)         ; block special
(define-symbol-macro S_IFREG         #o100000)         ; regular
(define-symbol-macro S_IFLNK         #o120000)         ; symbolic link
(define-symbol-macro S_IFSOCK        #o140000)         ; socket
(define-symbol-macro S_IFWHT         #o160000)         ; whiteout

; File mode
; Read, write, execute/search by owner
(define-symbol-macro S_IRWXU         #o000700)         ; RWX mask for owner
(define-symbol-macro S_IRUSR         #o000400)         ; R for owner
(define-symbol-macro S_IWUSR         #o000200)         ; W for owner
(define-symbol-macro S_IXUSR         #o000100)         ; X for owner
; Read, write, execute/search by group
(define-symbol-macro S_IRWXG         #o000070)         ; RWX mask for group
(define-symbol-macro S_IRGRP         #o000040)         ; R for group
(define-symbol-macro S_IWGRP         #o000020)         ; W for group
(define-symbol-macro S_IXGRP         #o000010)         ; X for group
; Read, write, execute/search by others
(define-symbol-macro S_IRWXO         #o000007)         ; RWX mask for other
(define-symbol-macro S_IROTH         #o000004)         ; R for other
(define-symbol-macro S_IWOTH         #o000002)         ; W for other
(define-symbol-macro S_IXOTH         #o000001)         ; X for other

(define-symbol-macro S_ISUID         #o004000)         ; set user id on execution
(define-symbol-macro S_ISGID         #o002000)         ; set group id on execution
(define-symbol-macro S_ISVTX         #o001000)         ; directory restrcted delete

(define-symbol-macro S_ISTXT         S_ISVTX)         ; sticky bit
(define-symbol-macro S_IREAD         S_IRUSR)         ; backward compatability
(define-symbol-macro S_IWRITE        S_IWUSR)         ; backward compatability
(define-symbol-macro S_IEXEC         S_IXUSR)         ; backward compatability

#+cffi-features:unix (defcfun "mkdir" :int (path :string) (mode :int))
#+cffi-features:windows (defcfun "mkdir" :int (path :string))

(defun fsize (stream)
  (let ((p (ftell stream)))
    (fseek stream 0 SEEK_END)
    (let ((e (ftell stream)))
      (fseek stream p SEEK_SET)
      e)))

(defun make-directory (path)
  (or #+cffi-features:windows (mkdir path)
      (mkdir path (logior S_IRWXU S_IRWXG S_IROTH S_IXOTH))))

(defmacro w/malloc ((var count) &body body)
  `(let ((,var (libc::malloc ,count)))
     (unwind-protect (progn ,@body)
       (libc::free ,var))))

(defmacro w/popen ((var command mode) &body body)
  `(let ((,var (libc::popen ,command ,mode)))
     (unwind-protect (progn ,@body)
       (unless (cffi:null-pointer-p ,var) (libc::pclose ,var)))))

(defmacro w/fopen ((var filename mode) &body body)
  `(let ((,var (libc::fopen ,filename ,mode)))
     (unwind-protect (progn ,@body)
       (unless (cffi:null-pointer-p ,var) (libc::fclose ,var)))))

(defmacro w/opendir ((var path) &body body)
  `(let ((,var (libc::opendir ,path)))
     (unwind-protect (progn ,@body)
       (unless (null-pointer-p ,var) (libc::closedir ,var)))))

(defun memory-to-array (m l)
  (let ((a (make-array l :element-type '(unsigned-byte 8))))
    (dotimes (i l) (setf (aref a i) (mem-ref m :uint8)))
    a))

(defun array-to-memory (m a)
  (let ((l (length a)))
    (dotimes (i l) (setf (mem-ref m :uint8) (aref a i)))
    a))



(defun split-path (path)
  (loop for i = 0 then (1+ j)
        as j = (position path-separator path :start i)
        collect (subseq path i j)
        while j))

(defun path-parts (path)
  (let* ((i (position path-separator path :from-end t))
         (dir (when i (subseq path 0 i)))
         (path (or (and i (subseq path (+ i 1)))
                   path))
         (i (position #\. path :from-end t))
         (file (or (and i (subseq path 0 i))
                   path))
         (ext (when i (subseq path (+ i 1)))))
    (list dir file ext)))

(defun folder-p (path) (w/opendir (dir path) (not (null-pointer-p dir))))
(defun file-p (path)
  (unless (folder-p path)
    (w/fopen (f path "r")
      (not (null-pointer-p f)))))

(defun mkpath (path)
  (let ((l (- (length path) 1)))
    (unless (char= (aref path l) path-separator)
      (setf path (concatenate 'string path (string path-separator)))))
  (loop for i = 1 then (1+ j)
        as j = (position path-separator path :start i)
        while j
        as p = (subseq path 0 j)
        do (unless (folder-p p)
             (when (file-p p)
               (error "mkpath: path is file (~a)" p))
             (make-directory p))))

(defun load-file (path)
  (w/fopen (file path "r")
    (unless (null-pointer-p file)
      (let* ((l (fsize file)))
        (w/malloc (m l)
          (fread m 1 l file)
          (memory-to-array m l))))))

(defun generate-temporary-name ()
  (w/malloc (buf 1024) (tmpnam buf)))

(defun strip-newline (s)
  (setf (mem-ref s :uint8 (- (strlen s) 1)) 0)
  s)

(defun shell (command)
  (w/popen (out
            (or ;;#+cffi-features:unix (format nil "bash -c '~a'" command)
                (format nil "~a" command))
            "r")
    (w/malloc (buf 4096)
      (loop until (null-pointer-p (fgets buf 4096 out))
         collect (foreign-string-to-lisp (strip-newline buf))))))

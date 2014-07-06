;; Frame Buffer

(in-package :cl-user)

(in-package :sdl)

(defmacro for (inits cnd update &body body)
  `(progn ,inits
          (util::while ,cnd
            ,@body
            ,update)))

(deftype uint32 () '(unsigned-byte 32))
(deftype cptr () 'sb-sys:system-area-pointer)


;(deftype cptr () '(sb-alien:alien (* t)))


(defmacro fsp (ptr ptr-type slot)
  `(cffi:foreign-slot-pointer ,ptr ,ptr-type ,slot))

(defmacro fsv (ptr ptr-type slot)
  `(cffi:foreign-slot-value ,ptr ,ptr-type ,slot))

(defmacro adrof (object)
  (cffi:pointer-address object))

(defmacro rgb (r g b)
  `(the uint32 (logior (ash (the (integer 0 255) ,r) 16)
                       (ash (the (integer 0 255) ,g) 8)
                       (the (integer 0 255)  ,b))))

(defmacro rgba (r g b a)
  `(the uint32 (logior (ash (the (integer 0 255) ,a) 24)
                       (ash (the (integer 0 255) ,r) 16)
                       (ash (the (integer 0 255) ,g) 8)
                       (the (integer 0 255) ,b))))


(defmacro w/rgb (cs &body body)
  (setf cs (util:group 4 cs))
  `(let* ,(reduce #'append
                  (mapcar (lambda (xs)
                            (destructuring-bind (r g b c) xs
                              (util:with-gensyms (x)
                                `((,x ,c)
                                  (,r (ldb (byte 8 16) ,x))
                                  (,g (ldb (byte 8  8) ,x))
                                  (,b (ldb (byte 8  0) ,x))))))
                          cs))
     ,@body))

(defmacro w/rgba (cs &body body)
  (setf cs (util:group 5 cs))
  `(let* ,(reduce #'append
                  (mapcar (lambda (xs)
                            (destructuring-bind (r g b a c) xs
                              (util:with-gensyms (x)
                                `((,x ,c)
                                  (,r (ldb (byte 8 16) ,x))
                                  (,g (ldb (byte 8  8) ,x))
                                  (,b (ldb (byte 8  0) ,x))
                                  (,a (ldb (byte 8 24) ,x))))))
                         cs))
     (declare ,@(mapcar (lambda (xs)
                         `(ignorable ,@(subseq xs 0 4)))
                       cs))
     ,@body))



(defmacro bgra-to-rgba (x)
  (util:with-gensyms (r g b a)
    `(w/rgba (,r ,g ,b ,a ,x) (rgba ,b ,g ,r ,a))))


(defmacro f+ (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (+ (the fixnum ,x) (the fixnum ,y))))))

(defmacro f- (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (- (the fixnum ,x) (the fixnum ,y))))))

(defmacro f* (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (* (the fixnum ,x) (the fixnum ,y))))))

(defmacro f/ (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (/ (the fixnum ,x) (the fixnum ,y))))))

(defmacro >> (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum (- ,y)))))))


(defmacro << (a b)
  (util:with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum ,y))))))

(defstruct rect
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type fixnum)
  (h 0 :type fixnum))

(defstruct gfx
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (pixels (null-pointer) :type cptr)
  (clips nil))

(defun top-clip (gfx)
  (car (gfx-clips gfx)))

(defun in-rect? (x y r)
  (declare (optimize (safety 0) (speed 3))
           (rect r)
           (fixnum x)
           (fixnum y))
  (and (>= x (rect-x r)) (< x (rect-w r))
       (>= y (rect-y r)) (< y (rect-h r))))

(defun clip-range (p s e)
  (cond ((< p s) s)
        ((> p e) e)
        (t p)))

(defun clip-rect (r c)
  (let ((x1 (clip-range (rect-x r) (rect-x c) (rect-w c)))
        (y1 (clip-range (rect-y r) (rect-y c) (rect-h c)))
        (x2 (clip-range (+ (rect-x r) (rect-w r)) (rect-x c) (rect-w c)))
        (y2 (clip-range (+ (rect-y r) (rect-h r)) (rect-y c) (rect-h c))))
    (make-rect :x x1 :y y1 :w (- x2 x1) :h (- y2 y1))))

(defun push-clip-rect (r gfx)
  (let ((r (clip-rect r (top-clip gfx))))
    (push (make-rect :x (rect-x r)
                     :y (rect-y r)
                     :w (+ (rect-x r) (rect-w r))
                     :h (+ (rect-y r) (rect-h r)))
          (gfx-clips gfx))))

(defun pop-clip (gfx)
  (let ((cs (cdr (gfx-clips gfx))))
    (if cs (setf (gfx-clips gfx) cs)))
  gfx)

(defun clear-pixels (color gfx)
  (let* ((m (gfx-pixels gfx))
         (w (gfx-w gfx))
         (h (gfx-h gfx))
         (l (* w h)))
    (locally
        (declare (optimize (safety 0) (speed 3))
                 (uint32 color)
                 (cptr m)
                 (fixnum w h l))
      (loop as i below l
         do (locally (declare (fixnum i)))
           (setf (mem-aref m :uint32 i) color))))
  gfx)

(defun put-pixel (x y color g)
  (declare (optimize (safety 0) (speed 3))
           (gfx g)
           (uint32 color))
  (let* ((m (gfx-pixels g))
         (w (gfx-w g))
         (p (f+ (f* y w) x)))
    (declare (cptr m)
             (fixnum x y w p))
    (setf (mem-aref m :uint32 p) color)))

(defun put-pixel-clip (x y color clip gfx)
  (declare (optimize (safety 0) (speed 3))
           (uint32 color)
           (rect clip)
           (fixnum x y))
  (if (in-rect? x y clip)
      (put-pixel x y color gfx)))


;;(declaim (ftype (function (fixnum fixnum gfx) uint32) get-pixel))

(defun get-pixel (x y g)
  (declare (optimize (safety 0) (speed 3))
           (gfx g))
  (let* ((m (gfx-pixels g))
         (w (gfx-w g))
         (p (f+ (f* y w) x)))
    (mem-aref m :uint32 p)))


(defmacro rand-xy () `(cons (random 32) (random 32)))

(defun avg-tile (g)
  (let* ((m (gfx-pixels g))
         (w (gfx-w g))
         (h (gfx-h g))
         (ar 0) (ag 0) (ab 0)
         ;;(ps #((7 . 6) (7 . 14) (15 . 6) (15 . 14)))
         (ps (vector (rand-xy) (rand-xy) (rand-xy) (rand-xy)
                     (rand-xy) (rand-xy)))
         (l (length ps)))
    (locally
        (declare (optimize (safety 0) (speed 3))
                 (uint32)
                 (cptr m)
                 (fixnum w h l ar ag ab))
      (loop as i across ps
         do (locally (declare (fixnum i)))
             (w/rgb (r g b (mem-aref m :uint32 (+ (* 32 (cdr i)) (car i))))
               (incf ar r) (incf ag g) (incf ab b)))
      (rgb (truncate ar l)
           (truncate ag l)
           (truncate ab l)))))


(defun draw-hline (color x y length gfx)
  (loop as i below length
     do (locally (declare (fixnum i)))
       (put-pixel (+ x i) y color gfx))
  gfx)

(defun draw-vline (color x y length gfx)
  (loop as i below length
     do (locally (declare (fixnum i)))
       (put-pixel x (+ y i) color gfx))
  gfx)

(defun draw-hline-clip (color x y length clip gfx)
  (let ((ex 0))
    (declare (fixnum x y length ex))
    (setf ex (+ x length))
    (for x (< x ex) (incf x)
      (put-pixel-clip x y color clip gfx)))
  gfx)

(defun draw-vline-clip (color x y length clip gfx)
  (let ((i 0))
    (declare (fixnum i x y length))
    (for nil (< i length) (incf i)
      (put-pixel-clip x (+ y i) color clip gfx)))
  gfx)

(defun draw-line (color sx sy dx dy gfx)
  (let ((x 0)
        (y 0)
        (xlen 0)
        (ylen 0)
        (incr 0)
        (p 0)
        (clip (top-clip gfx)))
    (declare (optimize (safety 0) (speed 3))
             (fixnum sx sy dx dy x y xlen ylen incr p)
             (gfx gfx)
             ;;(fixnum sx sy dx dy xlen ylen incr p)
             (rect clip)
             (uint32 color))
    (when (= sx dx)
      (return-from draw-line
        (if (< sy dy)
            (draw-vline-clip color sx sy (the fixnum (+ dy (- sy) 1))
                             clip gfx)
            (draw-vline-clip color dx dy (the fixnum (+ sy (- dy) 1))
                             clip gfx))))
    (when (= sy dy)
      (return-from draw-line
        (if (< sx dx)
            (draw-hline-clip color sx sy (the fixnum (+ dx (- sx) 1))
                             clip gfx)
            (draw-hline-clip color dx dy (the fixnum (+ sx (- dx) 1))
                             clip gfx))))
    (when (> sy dy)
      (rotatef sx dx)
      (rotatef sy dy))
    (setf ylen (- dy sy))
    (if (> sx dx)
        (setf xlen (- sx dx)
              incr -1)
        (setf xlen (- dx sy)
              incr 1))
    (setf x sx
          y sy)
    (cond
      ((> xlen ylen)
       (when (> sx dx)
         (rotatef sx dx)
         (setf y dy))
       (setf p (- (ash ylen 1) xlen))
       (for (setf x sx) (< x dx) (incf x)
         (put-pixel-clip x y color clip gfx)
         (cond ((>= p 0)
                (incf y incr)
                (incf p (ash (- ylen xlen) 1)))
               (t (incf p (ash ylen 1))))))
      ((> ylen xlen)
       (setf p (- (ash xlen 1) ylen))
       (for (setf y sy) (< y dy) (incf y)
         (put-pixel-clip x y color clip gfx)
         (cond ((>= p 0)
                (incf x incr)
                (incf p (ash (- xlen ylen) 1)))
               (t (incf p (ash xlen 1))))))
      ((= ylen xlen) ; diagonal line
       (for nil (/= y dy) (progn (incf y) (incf x incr))
           (put-pixel-clip x y color clip gfx))))
    gfx))

(defun draw-rect (color x y w h gfx)
  (let ((clip (top-clip gfx)))
    (draw-hline-clip color x y w clip gfx)
    (draw-hline-clip color x (+ y h -1) w clip gfx)
    (draw-vline-clip color x (+ y 1) (- h 2) clip gfx)
    (draw-vline-clip color (+ x w -1) (+ y 1) (- h 2) clip gfx)))

(defun fill-rect (color x y w h gfx)
  (let ((e (+ y h))
        (clip (top-clip gfx)))
    (for nil (< y e) (incf y)
      (draw-hline-clip color x y w clip gfx))))

(defun draw-circle (color x y r gfx)
  (let ((p (- 1 r))
        (px 0)
        (py r)
        (clip (top-clip gfx)))
    (locally (declare (optimize (safety 0) (speed 3))
                      (fixnum x y r p px py)
                      (rect clip)
                      (uint32 color)))
    (for nil (<= px (+ py 1)) (incf px)
      (put-pixel-clip (+ x px) (+ y py) color clip gfx)
      (put-pixel-clip (+ x px) (- y py) color clip gfx)
      (put-pixel-clip (- x px) (+ y py) color clip gfx)
      (put-pixel-clip (- x px) (- y py) color clip gfx)
      (put-pixel-clip (+ x py) (+ y px) color clip gfx)
      (put-pixel-clip (+ x py) (- y px) color clip gfx)
      (put-pixel-clip (- x py) (+ y px) color clip gfx)
      (put-pixel-clip (- x py) (- y px) color clip gfx)
      (cond ((< p 0) (incf p (+ (* 2 px) 3)))
            (t (incf p (+ (* 2 (- px py)) 5))
               (decf py))))))

(defun fill-circle (color x y r g)
  (let ((p (- 1 r))
        (px 0)
        (py r)
        (clip (top-clip g)))
    (declare (optimize (safety 0) (speed 3))
             (fixnum x y r p px py)
             (rect clip)
             (uint32 color))
    (for nil (<= px py) (incf px)
      (draw-vline-clip color
                       (the fixnum (+ x px)) y
                       (the fixnum (+ py 1))
                       clip g)
      (draw-vline-clip color
                       (the fixnum (+ x px)) (the fixnum (- y py))
                       py
                       clip g)
      (when (/= px 0)
        (draw-vline-clip color
                         (the fixnum (- x px)) y
                         (the fixnum (+ py 1))
                         clip g)
        (draw-vline-clip color
                         (the fixnum (- x px)) (the fixnum (- y py))
                         py
                         clip g))
      (cond ((< p 0) (incf p (+ (* 2 px) 3)))
            (t (incf p (+ (the fixnum (* 2 (- px py))) 5))
               (decf py)
               (when (>= py px)
                 (draw-vline-clip color
                                  (the fixnum (+ x py 1)) y
                                  (the fixnum (+ px 1))
                                  clip g)
                 (draw-vline-clip color
                                  (the fixnum (+ x py 1))
                                  (the fixnum (- y px))
                                  px
                                  clip g)
                 (draw-vline-clip color
                                  (the fixnum (- x py 1)) y
                                  (the fixnum (+ px 1))
                                  clip g)
                 (draw-vline-clip color
                                  (the fixnum (- x py 1)) (the fixnum (- y px))
                                  px
                                  clip g)))))))



(defun mark-tint (tints g &key (brightness 0))
  (let* ((p (gfx-pixels g))
         (i 0)
         (e (* (gfx-w g) (gfx-h g))))
    (declare (optimize (safety 0) (speed 3))
             (fixnum i e brightness)
             ((simple-array uint32) tints)
             (cptr p))
    (for nil (< i e) (incf i)
      (let ((c  (mem-aref p :uint32 i)))
        (when (find c tints)
          (w/rgb (r g b c)
            (setf (mem-aref p :uint32 i)
                  (rgba (clip-range (the fixnum (+ r brightness)) 0 #xff)
                        g
                        b
                        1))))))
    g))

(defun gfx-margins (g)
  (let* ((w (gfx-w g))
         (h (gfx-h g))
         (x1 w) (x2 -1)
         (x  0) (y  0)
         (sx 0)
         (xb 0) (xe  0)
         (yb h) (ye -1)
         (s (gfx-pixels g)))
    (declare (optimize (safety 0) (speed 3))
             (fixnum x y w h x1 x2 sx xb xe)
             (cptr s))
    (for nil (< y h) (incf y)
      (setf sx (f* y w))
      (setf xb w)
      (setf xe -1)
      (for (setf x 0) (< x w) (incf x)
        (w/rgba (r g b a (mem-aref s :uint32 (f+ x sx)))
          (when (/= a 255)
            (if (= xb w) (setf xb x))
            (setf xe x))))
      (when (/= xb w)
        (if (< xb x1) (setf x1 xb))
        (if (> xe x2) (setf x2 xe))
        (if (= yb h) (setf yb y))
        (setf ye y)))
    (if (/= x1 w)
        (vector x1 yb (f+ (f- x2 x1) 1) (f+ (f- ye yb) 1))
        (vector 0 0 w h))))




#|
(defparameter *test-image* nil)

(defun blit-test (gfx)
  (clear-pixels (rgb 0 0 0) gfx)
  (blit gfx 0 0 *test-image* :tint #x00FF00)
  (blit gfx 50 50 *test-image* :tint #xFFFF00)
  )

(defun run-test ()
  (setf *test-image* (mark-tint *tint-colors* (load-image "data/ship.png")
                                :brightness 20))
  (run #'blit-test)
  )
|#


(defun s-pixbuf (surface)
  (fsv surface 'sdl-surface 'pixels))
(defun s-w (surface)
  (fsv surface 'sdl-surface 'w))
(defun s-h (surface)
  (fsv surface 'sdl-surface 'h))
(defun s-pitch (surface)
  (fsv surface 'sdl-surface 'pitch))
(defun s-bpp (surface)
  (fsv (fsv surface 'sdl-surface 'format)
       'sdl-pixel-format 'BytesPerPixel))
(defun s-cmap (surface)
  (fsv (fsv (fsv surface 'sdl-surface 'format)
            'sdl-pixel-format 'palette)
       'sdl-palette 'colors))
(defun s-ncolors (surface)
  (fsv (fsv (fsv surface 'sdl-surface 'format)
            'sdl-pixel-format 'palette)
       'sdl-palette 'ncolors))
(defun s-ck (surface)
  (fsv (fsv surface 'sdl-surface 'format)
       'sdl-pixel-format 'colorkey))


(defun gfx-over-surface (s)
  (make-gfx :w (s-w s) :h (s-h s)
            :pixels (s-pixbuf s)
            :clips (list (make-rect :x 0 :y 0
                                    :w (s-w s) :h (s-h s)))))

(defun new-gfx (w h)
  (let* ((p (cffi:foreign-alloc :uint8 :count (* w h 4)))
         (g (make-gfx :w w :h h
                      :pixels p
                      :clips (list (make-rect :x 0 :y 0
                                              :w w :h h)))))
    (tg:finalize g (lambda () (cffi:foreign-free p)))
    g))


(defun surface-to-gfx (surface)
  (let* ((w   (s-w surface))
         (h   (s-h surface))
         (e   0)
         (g   (new-gfx w h))
         (d   (gfx-pixels g))
         (s   (s-pixbuf surface))
         (bpp (s-bpp surface))
         (pad (- (s-pitch surface) (* bpp w))) ; because SDL sucks
         (ps  (pointer-address s))
         (pd 0)
         (si 0) ; source color index
         (sc 0) ; source color
         (cmap (make-pointer 0)) ; color map
         (ck 257) ; color key for transparent images
         (y 0))
    (declare ;;(optimize (safety 0) (speed 3))
             (fixnum y w h e pd si ck)
             (uint32 sc ck)
             (cptr d s cmap))
    (case bpp
      (4
       (for nil (< y h) (progn (incf y) (incf ps pad))
         (for (setf e (+ pd w)) (< pd e) (progn (incf pd) (incf ps 4))
           (w/rgba (r g b a (mem-aref (make-pointer ps) :uint32))
             (setf a (- #xff a))
             (if (= a 1) (setf a 2))
             (setf (mem-aref d :uint32 pd)
                   (rgba b g r a))))))
      (3
       (for nil (< y h) (progn (incf y) (incf ps pad))
         (for (setf e (+ pd w)) (< pd e) (progn (incf pd) (incf ps 3))
           (setf (mem-aref d :uint32 pd)
                 (bgra-to-rgba (logand #xffffff
                                       (mem-aref (make-pointer ps)
                                                 :uint32)))))))
      (1
       (setf cmap (s-cmap surface))
       (setf ck (s-ck surface))
       (for nil (< y h) (progn (incf y) (incf ps pad))
         (for (setf e (+ pd w)) (< pd e) (progn (incf pd) (incf ps))
           (setf si (mem-aref (make-pointer ps) :uint8))
           (setf sc (if (eql si ck)
                        (rgba 0 0 0 #xff)
                        (mem-aref cmap :uint32 si)))
           (setf (mem-aref d :uint32 pd)
                 (bgra-to-rgba sc)))))
      (otherwise
       (error "surface-to-gfx: unsupported BPP=~a" (s-bpp surface))))
    g))

;; FIXME: convert from indexed to true-color
(defun load-image (filename)
  (let ((s (sdl-image::img-load-img filename)))
    (unless (cffi:null-pointer-p s)
      (let ((g (surface-to-gfx s)))
        (sdl-free-surface s)
        g))))


#|
(defvar fill-val 0)
(defvar fill-inc 0)

(defun color-fade (gfx)
  (cond ((<= fill-val   0) (setf fill-val   0) (setf fill-inc  1))
        ((>= fill-val 255) (setf fill-val 255) (setf fill-inc -1)))
  (clear-pixels (rgb 0 0 fill-val) gfx)
  (incf fill-val fill-inc))


(defun draw-scene (gfx)
  (clear-pixels (rgb 0 0 0) gfx)
  (draw-line (rgb #xff #xff #xff) 50 50 50 100 gfx)
  (draw-line (rgb #xff #xff #xff) 50 50 100 50 gfx)
  (draw-line (rgb #xff #xff #xff) 50 100 100 100 gfx)
  (draw-line (rgb #xff #xff #xff) 100 50 100 100 gfx)
  (draw-line (rgb #xff #xff #xff) 50 50 75 100 gfx)
  (draw-line (rgb #xff #xff #xff) 100 50 75 100 gfx)
  (fill-circle (rgb #x00 #x00 #xff) 200 200 50 gfx)
  (fill-rect (rgb #x00 #xff #x00) 200 200 80 80 gfx)
  )
|#


(defparameter *mix-channels* 16)
(defparameter *cur-channel* 0)


(defun load-music (filename)
  (let ((m (sdl-mixer::load-mus filename)))
    (when (/= (pointer-address m) 0)
      (tg:finalize m (lambda () (sdl-mixer::free-music m)))
      m)))

(defun play-music (mus)
  (sdl-mixer::play-music mus 0))

(defun music-playing ()
  (/= (sdl-mixer::playing-music) 0))

(defun stop-music ()
  (sdl-mixer::halt-music))

(defun set-music-volume (volume)
  (let ((volume (truncate (* volume sdl-mixer::+max-volume+))))
    (sdl-mixer::volume-music volume)))


(defmacro with-sdl (&body body)
  `(progn (when (< (sdl-init (logior sdl-init-video
                                     sdl-init-timer))
                   0)
            (error "couldn't init sdl"))
          (sdl-mixer::open-audio 44100 AUDIO-S16SYS 2 4096)
          (setf *mix-channels* (sdl-mixer::allocate-channels 16))
          (setf *cur-channel* 0)
          (unwind-protect (progn ,@body)
            (stop-music)
            (sdl-mixer::close-audio)
            (sdl-quit))))

(defun event-type (event)
  (fsv event 'sdl-event 'type))

(defmacro event-enum-type (enum-item-name)
  `(foreign-enum-value 'sdl-event-type ,enum-item-name))

(defmacro with-sdl-events (event-var &rest handlers)
  `(with-foreign-object ,`(,@event-var 'sdl-event)
     ,`(loop while ,`(not ,`(eql ,`(sdl-poll-event ,@event-var) 0))
          do ,`(case ,`(event-type ,@event-var)
                 ,@(mapcar #'(lambda (x) (list (event-enum-type (first x))
                                               (second x)))
                           handlers)))))

(defparameter *screen* nil) ;; SDL screen surface
(defparameter *fb* (make-gfx)) ;; screen gfx
(defparameter *input* nil)
(defparameter *done* nil)

(defun stop () (setf *done* t))

(defun resize (w h)
  ;;(format t "SDL: resizing to ~ax~a~%" w h)
  (let* ((flags (logior sdl-any-format sdl-any-format sdl-resizable))
         (s (sdl-set-video-mode w h 32 flags)))
    (when (null-pointer-p s)
      (error "SDL: couldn't set video mode"))
    (setf *screen* s)
    (setf *fb* (make-gfx :w (s-w s) :h (s-h s)
                         :pixels (s-pixbuf s)
                         :clips (list (make-rect :x 0 :y 0
                                                 :w (s-w s) :h (s-h s)))))))

(defun mod-to-list (mod)
  (let ((r nil))
    (if (/= (logand mod #x0001) 0) (push "lshift" r))
    (if (/= (logand mod #x0002) 0) (push "rshift" r))
    (if (/= (logand mod #x0040) 0) (push "lctrl" r))
    (if (/= (logand mod #x0080) 0) (push "rctrl" r))
    (if (/= (logand mod #x0100) 0) (push "lalt" r))
    (if (/= (logand mod #x0200) 0) (push "ralt" r))
    (if (/= (logand mod #x0400) 0) (push "lmeta" r))
    (if (/= (logand mod #x0800) 0) (push "rmeta" r))
    (if (/= (logand mod #x1000) 0) (push "numlock" r))
    (if (/= (logand mod #x2000) 0) (push "capslock" r))
    (if (/= (logand mod #x4000) 0) (push "mode" r))
    r))


(defun process-kb-event (event down)
  (with-foreign-slots ((keysym) event sdl-keyboard-event)
    (with-foreign-slots ((sym mod) keysym sdl-key-sym)
      (push (list (string-downcase (subseq (symbol-name sym) 8))
                  down
                  (mod-to-list mod))
            *input*))))

(defun process-mice-button (event down)
  (let ((bs (vector "mice-left" "mice-middle" "mice-right" "wheel-up" "wheel-down")))
    (with-foreign-slots ((button x y) event sdl-mouse-button-event)
      (if (<= 1 button 5)
          (push (list (aref bs (- button 1)) down x y)
                *input*)))))

(defun process-mice-motion (event)
  (with-foreign-slots ((button x y) event sdl-mouse-button-event)
    (push (list "mice-motion" nil x y) *input*)))

(defun process-events ()
  (with-sdl-events (event)
    (:sdl-key-down-event (process-kb-event event t))
    (:sdl-key-up-event (process-kb-event event nil))
    (:sdl-mouse-button-down-event (process-mice-button event t))
    (:sdl-mouse-button-up-event (process-mice-button event nil))
    (:sdl-mouse-motion-event (process-mice-motion event))
    (:sdl-video-resize-event
     (with-foreign-slots ((w h) event sdl-resize-event)
       (resize w h)))
    (:sdl-quit-event (stop))))


;; NOTE we shoul pass input to draw function, without callbacks
(defun run (draw-function &key (width 800) (height 600) (title ""))
  (with-sdl
    (resize width height)
    ;;(let ((s *screen*))
    ;;  (format t "SDL: mode=~ax~ax~a~%" (s-w s) (s-h s) (* (s-bpp s) 8)))
    (sdl-wm-set-caption title nil)
    (sdl-show-cursor 0) ; user should use his own cursor
    (setf *done* nil)
    (loop while (not *done*)
	 do (progn
          (setf *input* nil)
	      (process-events)
	      (funcall draw-function *input* *fb*)
	      (sdl-flip *screen*)))))


(defun load-sound (filename)
  (let ((s (sdl-mixer::load-wav filename)))
    (when (/= (pointer-address s) 0)
      (tg:finalize s (lambda () (sdl-mixer::free-chunk s)))
      s)))

(defun play-sound (volume snd)
  (cond ((< volume 0.0) (setf volume 0.0))
        ((> volume 1.0) (setf volume 1.0)))
  (let ((volume (truncate (* volume sdl-mixer::+max-volume+))))
    (sdl-mixer::volume *cur-channel* volume)
    (sdl-mixer::play-channel *cur-channel* snd 0)
    (setf *cur-channel* (mod (+ *cur-channel* 1) *mix-channels*))))


(define-foreign-library libpng
  ;;(:unix (:or "libpng12.0.dylib" "libpng12.dylib" "libpng12.so.0"))
  (t (:default "libpng12")))

(use-foreign-library libpng)

(defconstant +png-libpng-ver-string+ (symbol-name '|1.2.26|))
;;(load (cffi-grovel:process-grovel-file (truename "png-grovel.lisp")))


(defcfun "png_create_write_struct" :pointer
  (user-png-ver :string)
  (error-ptr :pointer)
  (error-fn :pointer)
  (warn-fn :pointer))
(defcfun "png_destroy_write_struct" :void
  (png-ptr-ptr :pointer) (info-ptr-ptr :pointer))
(defcfun "png_create_info_struct" :pointer (png-ptr :pointer))
(defcfun "png_set_write_fn" :void
  (png-ptr :pointer)
  (io-ptr :pointer)
  (write-data-fn :pointer)
  (output-flush-fn :pointer))
(defcfun "png_set_IHDR" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (width :uint32)
  (height :uint32)
  (bit-depth :int)
  (color-type :int)
  (interlace-type :int)
  (compression-type :int)
  (filter-type :int))
(defcfun "png_write_info" :void (png-ptr :pointer) (info-ptr :pointer))
(defcfun "png_write_row" :void (png-ptr :pointer) (row :pointer))
(defcfun "png_write_end" :void (png-ptr :pointer) (info-ptr :pointer))

(defparameter *png-stream* nil)


(defcallback png-write-cb :void
    ((png-ptr :pointer) (data :pointer) (length png-size))
  (declare (ignore png-ptr))
  (let ((i 0)
        (l (truncate length)))
    (declare (optimize (safety 0) (speed 3))
             (fixnum i l))
  (for nil (< i l) (incf i)
    (write-byte (mem-aref data :uint8 i) *png-stream*))))

(defcallback png-flush-cb :void ((png-ptr :pointer))
  (declare (ignore png-ptr)))

(defcallback png-error-cb :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))

(defcallback png-warn-cb :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))

(defun clone-image (g)
  (let ((ng (new-gfx (gfx-w g) (gfx-h g))))
    (blit 0 0 g ng  :|blend| nil)
    ng))

(defun save-image (file g)
  (setf g (clone-image g))
  (let* ((m (gfx-pixels g))
         (w (gfx-w g))
         (h (gfx-h g))
         (i 0)
         (l (* w h)))
    (locally
        (declare (optimize (safety 0) (speed 3))
                 (cptr m)
                 (fixnum w h i l))
      (for nil (< i l) (incf i)
        (w/rgba (r g b a (mem-aref m :uint32 i))
          (setf (mem-aref m :uint32 i) (rgba b g r (- #xff a)))))))
  (let* ((p (pointer-address (gfx-pixels g)))
         (w (gfx-w g))
         (h (gfx-h g))
         (y 0)
         (png (png-create-write-struct
               +png-libpng-ver-string+ (null-pointer)
               (callback png-error-cb) (callback png-warn-cb)))
         (info (png-create-info-struct png)))
    (png-set-write-fn png (null-pointer)
                      (callback png-write-cb) (callback png-flush-cb))
    (png-set-ihdr png info w h 8 +png-color-type-rgb-alpha+ +png-interlace-none+
              +png-compression-type-default+ +png-filter-type-default+)
    (with-open-file (*png-stream* file :element-type '(unsigned-byte 8)
                                  :direction :output :if-exists :supersede)
      (png-write-info png info)
      (for nil (< y h) (incf y)
        (png-write-row png (make-pointer (+ p (* y w 4)))))
      (png-write-end png info))
    (with-foreign-pointer (png-ptr (foreign-type-size :pointer))
      (setf (mem-ref png-ptr :pointer) png)
      (with-foreign-pointer (info-ptr (foreign-type-size :pointer))
        (setf (mem-ref info-ptr :pointer) info)
        (png-destroy-write-struct png-ptr info-ptr)))))


#|
(defun snd-test ()
  (with-sdl
    (sdl-mixer::volume -1 128)
    (let ((s (load-sound "/home/exa/prj/symta/wc2/data/sounds/human/rescue.wav")))
      (play-sound 1.0 s))
    (sdl-delay 3000)))

(defun mus-test ()
  (with-sdl
    (let ((m (load-music "/home/exa/prj/symta/wc2/data/music/neutral/disco.ogg")))
      (play-music m)
      (set-music-volume 1.0)
      (sdl-delay 1000)
      (stop-music))))
|#
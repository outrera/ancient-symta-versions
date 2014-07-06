(in-package :gfx)

(define-symbol-macro +png-header+ #(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A))

(defun png-deinterlace-adam7 (p)
  (let ((m #(1 6 4 6 2 6 4 6
             7 7 7 7 7 7 7 7
             5 6 5 6 5 6 5 6
             7 7 7 7 7 7 7 7
             3 6 4 6 3 6 4 6
             7 7 7 7 7 7 7 7
             5 6 5 6 5 6 5 6
             7 7 7 7 7 7 7 7)))
    (loop as n from 1 to 7 do (identity 123)
    )))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) paeth))
(defun paeth (a b c)
  (declare (optimize (safety 0) (speed 3)))
  ;; a = left, b = above, c = upper left
  (let* ((p  (f- (f+ a b) c))
         (pa (abs (f- p a)))
         (pb (abs (f- p b)))
         (pc (abs (f- p c))))
    ;; return nearest of a,b,c, breaking ties in order a,b,c
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(defun png-defilter (g s depth)
  ;(declare (optimize (safety 0) (speed 3)))
  (let* ((w (gfx-w g))
         (h (gfx-h g))
         (c  (f* (gfx-c g) (if (= depth 16) 2 1))) ;;hack
         (s (cons s 0))
         (ppb (if (= depth 16) 1 (f/ 8 depth))) ;;pixels per byte
         (l (* c (f/ (f+ w (f- ppb 1)) ppb)))
         (d (cons (vec (* h l) u1) 0)))
    (times y h
      (let* ((f  (% s))
             (p  (cons (%arr d) (%off d)))  ; prev pixel
             (u  (cons (%arr d) (f- (%off d) l)))  ; upper row
             (pu (cons (%arr u) (%off u)))) ; upper row, prev pixel
        (when (and (= y 0) (case f ((2 3 4) t)))
          (setf p (cons (vec l u1) 0)))
        (case f
          (0 (times _ l       (% d (% s)))) ; plaintext
          (1 (times _ c       (% d (% s))) ; sub
             (times _ (- l c) (% d (+ (% p) (% s)))))
          (2 (times _ l       (% d (+ (% u) (% s))))) ; up
          (3 (times _ c       (% d (+ (avg     0 (% u)) (% s)))) ; average
             (times _ (- l c) (% d (+ (avg (% p) (% u)) (% s)))))
          (4 (times _ c       (% d (+ (paeth     0 (% u)      0) (% s)))) ; paeth
             (times _ (- l c) (% d (+ (paeth (% p) (% u) (% pu)) (% s)))))
          (otherwise (gfx-load-error "Invalid line filter (~a)" f)))))
    (case depth
      ((1 2 4) ;; unpack bits into separate indices
       (let* ((s (%ptr (car d)))
              (m (f- (f<< 1 depth) 1)))
         (setf d (cons (vec (* h w) u1) 0))
         (times y h
           (let ((x 0))
             (while (< x w)
               (let ((b (% s))
                     (e (+ x (min (- w x) ppb)))
                     (sh (f* ppb depth)))
                 (while (< x e)
                   (decf sh depth)
                   (% d (logand (f>> b sh) m))
                   (incf x))))))))
      (16 (setf c (gfx-c g)) ;; conver 64-bit images to 32-bit
          (setf s (%ptr (car d)))
          (setf d (cons (vec (* h w c) u1) 0))
          (times y h
            (times n c
              (times x w
                (% d (% s)) ;;most-significant byte first
                (% s)
                ))))
      )
    (setf (gfx-d g)
          (let ((d (car d)))
            (cond ((= c 4) (bytes-to-colors (r g b a) (r g b (- #xFF a)) d))
                  (t (u1-u4 c d)))))))

(defun png-parse (s)
  (let ((chunks (make-hash-table :test 'equal)))
    (deser s (('+png-header+ raw 8)) :on-fail (gfx-load-error "Not a PNG file"))
    (loop (deser s ((len msb  4)
                    (tag utf8 4)
                    (xs  raw  len)
                    (crc msb  4))
            :on-fail (gfx-load-error "Truncated stream")
            (when (and (equal tag "IDAT") (gethash tag chunks))
              (setf xs (acat (gethash tag chunks) xs)))
            (when (equal tag "IEND") (return))
            (setf (gethash tag chunks) xs)))
    chunks))

(defun png-load (s)
  (setf s (png-parse s))
  (let* ((g nil)
         (c 0)
         (IHDR (gethash "IHDR" s))
         (IDAT (gethash "IDAT" s))
         (PLTE (gethash "PLTE" s))
         (tRNS (gethash "tRNS" s)))
    (unless IHDR (gfx-load-error "Missing IHDR"))
    (unless IDAT (gfx-load-error "Missing IDAT"))
    (deser IHDR ((width     msb 4)
                 (height    msb 4)
                 (depth     1) ; depth of color channel in bits: 1 2 4 8 16
                 (type      1)
                 (enc       1)
                 (filter    1)
                 (interlace 1))
      (unless (= enc 0) (gfx-load-error "Unsupported encoding (~a)" enc))
      (unless (= filter 0) (gfx-load-error "Unsupported filter method (~a)" filter))

      (setf c (case type
                (0 1) ; grayscale
                (2 3) ; truecolor
                (3 1) ; indexed
                (4 4) ; grayscale with alpha
                (6 4) ; truecolor with alpha
                (otherwise (gfx-load-error "Invalid color type (~d)" type))))

      (case depth
        ((1 2 4) (when (/= c 1)
                   (gfx-load-error "Invalid color type (~a) for depth ~a" type depth)))
        ((8 16) )
        (otherwise (gfx-load-error "Unsupported channel-depth (~a)" depth)))

      (setf g (gfx width height :c c))
      (png-defilter g (zlib:unpack IDAT) depth)
      (case interlace
        (0 ) ; no interlace
        (1 (gfx-load-error "Interlaced PNGs are not supported")) ;(png-deinterlace-adam7 g))
        (otherwise (gfx-load-error "Invalid interlace type (~d)" interlace)))
      (when (= c 1)
        (let ((m (if PLTE
                     (u1-u4 3 PLTE)
                     (sc u4 (loop as i below 256 collect (rgb i i i))))))
          (when tRNS (times i (length tRNS)
                       (w/rgb (r g b) (aref m i)
                         (setf (aref m i) (rgb r g b (- #xFF (aref tRNS i)))))))
          (setf (gfx-m g) m)))
      g)))



(defun make-crc-table ()
  (declare (optimize (safety 0) (speed 3)))
  (let ((v (vec 256 u4)))
    (dotimes (n 256)
      (let ((c n))
        (dotimes (k 8)
          (declare (u4 n) (u4 c) (u4 k))
          (setf c (if (/= (logand c 1) 0)
                      (uxor #xEDB88320 (ash c -1))
                      (u>> c 1))))
        (setf (aref v n) c)))
    v))

(defvar *crc-table* (make-crc-table))
(declaim ((SIMPLE-ARRAY u4 (256)) *crc-table*))

(declaim (ftype (function (u4 (SIMPLE-ARRAY u1 (*))) u4) update-crc))
(defun update-crc (crc bs)
  (declare (optimize (safety 0) (speed 3))
           (u4 crc))
  (loop as b across bs do
       (setf crc (uxor (aref *crc-table* (uand (uxor crc b) #xFF))
                       (u>> crc 8))))
  crc)

(defun calc-crc (bs)
  (declare (optimize (safety 0) (speed 3)))
  (uxor (update-crc #xFFFFFFFF bs) #xFFFFFFFF))



(defun png-filter (w h c d)
  (declare (optimize (safety 0) (speed 3)))
  (let* ((p (%new (f* h (f+ 1 (f* w c)))))
         (q (%ptr d)))
    (times y h
      (% p 0)
      (times x (f* w c)
        (% p (% q))))
    (car p)))


(defmacro png-chunk (o name data)
  (with-gensyms (d b)
    `(let* ((,d ,data)
            (,b (ser t (tag utf8 ,name)
                       (body arr 1 ,d))))
       (ser ,o
         (len msb 4 (length ,d))
         (tag arr 1 ,b)
         (crc msb 4 (calc-crc ,b))))))

;(save-file "tmp/1.png" (png-create (png-load "tmp/test.png")))
(defun png-make (g)
  (bind-struct gfx g (w h c d m)
    (let* ((o (%new))
           (d  (cond ((= c 4) (colors-to-bytes (r g b a) (r g b (u- #xFF a)) d))
                     ((= c 3) (colors-to-bytes (r g b) (r g b) d))
                     (t (u4-u1 1 d)))))
      (ser o (magic arr 1 +png-header+))
      (png-chunk o "IHDR"
        (ser t
          (width  msb 4 w)
          (height msb 4 h)
          (depth     1 8)
          (type 1 (case c
                    (1 3) ; indexed
                    (3 2) ; truecolor
                    (4 6) ; truecolor with alpha
                    (otherwise (error "png-make: cant save this image type"))
                    ))
          (enc       1 0) ; compression: 0=zlib
          (filter    1 0) ; pre-compression-filter: 0=default
          (interlace 1 0)))
      (when (= c 1)
        (png-chunk o "PLTE" (u4-u1 3 m))
        (let ((tRNS (vec 256 u1))
              (emit nil))
          (times i 256
            (w/rgb (_ _ _ a) (aref m i)
              (setf (aref tRNS i) (u- #xFF a))
              (when (/= a 0) (setf emit t))))
          (when emit (png-chunk o "tRNS" tRNS))))
      (png-chunk o "IDAT" (zlib:pack (png-filter w h c d)))
      (png-chunk o "IEND" #())
      (%crop o)
      )))


(gfx-set-handler "png" png-load png-make)

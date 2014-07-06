(in-package :gfx)

(defun pcx-derle-line (o s e)
  (while (< (cdr o) e)
    (let ((c (% s)))
      (if (/= (logand c #xC0) #xC0)
          (% o c)
          (let ((c (logxor c #xC0))
                (v (% s)))
            (while (plusp c)
              (% o v)
              (decf c)))))))

(defun pcx-load (s)
  (deser s
      ((Manufacturer  1) ; 10 = ZSoft
       (Version       1) ; 0 = Paintbrush v2.5
                         ; 2 = Paintbrush v2.8 w palette
                         ; 3 = Paintbrush v2.8 w/o palette
                         ; 4 = Paintbrush for Windows
                         ; 5 = Paintbrush v3.0+
       (Encoding      1) ; 1 = RLE
       (BPP           1)
       (XMin          2)
       (YMin          2)
       (XMax          2)
       (YMax          2)
       (HDPI          2)
       (VDPI          2)
       (ColorMap raw 48)
       (Reserved      1) ; set to 0
       (NPlanes       1) ; number of color planes (like R, G and B)
       (BytesPerLine  2)
       (PaletteInfo   2) ; 1 = Color/BW, 2 = Grayscale
       (HScreenSize   2)
       (VScreenSize   2)
       (Filler   raw 54) ; set to 0
       )
    :on-fail (gfx-load-error "Truncated stream")
    (unless (= Encoding 1)
      (gfx-load-error "Unsupported encoding (~a)" Encoding))
    (unless (case NPlanes ((1 3) t))
      (gfx-load-error "Invalid number of planes (~a)" NPlanes))
    (let* ((w (- XMax XMin -1))
           (h (- YMax YMin -1))
           (g (gfx w h :c NPlanes))
           (o (cons (vec (* w h NPlanes) u1) 0)))
      (times y h (pcx-derle-line o s (+ (* w NPlanes) (cdr o))))
      (setf (gfx-d g) (let ((d (u1-u4 NPlanes (car o) :w w)))
                        (cond ((= NPlanes 3) (bytes-to-colors (r g b) (r g b) (u4-u1 NPlanes d)))
                              ((= NPlanes 4) (bytes-to-colors (r g b a) (r g b a) (u4-u1 NPlanes d)))
                              (t d))))
      g)))

(defun pcx-rle-line (o s e)
  (while (< (cdr s) e)
    (let ((v (% s))
          (c 1))
      (while (and (< (cdr s) e) (= (% 0 s) v) (< c 63))
        (% s)
        (incf c))
      (unless (and (= c 1) (/= (logand #xC0 v) #xC0))
        (ser o (Count 1 (logior #xC0 c))))
      (ser o (Value 1 v)))))

(defun pcx-make (g)
  (bind-struct gfx g (w h c d m)
    (let* ((o (%new))
           (d (%ptr (u4-u1 c
                           (if (= c 1) d (colors-to-colors (r g b a) (cw b g r a) (copy-seq d)))
                           :w w))))
      (ser o (Manufacturer  1 10) ; 10 = ZSoft
             (Version       1 5) ; 5 = Paintbrush v3.0+
             (Encoding      1 1) ; 1 = RLE
             (BPP           1 8)
             (XMin          2 0)
             (YMin          2 0)
             (XMax          2 (- w 1))
             (YMax          2 (- h 1))
             (HDPI          2 300)
             (VDPI          2 300)
             (ColorMap dup 48 1 0)
             (Reserved      1 0) ; set to 0
             (NPlanes       1 c) ; number of color planes (like R, G and B)
             (BytesPerLine  2 w) ; must be aligned by two?
             (PaletteInfo   2 1) ; 1 = Color/BW, 2 = Grayscale
             (HScreenSize   2 0)
             (VScreenSize   2 0)
             (Filler   dup 54 1 0) ; set to 0
             )
      (dotimes (y h)
        (pcx-rle-line o d (+ (* w c) (cdr d))))
      (when (/= (length m) 0)
        (ser o (Magic       1 #x0C)
               (Palette arr 1 (colors-to-bytes (r g b) (r g b) m))))
      (%crop o))))

(gfx-set-handler "pcx" pcx-load pcx-make)

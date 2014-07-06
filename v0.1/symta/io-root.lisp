(in-package :symta)


;; fold bytes of specified encoding into string
(defun fold-enc (seq &key (encoding :utf-8))
  ;; use (list-character-encodings) to get available ones
  (let ((*default-character-encoding* encoding)
        (l (sq-to-list seq)))
    (unless (every (fn (x) (and (numberp x) (>= x 0) (< x 256))) l)
      (return-from fold-enc ""))
    (octets-to-string (coerce l '(vector (unsigned-byte 8))))))

;; unfold string into bytes of specified encoding
(defun unfold-enc (s &key (encoding :utf-8))
  (unless (str? s) (return-from unfold-enc nil))
  (let ((*default-character-encoding* encoding)
        (s (sq-to-str s)))
    (list-to-sq (coerce (string-to-octets s) 'list))))




(defmacro fn-k (args &rest exprs)
  `(fn (k ,@args) (funcall k (progn ,@exprs))))

(defparameter kleene-wildcard (fn-k (x) 1))

;; Kleene Star. Now used only in lexer.
(defstruct kleene (value kleene-wildcard))
(defun kleene? (x) (if (typep x 'kleene) x))
(defun kleene (v) (make-kleene :value v))


(defun symbol-char? (ch)
  (if (or (alpha? ch) (digit? ch) (eql #\_ ch) (eql #\? ch))
      ch))
(defun sym-hd (k x) (funcall k (or (alpha? x) (eql x #\_))))
(defun sym-ch (k x) (funcall k (symbol-char? x)))
(defun digit  (k x) (funcall k (digit? x)))
(defun symp   (k x) (funcall k (sym? x)))


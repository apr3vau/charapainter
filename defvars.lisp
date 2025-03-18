;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defparameter *version* (asdf:system-version (asdf:find-system :charapainter)))

(defparameter *settings-file*
  (merge-pathnames "charapainter.sexp" (sys:get-folder-path :appdata)))

(defparameter *default-font-family*
  #+mswindows "Courier New"
  #+darwin "Menlo"
  #+linux "Liberation Mono")
(defparameter *default-font-size* 16)
(defvar *default-background* (color:make-rgb 0 0 0))
(defvar *default-foreground* (color:make-rgb 1 1 1))

(defparameter *undo-ring-size* 50)

(defparameter *privacy-policy*
  (read-file-into-string
   (merge-pathnames "privacy-policy.txt" (asdf:system-source-directory :charapainter))))

(defparameter *all-tools-names*
  '(pan brush eraser stroke rectangle picker select import-image))

(defclass need-invalidate-after-style-change () ())

(make-face 'hl-background
           :background :color_highlight
           :if-exists :overwrite)

;; Resources

(defun find-resource-icon (name size)
  (car (directory (merge-pathnames (format nil "res/icons8-~A-~A.png" (string-downcase name) size)
                                   (asdf:system-source-directory :charapainter)))))

(loop for sym in '(brush eraser stroke rectangle pan select import-image picker
                         settings visible invisible up down add remove)
      for name in '("brush" "erase" "squiggly-line" "square" "hand" "select-all" "import" "color-dropper"
                    "setting" "visible" "invisible" "up" "down" "add" "remove")
      do (setf (get sym :data) (read-file-into-byte-vector (find-resource-icon name 48))))

(setf (get 'logo :data) (read-file-into-byte-vector
                         (merge-pathnames "res/cp.png"
                                          (asdf:system-source-directory :charapainter))))

(dolist (sym (append *all-tools-names*
                     '(logo settings visible invisible up down add remove)))
  (when-let (data (get sym :data))
    (gp:register-image-translation sym (make 'gp:external-image :data data :type :png))))

(defun set-variables-default ()
  (setq capi:*editor-cursor-active-style* :inverse
        *undo-ring-size* 50
        *default-background* (color:make-rgb 0 0 0)
        *default-foreground* (color:make-rgb 1 1 1)))

(defun save-settings ()
  (with-open-file (out *settings-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (prin1 (loop for sym in '(capi:*editor-cursor-active-style* *undo-ring-size*)
                 collect sym
                 collect (symbol-value sym))
           out)))

(defun load-settings ()
  (when (probe-file *settings-file*)
    (handler-case
        (let* ((*read-eval* nil)
               (plist (with-open-file (in *settings-file*) (read in))))
          (loop for (sym val) in plist
                do (set sym val)))
      (error (e)
        (delete-file *settings-file*)
        (set-variables-default)))))

;; Character and character sets for stroke and rectangle

(defvar *char-board-columns* 16)

(defparameter *characters*
  (string-append
   ".,/\\|-=_+*~`'\";:"
   "!@#$%^&()[]{}<> "
   "─│┌┐┍┑┎┒┏┓╒╕╓╖╔╗"
   "━┃└┘┕┙┖┚┗┛╘╛╙╜╚╝"
   "├┤┝┥┞┦┟┧┠┨┡┩┢┪┣┫"
   "┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋"
   "╭╮┬┮┭┰┲┯┱┳╥╤╦╞╪╡"
   "╰╯┴┶┵┸┺┷┹┻╨╧╩╟╫╢"
   "┄┆┅┇┈┊┉┋╷╻╽╼╾╠╬╣"
   "╌╎╍╏═║╱╲╵╹╿╶╴╺╸╳"
   "▀▁▂▃▄▅▆▇█▉▊▋▌▍▎▏"
   "▐░▒▓▔▕▖▗▘▙▚▛▜▝▞▟"
   "↖↑↗⇖⇑⇗ ⇈  ⇡ ⇔⇕↔↕"
   "← →⇐ ⇒⇇ ⇉⇠ ⇢⇄⇅⇆⇋"
   "↙↓↘⇙⇓⇘ ⇊  ⇣ ⇌⇍⇎⇏"
   "¡¿¢£¤¥©®·¹²³°±§¶"))

(defvar *charsets*
  '(:ascii (:lt #\+ :rt #\+ :lb #\+ :rb #\+ :lrt #\+ :lrb #\+ :tbl #\+ :tbr #\+ :cross #\+ :h #\- :v #\|
            :arr-l #\< :arr-r #\> :arr-t #\^ :arr-b #\v)
    :box-light (:lt #\┘ :rt #\└ :lb #\┐ :rb #\┌ :lrt #\┴ :lrb #\┬ :tbl #\┤ :tbr #\├ :cross #\┼ :h #\─ :v #\│
                :arr-l #\← :arr-t #\↑ :arr-r #\→ :arr-b #\↓)
    :box-heavy (:lt #\┛ :rt #\┗ :lb #\┓ :rb #\┏ :lrt #\┻ :lrb #\┳ :tbl #\┫ :tbr #\┣ :cross #\╋ :h #\━ :v #\┃
                :arr-l #\← :arr-t #\↑ :arr-r #\→ :arr-b #\↓)
    :box-double (:lt #\╝ :rt #\╚ :lb #\╗ :rb #\╔ :lrt #\╩ :lrb #\╦ :tbl #\╣ :tbr #\╠ :cross #\╬ :h #\═ :v #\║
                 :arr-l #\⇇ :arr-t #\⇈ :arr-r #\⇉ :arr-b #\⇊)
    :box-rounded (:lt #\╯ :rt #\╰ :lb #\╮ :rb #\╭ :lrt #\┴ :lrb #\┬ :tbl #\┤ :tbr #\├ :cross #\┼ :h #\─ :v #\│
                  :arr-l #\← :arr-t #\↑ :arr-r #\→ :arr-b #\↓)))

(defun charset-get (charset-name char-name)
  (declare (inline charset-get))
  (getf (getf *charsets* charset-name) char-name))

(defun charset-member (char charset-name)
  (declare (inline charset-member))
  (member char (getf *charsets* charset-name)))

;; Colors

(defstruct term-color bit code spec (alpha 1))

(defvar *default-4-bit-colors*
  (make-array
   16
   :initial-contents
   (with-collector (c)
     (dolist* (i spec (list (color:make-rgb 0   0   0)
                            (color:make-rgb 2/3 0   0)
                            (color:make-rgb 0   2/3 0)
                            (color:make-rgb 2/3 1/3 0)
                            (color:make-rgb 0   0   2/3)
                            (color:make-rgb 2/3 0   2/3)
                            (color:make-rgb 0   2/3 2/3)
                            (color:make-rgb 2/3 2/3 2/3)))
       (c (make-term-color :bit 4 :code (+ i 30) :spec spec)))
     (dolist* (i spec (list (color:make-rgb 1/3 1/3 1/3)
                            (color:make-rgb 1   1/3 1/3)
                            (color:make-rgb 1/3 1   1/3)
                            (color:make-rgb 1   1   1/3)
                            (color:make-rgb 1/3 1/3 1)
                            (color:make-rgb 1   1/3 1)
                            (color:make-rgb 1/3 1   1)
                            (color:make-rgb 1   1   1)))
       (c (make-term-color :bit 4 :code (+ i 90) :spec spec))))))

#|(defun load-iterm-theme (input)
  (let* ((root (plump-parser:parse input))
         (vector (plump-dom:children
                  (aref (plump-dom:children
                         (plump-dom:get-element-by-id "plist"))
                        0))))
    (dorange$fixnum (i 0 (length vector) 2)
      (let ((key (aref vector i))
            (dict (aref vector (1+ i))))
        ))))|#

(defvar *4-bit-colors*
  (make-array
   16
   :initial-contents
   (with-collector (c)
     (dolist* (i spec (list (color:make-rgb 0   0   0)
                            (color:make-rgb 2/3 0   0)
                            (color:make-rgb 0   2/3 0)
                            (color:make-rgb 2/3 1/3 0)
                            (color:make-rgb 0   0   2/3)
                            (color:make-rgb 2/3 0   2/3)
                            (color:make-rgb 0   2/3 2/3)
                            (color:make-rgb 2/3 2/3 2/3)))
       (c (make-term-color :bit 4 :code (+ i 30) :spec spec)))
     (dolist* (i spec (list (color:make-rgb 1/3 1/3 1/3)
                            (color:make-rgb 1   1/3 1/3)
                            (color:make-rgb 1/3 1   1/3)
                            (color:make-rgb 1   1   1/3)
                            (color:make-rgb 1/3 1/3 1)
                            (color:make-rgb 1   1/3 1)
                            (color:make-rgb 1/3 1   1)
                            (color:make-rgb 1   1   1)))
       (c (make-term-color :bit 4 :code (+ i 90) :spec spec))))))

(defvar *8-bit-colors* (make-array 256))

(dolist* (index spec (list (color:make-rgb 0   0   0)
                           (color:make-rgb 2/3 0   0)
                           (color:make-rgb 0   2/3 0)
                           (color:make-rgb 2/3 1/3 0)
                           (color:make-rgb 0   0   2/3)
                           (color:make-rgb 2/3 0   2/3)
                           (color:make-rgb 0   2/3 2/3)
                           (color:make-rgb 2/3 2/3 2/3)
                           (color:make-rgb 1/3 1/3 1/3)
                           (color:make-rgb 1   1/3 1/3)
                           (color:make-rgb 1/3 1   1/3)
                           (color:make-rgb 1   1   1/3)
                           (color:make-rgb 1/3 1/3 1)
                           (color:make-rgb 1   1/3 1)
                           (color:make-rgb 1/3 1   1)
                           (color:make-rgb 1   1   1)))
  (setf (aref *8-bit-colors* index)
        (make-term-color :bit 8 :code index :spec spec)))

(dotimes (r 6)
  (dotimes (g 6)
    (dotimes (b 6)
      (let ((index (+ (* 36 r) (* 6 g) b 16))
            (spec (color:make-rgb (/ r 5) (/ g 5) (/ b 5))))
        (setf (aref *8-bit-colors* index)
              (make-term-color :bit 8 :code index :spec spec))))))

(loop for index :from 232
      for level :from 1/32 :to 119/128 :by 5/128
      do (setf (aref *8-bit-colors* index)
               (make-term-color :bit 8 :code index :spec (color:make-gray level))))

(defun code-color (bit code &optional (alpha 1))
  (let ((color (case bit
                 (4 (copy-term-color (find code *4-bit-colors* :key #'term-color-code)))
                 (8 (copy-term-color (aref *8-bit-colors* code)))
                 (24 (destructuring-bind (r g b) code
                       (make-term-color :bit 24 :code code
                                        :spec (color:make-rgb (/ r 255) (/ g 255) (/ b 255))))))))
    (setf (term-color-alpha color) alpha)
    color))

(defun term-color-spec-with-alpha (color)
  (declare (inline term-color-spec-with-alpha))
  (color:color-with-alpha (term-color-spec color) (term-color-alpha color)))

(defun term-color-from-spec (spec)
  (make-term-color :bit 24 :spec spec
                   :code (mapcar (op (round (* 255 _)))
                                 (list (color:color-red spec)
                                       (color:color-green spec)
                                       (color:color-blue spec)))))

(defgeneric coerce-color-to (bit color &optional alpha)
  (:method :around (bit color &optional alpha)
   (when color (call-next-method)))
  (:method ((bit (eql 4)) color &optional (alpha 1 alpha-supplied-p))
   (when (term-color-p color)
     (unless alpha-supplied-p
       (setq alpha (term-color-alpha color)))
     (setq color (term-color-spec color)))
   (setq color (color:ensure-rgb color))
   (flet ((color-deviation (c1 c2)
            (let ((r1 (color:color-red c1))
                  (g1 (color:color-green c1))
                  (b1 (color:color-blue c1))
                  (r2 (color:color-red c2))
                  (g2 (color:color-green c2))
                  (b2 (color:color-blue c2)))
              (+ (abs (- r1 r2)) (abs (- g1 g2)) (abs (- b1 b2))))))
     (let ((found (copy-term-color
                   (reduce (lambda (c1 c2)
                             (if (< (color-deviation color (term-color-spec c1))
                                    (color-deviation color (term-color-spec c2)))
                               c1 c2))
                           *4-bit-colors*))))
       (setf (term-color-alpha found) alpha)
       found)))
  (:method ((bit (eql 8)) color &optional (alpha 1 alpha-supplied-p))
   (when (term-color-p color)
     (unless alpha-supplied-p
       (setq alpha (term-color-alpha color)))
     (setq color (term-color-spec color)))
   (if (< (standard-deviation (cdr (coerce color 'list)))
          0.01)
     (progn
       (setq color (color:ensure-gray color))
       (let ((level (round (color:color-level color) 1/24)))
         (make-term-color :bit 8 :code (+ level 232) :spec color)))
     (progn
       (setq color (color:ensure-rgb color))
       (let ((r (round (color:color-red color) 1/5))
             (g (round (color:color-green color) 1/5))
             (b (round (color:color-blue color) 1/5)))
         (make-term-color :bit 8 :code (+ (* r 36) (* g 6) b 16) :alpha alpha
                          :spec (color:make-rgb (/ r 5) (/ g 5) (/ b 5)))))))
  (:method ((bit (eql 24)) color &optional (alpha 1 alpha-supplied-p))
   (when (term-color-p color)
     (unless alpha-supplied-p
       (setq alpha (term-color-alpha color)))
     (setq color (term-color-spec color)))
   (setq color (color:ensure-rgb color))
   (let ((r (round (color:color-red color) 1/255))
         (g (round (color:color-green color) 1/255))
         (b (round (color:color-blue color) 1/255)))
     (make-term-color :bit 24 :code (list r g b) :alpha alpha :spec color))))
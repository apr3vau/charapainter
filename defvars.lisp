;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defparameter *version* (asdf:system-version (asdf:find-system :charapainter)))
(defparameter *settings-file*
  (merge-pathnames "charapainter.fasl" (sys:get-folder-path :appdata)))

(defparameter *font-family*
  #+mswindows "Courier New"
  #+darwin "Menlo"
  #+linux "Liberation Mono")
(defparameter *font-size* 16)
(defvar *default-background* (color:make-rgb 0 0 0))
(defvar *default-foreground* (color:make-rgb 1 1 1))
(defvar *fdesc*)
(defvar *fg* nil)
(defvar *bg* nil)
(defvar *char* #\.)

(defvar *char-width*)
(defvar *char-height*)
(defvar *char-board-grid-width*)
(defvar *char-board-grid-height*)
(defvar *char-board-width*)
(defvar *char-board-height*)

(defparameter *board-width* 80)
(defparameter *board-height* 32)
(defparameter *undo-ring-size* 50)

(defparameter *selection-border-char* #\§)
(defparameter *selection-border-foreground* :black)
(defparameter *selection-border-background* :white)

(defparameter *motd*
  (read-file-into-string
   (merge-pathnames "motd.txt" (asdf:system-source-directory :charapainter))))
(defparameter *privacy-policy*
  (read-file-into-string
   (merge-pathnames "privacy-policy.txt" (asdf:system-source-directory :charapainter))))

(defparameter *all-tools-names*
  '(pan brush eraser coloring picker stroke rectangle select import-image))

;; Resources

(defun find-resource-icon (name size)
  (car (directory (merge-pathnames (format nil "res/icons8-~A-~A.png" (string-downcase name) size)
                                   (asdf:system-source-directory :charapainter)))))

(loop for sym in '(brush eraser stroke rectangle pan coloring select import-image picker)
      for name in '("brush" "erase" "squiggly-line" "square" "hand" "paint-roller" "select-all" "import" "color-dropper")
      do (setf (get sym :data) (read-file-into-byte-vector (find-resource-icon name 48))))

(setf (get 'settings :data) (read-file-into-byte-vector (find-resource-icon "setting" 48)))
(setf (get 'logo :data) (read-file-into-byte-vector
                         (merge-pathnames "res/cp.png"
                                          (asdf:system-source-directory :charapainter))))
(defun set-variables-default ()
  (setq capi:*editor-cursor-active-style* :inverse
        *font-family*
        #+mswindows "Courier New"
        #+darwin "Menlo"
        #+linux "Liberation Mono"
        *font-size* 16
        *board-width* 80
        *board-height* 32
        *undo-ring-size* 50
        *default-background* (color:make-rgb 0 0 0)
        *default-foreground* (color:make-rgb 1 1 1)
        *selection-border-char* #\$
        *selection-border-foreground* :black
        *selection-border-background* :white))

(defun save-settings ()
  (dump-forms-to-file
   *settings-file*
   `((setq ,@(iter (for sym in '(capi:*editor-cursor-active-style*
                                 *font-family* *font-size* *board-width* *board-height* *undo-ring-size*
                                 *selection-border-char* *selection-border-background* *selection-border-foreground*))
                   (nconcing (list sym (symbol-value sym))))))))

(defun load-settings ()
  (when (probe-file *settings-file*)
    (load-data-file *settings-file*)))

(defun set-variables ()
  (load-settings)
  (let ((dummy (capi:create-dummy-graphics-port)))
    (setf *fdesc* (gp:make-font-description :family *font-family* :size *font-size*)
          *char-width* (gp:get-font-width dummy (gp:find-best-font dummy *fdesc*))
          *char-height* (gp:get-font-height dummy (gp:find-best-font dummy *fdesc*))
          *char-board-grid-width* (+ *char-width* 2)
          *char-board-grid-height* (+ *char-height* 2)
          *char-board-width* (* *char-board-grid-width* (length (aref *characters* 0)))
          *char-board-height* (* *char-board-grid-height* (length *characters*)))
    (dolist (sym (append *all-tools-names* '(settings logo)))
      (when-let (data (get sym :data))
        (gp:register-image-translation sym (make 'gp:external-image :data data))))))

;; Character and character sets for stroke and rectangle

(defparameter *characters*
  #(".,/\\|-=_+*~`'\";:"
    "+!@#$%^&()[]{}<>"
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

(defstruct term-color bit code spec)

(defun term-color= (color1 color2)
  (declare (inline term-color=))
  (and color1 color2
       (eql (term-color-bit color1) (term-color-bit color2))
       (equal (term-color-code color1) (term-color-code color2))))

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

(iter (for index :from 232)
      (for level :from 1/32 :to 119/128 :by 5/128)
      (setf (aref *8-bit-colors* index)
            (make-term-color :bit 8 :code index :spec (color:make-gray level))))

(defun code-color (bit code)
  (case bit
    (4 (find code *4-bit-colors* :key #'term-color-code))
    (8 (aref *8-bit-colors* code))
    (24 (destructuring-bind (r g b) code
          (make-term-color :bit 24 :code code
                           :spec (color:make-rgb (/ r 255) (/ g 255) (/ b 255)))))))

(defgeneric coerce-color-to (bit color)
  (:method ((bit (eql 4)) color)
   (when (term-color-p color)
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
     (iter (for c :in-vector *4-bit-colors*)
           (finding c :minimizing (color-deviation color (term-color-spec c))))))
  (:method ((bit (eql 8)) color)
   (when (term-color-p color)
     (setq color (term-color-spec color)))
   (if (< (standard-deviation (cdr (coerce color 'list)))
          0.01)
     (progn
       (setq color (color:ensure-gray color))
       (let ((level (round (color:color-level color) 1/24)))
         (make-term-color :bit 8 :code (+ level 232) :spec color)))
     (progn
       (setq color (color:ensure-rgb color))
       (let ((r (round (color:color-red color) 1/6))
             (g (round (color:color-green color) 1/6))
             (b (round (color:color-blue color) 1/6)))
         (make-term-color :bit 8 :code (+ (* r 36) (* g 6) b)
                          :spec (color:make-rgb (/ r 6) (/ g 6) (/ b 6)))))))
  (:method ((bit (eql 24)) color)
   (when (term-color-p color)
     (setq color (term-color-spec color)))
   (setq color (color:ensure-rgb color))
   (let ((r (round (color:color-red color) 1/255))
         (g (round (color:color-green color) 1/255))
         (b (round (color:color-blue color) 1/255)))
     (make-term-color :bit 24 :code (list r g b) :spec color))))
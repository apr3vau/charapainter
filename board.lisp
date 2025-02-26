;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defun change-cursor-movement (board data)
  (with-slots (cursor-movement-option) (capi:element-interface board)
    (setf (slot-value board 'cursor-movement) data)
    (setf (capi:choice-selected-item cursor-movement-option) data)))

(defun board-recenter (board)
  (let* ((point (buffer-point (capi:editor-pane-buffer board)))
         (x (point-column point))
         (y (point-linenum point))
         (cx (floor *board-width* 2))
         (cy (floor *board-height* 2)))
    (move-board board (cons (- x cx) (- y cy)))))

(defun move-board (board amount)
  (with-slots (x-offset y-offset) board
    (destructuring-bind (ox . oy) amount
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x (point-column point))
             (y (point-linenum point)))
        (incf x-offset ox)
        (incf y-offset oy)
        (refresh-board board)
        (move-cursor-relative board (- x ox) (- y oy))))))

(defclass board (capi:editor-pane)
  ((project :initform (make-project)
            :accessor board-project)
   (saved :initform t)
   (undo-ring :initform (make-ring *undo-ring-size* 'charapainter-undo-ring)
              :accessor board-undo-ring)
   (current-tool :initform nil
                 :accessor board-current-tool)
   (x-offset :initform 0)
   (y-offset :initform 0)
   (cursor-movement :initform :right)
   (selected-pixels :initform nil)
   (drawing-pending-p :initform nil))
  (:default-initargs
   :font *fdesc*
   :buffer (make-buffer "charapainter" :temporary t :flag :charapainter
                        :contents (string-join (mapcar (op (subseq (pad-end _ *board-width*) 0 *board-width*))
                                                       (split-sequence '(#\Newline) *motd*))
                                               #\Newline))
   :foreground *default-foreground*
   :background *default-background*
   :visible-min-width (list 'character (- *board-width* 3))
   :visible-min-height (list 'character *board-height*)
   :visible-max-width t
   :visible-max-height t
   :wrap-style nil
   :line-wrap-marker nil
   :line-wrap-face nil
   :vertical-scroll nil
   :input-model
   `(((:button-1 :press)               ,(lambda (pane x y)     (tool-press (board-current-tool pane) x y)))
     ((:button-1 :release)             ,(lambda (pane x y)     (tool-release (board-current-tool pane) x y)))
     ((:button-1 :motion)              ,(lambda (pane x y)     (tool-drag (board-current-tool pane) x y)))
     ((:button-1 :press :shift)        ,(lambda (pane x y)     (tool-shift-press (board-current-tool pane) x y)))
     ((:button-1 :motion :shift)       ,(lambda (pane x y)     (tool-shift-drag (board-current-tool pane) x y)))
     ((:button-1 :release :shift)      ,(lambda (pane x y)     (tool-release (board-current-tool pane) x y)))
     (:motion                          ,(lambda (pane x y)     (tool-motion (board-current-tool pane) x y)))
     ((:gesture-spec :left)            ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :right)           ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :up)              ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :down)            ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\Return)         ,(lambda (pane x y key) (tool-return (board-current-tool pane) x y key)))
     ((:gesture-spec #\Backspace)      ,(lambda (pane x y key) (tool-backspace (board-current-tool pane) x y key)))
     ((:gesture-spec #\Escape)         ,(lambda (pane x y key) (tool-escape (board-current-tool pane) x y key)))
     
     ((:gesture-spec #\f :control)     ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\b :control)     ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\p :control)     ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\n :control)     ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\Space :control) ,(lambda (pane x y key) (tool-ctrl-space (board-current-tool pane) x y key)))
     ((:gesture-spec #\a :control)     ,(lambda (pane x y key) (tool-line-start (board-current-tool pane) x y key)))
     ((:gesture-spec #\e :control)     ,(lambda (pane x y key) (tool-line-end (board-current-tool pane) x y key)))
     ((:gesture-spec #\l :control)     ,(lambda (pane x y key) (declare (ignore x y key)) (board-recenter pane)))
     
     ((:gesture-spec :left #+darwin :hyper #-darwin :control)  ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(-1 . 0))))
     ((:gesture-spec :right #+darwin :hyper #-darwin :control) ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(1 . 0))))
     ((:gesture-spec :up #+darwin :hyper #-darwin :control)    ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(0 . -1))))
     ((:gesture-spec :down #+darwin :hyper #-darwin :control)  ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(0 . 1))))
     
     (:gesture-spec                    ,(lambda (pane x y key) (tool-key (board-current-tool pane) x y key))))))

(defmethod board-undo (itf)
  (tool-cleanup @itf.board.current-tool)
  (paint-pixels @itf.board (ring-pop @itf.board.undo-ring)))

(defmethod capi:pane-interface-undo-p ((board board) itf)
  (with-slots (undo-ring) board
    (plusp (ring-length undo-ring))))

(defmethod capi:pane-interface-cut-p ((board board) itf)
  @board.selected-pixels)

(defun board-cut (itf)
  (with-slots (board) itf
    (with-slots (current-tool project selected-pixels undo-ring) board
      (let ((pixels (or selected-pixels (project-pixels project))))
        (capi:set-clipboard
         board
         (copy-pixels pixels)
         (with-output-to-string (out)
           (string-case (capi:choice-selected-item (slot-value itf 'copy-option))
             ("Escaped sequence" (export-to-ansi pixels out))
             ("HTML"             (export-to-html pixels out))
             ("Plain text"       (export-to-plain-text pixels out))))
         (list :image (export-to-image board pixels)))
        (if selected-pixels
          (typecase current-tool
            (select (with-slots (pixels) current-tool
                      (ring-push pixels undo-ring)
                      (tool-cleanup current-tool)))
            (import-image (with-slots (old-tool) current-tool
                            (tool-cleanup current-tool)
                            (set-tool itf board old-tool))))
          (progn
            (ring-push (project-pixels project) undo-ring)
            (setf (project-pixels project) (make-pixels))
            (refresh-board board)))))))

(defmethod capi:pane-interface-copy-p ((board board) itf)
  (with-slots (selected-pixels project) board
    (or selected-pixels (pixels-not-empty-p (project-pixels project)))))

(defun board-copy (itf)
  (with-slots (board) itf
    (with-slots (project selected-pixels) board
      (let ((pixels (or selected-pixels (project-pixels project))))
        (capi:set-clipboard
         board
         (copy-pixels pixels)
         (with-output-to-string (out)
           (string-case (capi:choice-selected-item (slot-value itf 'copy-option))
             ("Escaped sequence" (export-to-ansi pixels out))
             ("HTML"             (export-to-html pixels out))
             ("Plain text"       (export-to-plain-text pixels out))))
         (list :image (export-to-image board pixels)))))))

(defmethod capi:pane-interface-paste-p ((board board) itf)
  (or (capi:clipboard board :string)
      (capi:clipboard board :value)
      (capi:clipboard board :image)))

(defun board-paste (itf)
  (with-slots (board tools) itf
    (with-slots (selected-pixels current-tool x-offset y-offset) board
      (acond
        ((capi:clipboard board :value)
         (when (pixels-p it)
           (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                  (x0 (+ x-offset (point-column point)))
                  (y0 (+ y-offset (point-linenum point)))
                  (new (make-pixels)))
             (with-pixels-boundary it
               (loop-pixels it
                 (add-pixel (+ (- %x %left) x0) (+ (- %y %top) y0) %pixel new)))
             (let ((tool (find 'select tools :key (op (class-name (class-of _))))))
               (if (eq @board.current-tool tool)
                 (tool-return tool nil nil nil)
                 (tool-cleanup tool))
               (set-tool-internal itf board tool)
               (setf @tool.pixels (copy-pixels new)))
             (set-selected-pixels board new)
             (move-cursor board (pixels-left new) (pixels-top new)))))
        ((capi:clipboard board :image)
         (let ((tool (find 'import-image tools :key (op (class-name (class-of _))))))
           (with-slots (image dx dy old-tool width height) tool
             (let* ((w (gp:image-width it))
                    (h (gp:image-height it))
                    (ratio (min (/ (* 7/8 *board-width*) w)
                                (/ (* 7/8 *board-height*) h))))
               (setf image it
                     dx (1+ x-offset)
                     dy (1+ y-offset)
                     width (floor (* w (* ratio 2)))
                     height (floor (* h ratio))
                     old-tool (class-name (class-of current-tool)))
               (set-tool-internal itf board tool)
               (refresh-import-image tool)))))
        ((capi:clipboard board :string)
         (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                (x0 (+ x-offset (point-column point)))
                (y0 (+ y-offset (point-linenum point)))
                (x x0) (y y0)
                (new (make-pixels)))
           (map nil (lambda (c)
                      (if (member c '(#\Return #\Newline))
                        (setq x x0 y (1+ y))
                        (progn
                          (add-pixel x y (make-pixel :char c :fg *fg* :bg *bg*) new)
                          (incf x))))
                it)
           (setf selected-pixels new)
           (let ((tool (find 'select tools :key (op (class-name (class-of _))))))
             (setf (slot-value tool 'pixels) (make-pixels))
             (set-tool-internal itf board tool))))))))

(defmethod capi:pane-interface-paste-object ((board board) itf)
  (board-paste itf))

(defstruct project (name "Unnamed") (pixels (make-pixels)))

(defun get-pixel (x y board)
  (declare (inline get-pixel))
  (find-pixel x y (project-pixels (board-project board))))

(defun draw-a-pixel (board x y pixel)
  (with-slots (x-offset y-offset) board
    (when (and (<= x-offset x (+ x-offset *board-width* -1))
               (<= y-offset y (+ y-offset *board-height* -1)))
      (let* ((window (capi:editor-window board))
             (buffer (window-buffer window)))
        (process-character
         (lambda (arg) (declare (ignore arg))
           (with-buffer-locked (buffer)
             (with-point ((point (buffers-start buffer)))
               (line-offset point (- y y-offset))
               (editor::move-to-column point (- x x-offset))
               (let ((old (get-text-property point 'pixel)))
                 (if pixel
                   (macrolet ((mkface ()
                                '(make-face nil :foreground (awhen (pixel-fg pixel) (term-color-spec it))
                                            :background (awhen (pixel-bg pixel) (term-color-spec it)))))
                     (if (or (null old) (char/= (pixel-char pixel) (pixel-char old)))
                       (editor::big-replace-string
                        point
                        (editor::make-buffer-string
                         :%string (string (pixel-char pixel))
                         :properties `((0 1 (face ,(mkface) pixel ,pixel))))
                        1)
                       (unless (and (term-color= (pixel-fg pixel) (pixel-fg old))
                                    (term-color= (pixel-bg pixel) (pixel-bg old)))
                         (with-point ((start point))
                           (editor::point-after point)
                           (set-text-properties start point (list 'face (mkface) 'pixel pixel))))))
                   (when old
                     (editor::big-replace-string point (editor::make-buffer-string :%string " " :properties '((0 1 nil))) 1)))))))
         window)))))

(defun draw-pixels (board pixels)
  (let* ((window (capi:editor-window board))
         (buffer (window-buffer window))
         (x-offset (slot-value board 'x-offset))
         (y-offset (slot-value board 'y-offset)))
    (setf (slot-value board 'drawing-pending-p) t)
    (process-character
     (lambda (arg) (declare (ignore arg))
       (block process-char
         (setf @board.drawing-pending-p nil)
         (with-buffer-locked (buffer)
           (with-point ((point (buffers-start buffer)))
             (let ((func (symbol-function 'editor::copy-to-buffer-string)))
               (setf (symbol-function 'editor::copy-to-buffer-string) (constantly (editor::make-buffer-string :%string "")))
               (unwind-protect
                   (loop-pixels pixels
                     (when (and (<= x-offset %x (+ x-offset *board-width* -1))
                                (<= y-offset %y (+ y-offset *board-height* -1)))
                       (if (plusp (- %y y-offset))
                         (line-offset point (- %y y-offset) (- %x x-offset))
                         (editor::move-to-column point (- %x x-offset)))
                       (let ((old (get-text-property point 'pixel)))
                         (if %pixel
                           (macrolet ((mkface ()
                                        '(make-face nil :foreground (awhen (pixel-fg %pixel) (term-color-spec it))
                                                    :background (awhen (pixel-bg %pixel) (term-color-spec it)))))
                             (unless @board.drawing-pending-p
                               (if (or (null old) (char/= (pixel-char %pixel) (pixel-char old)))
                                 (editor::big-replace-string
                                  point
                                  (editor::make-buffer-string
                                   :%string (string (pixel-char %pixel))
                                   :properties `((0 1 (face ,(mkface) pixel ,%pixel))))
                                  1)
                                 (unless (and (term-color= (pixel-fg %pixel) (pixel-fg old))
                                              (term-color= (pixel-bg %pixel) (pixel-bg old)))
                                   (with-point ((start point))
                                     (editor::point-after point)
                                     (set-text-properties start point (list 'face (mkface) 'pixel %pixel)))))))
                           (when old
                             (editor::big-replace-string point (editor::make-buffer-string :%string " " :properties '((0 1 nil))) 1))))
                       (move-point point (buffers-start buffer))))
                 (setf (symbol-function 'editor::copy-to-buffer-string) func)))))))
     window)))

(defun paint-pixel (board x y pixel)
  (setf (slot-value board 'saved) nil)
  (if pixel
    (add-pixel x y pixel (project-pixels (board-project board)))
    (delete-pixel x y (project-pixels (board-project board))))
  (draw-a-pixel board x y pixel))

(defun paint-pixels (board pixels)
  (setf (slot-value board 'saved) nil)
  (let ((copy (copy-pixels pixels)))
    (loop-pixels copy
      (if %pixel
        (add-pixel %x %y %pixel (project-pixels (board-project board)))
        (delete-pixel %x %y (project-pixels (board-project board)))))
    (draw-pixels board (copy-pixels pixels))))

(defun translate-position (board px py)
  (with-slots (x-offset y-offset) board
    (values (+ (floor px *char-width*) x-offset) (+ (floor py *char-height*) y-offset))))

(defun move-cursor-pixelwise (pane x y)
  (let* ((window (capi:editor-window pane))
         (point (buffer-point (capi:editor-pane-buffer pane)))
         (h (editor::wm-window-pixel-height window))
         (col (floor (* (/ y h) *board-height*))))
    (process-character
     (lambda (arg)
       (declare (ignore arg))
       (editor::lock-window-and-call
        (lambda (&rest args) (declare (ignore args))
          (editor::cursorpos-to-point x col window point))
        window))
     window)))

(defun move-cursor-relative (pane x y)
  (setq x (clamp x 0 *board-width*)
        y (clamp y 0 *board-height*))
  (let* ((window (capi:editor-window pane))
         (buffer (capi:editor-pane-buffer pane))
         (point (buffer-point buffer)))
    (process-character
     (lambda (arg)
       (declare (ignore arg))
       (move-point point (buffers-start buffer))
       (if (plusp y)
         (line-offset point y x)
         (editor::move-to-column point x)))
     window)))

(defun move-cursor (pane x y)
  (with-slots (x-offset y-offset) pane
    (if (and (<= x-offset x (+ x-offset *board-width* -1))
             (<= y-offset y (+ y-offset *board-height* -1)))
      (progn
        (decf x x-offset)
        (decf y y-offset)
        (move-cursor-relative pane x y))
      (let* ((1/2w (floor *board-width* 2))
             (1/2h (floor *board-height* 2)))
        (setf x-offset (- x 1/2w)
              y-offset (- y 1/2h))
        (refresh-board pane)
        (move-cursor-relative pane 1/2w 1/2h)))))

(defun draw-selected-pixels (board)
  (with-slots (selected-pixels) board
    (when (and selected-pixels
               (pixels-not-empty-p selected-pixels))
      (with-pixels-boundary selected-pixels
        (let ((pixels (copy-pixels selected-pixels))
              (pixel (make-pixel :char *selection-border-char*
                                 :fg (make-term-color :spec *selection-border-foreground*)
                                 :bg (make-term-color :spec *selection-border-background*))))
          (dorange (x (1- %left) (1+ %right))
            (add-pixel x (1- %top) pixel pixels)
            (add-pixel x %bottom pixel pixels))
          (dorange (y (1- %top) (1+ %bottom))
            (add-pixel (1- %left) y pixel pixels)
            (add-pixel %right y pixel pixels))
          (draw-pixels board pixels))))))

(defun set-selected-pixels (board pixels)
  (with-slots (selected-pixels) board
    (when selected-pixels
      (let ((origin (make-pixels)))
        (with-pixels-boundary selected-pixels
          (dorange$fixnum (y (1- %top) (1+ %bottom))
            (dorange$fixnum (x (1- %left) (1+ %right))
              (when (or (null pixels)
                        (not (find-pixel x y pixels)))
                (add-pixel x y (get-pixel x y board) origin)))))
        (draw-pixels board origin)))
    (setf selected-pixels pixels)
    (draw-selected-pixels board)))

(defun refresh-board (board)
  (with-slots (project x-offset y-offset) board
    (let* ((window (capi:editor-window board))
           (buffer (window-buffer window))
           (pixels (project-pixels project))
           (point (buffer-point buffer))
           (pos (point-position point)))
      (process-character
       (lambda (arg) (declare (ignore arg))
         (with-buffer-locked (buffer)
           (clear-buffer buffer)
           (buffer-start point)
           (dorange$fixnum (y y-offset (+ *board-height* y-offset))
             (dorange$fixnum (x x-offset (+ *board-width* x-offset))
               (with-point ((start point))
                 (if-let (pixel (find-pixel x y pixels))
                     (progn (insert-character point (pixel-char pixel))
                       (editor::point-before start)
                       (set-text-properties
                        start point
                        (list 'face (make-face nil
                                               :foreground (awhen (pixel-fg pixel)
                                                             (term-color-spec it))
                                               :background (awhen (pixel-bg pixel)
                                                             (term-color-spec it))))
                        :modification nil))
                   (progn
                     (insert-character point #\Space)
                     (editor::point-before start)
                     (remove-text-properties start point '(face) :modification nil)))))
             (insert-character point #\Newline))
           (editor::delete-characters point -1)
           (setf (point-position point) pos)
           (editor::redisplay-all)))
       window))
    (draw-selected-pixels board)
    (let ((itf (capi:element-interface board)))
      (setf (capi:text-input-pane-text @itf.x-offset-input) (princ-to-string x-offset)
            (capi:text-input-pane-text @itf.y-offset-input) (princ-to-string y-offset)))))
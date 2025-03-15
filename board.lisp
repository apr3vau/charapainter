;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

;; Definition

(defclass board-container (capi:pinboard-layout)
  ((board :initform (make 'board)))
  (:default-initargs
   :focus-callback (lambda (self p) (when p (capi:set-pane-focus @self.board)))
   :resize-callback (lambda (container x y w h)
                      (declare (ignore x y))
                      (let* ((board @container.board)
                             (char-width (gp:get-font-width board))
                             (new-w (floor w char-width))
                             (new-h (floor h (gp:get-font-height board))))
                        (setf @board.width new-w
                              @board.height new-h)
                        (setf (capi:static-layout-child-geometry board) (values 0 0 (+ w char-width) h))
                        (refresh-board board)))))

(defmethod initialize-instance :after ((self board-container) &key)
  (setf (capi:layout-description self) (list @self.board)))

(defclass board (capi:editor-pane)
  ((project           :initform (make-project)
                      :accessor board-project)
   (saved             :initform t)
   
   (current-layer     :initform nil)
   (current-tool      :initform nil            
                      :accessor board-current-tool)
   
   (x-offset          :initform 0)
   (y-offset          :initform 0)
   (width             :initform 80)
   (height            :initform 32)
   (cursor-movement   :initform :right)
   (selected-pixels   :initform nil)
   
   (drawing-pending-p :initform nil)
   (pending-pixels    :initform (make-pixels))
   (refresh-pending-p :initform nil)
   
   (fdesc             :initform nil)
   (char-width        :initform nil)
   (char-height       :initform nil)
   
   (float-font-size   :initform *default-font-size*)

   (fg                :initform nil)
   (bg                :initform nil)
   (char              :initform #\.)
   (bold-p            :initform nil)
   (italic-p          :initform nil)
   (underline-p       :initform nil))
  (:default-initargs
   :buffer (make-buffer (symbol-name (gensym)) :temporary t :flag :charapainter)
   :foreground *default-foreground*
   :background *default-background*
   :internal-min-width (list 'character (- 80 3))
   :wrap-style nil
   :line-wrap-marker nil
   :line-wrap-face nil
   :vertical-scroll nil
   :create-callback (lambda (board)
                      (setf @board.char-width (gp:get-font-width board)
                            @board.char-height (gp:get-font-height board)))
   :display-callback #'board-display-callback
   :input-model
   `(((:button-1 :press)                                       ,(lambda (pane x y)     (tool-press (board-current-tool pane) x y)))
     ((:button-1 :release)                                     ,(lambda (pane x y)     (tool-release (board-current-tool pane) x y)))
     ((:button-1 :motion)                                      ,(lambda (pane x y)     (tool-drag (board-current-tool pane) x y)))
     ((:button-1 :press :shift)                                ,(lambda (pane x y)     (tool-shift-press (board-current-tool pane) x y)))
     ((:button-1 :motion :shift)                               ,(lambda (pane x y)     (tool-shift-drag (board-current-tool pane) x y)))
     ((:button-1 :release :shift)                              ,(lambda (pane x y)     (tool-release (board-current-tool pane) x y)))
     (:motion                                                  ,(lambda (pane x y)     (tool-motion (board-current-tool pane) x y)))
     ((:touch :zoom)                                           ,(lambda (pane x y factor)
                                                                  (with-slots (float-font-size) pane
                                                                    (setf float-font-size (max 5.0 (* float-font-size factor))
                                                                          @pane.project.font-size (round float-font-size)))
                                                                  (refresh-font pane)
                                                                  (move-cursor-pixelwise pane x y)))
     
     ((:gesture-spec #\Return)                                 ,(lambda (pane x y key) (tool-return (board-current-tool pane) x y key)))
     ((:gesture-spec #\Backspace)                              ,(lambda (pane x y key) (tool-backspace (board-current-tool pane) x y key)))
     ((:gesture-spec #\Escape)                                 ,(lambda (pane x y key) (tool-escape (board-current-tool pane) x y key)))

     ((:gesture-spec :left)                                    ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :right)                                   ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :up)                                      ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :down)                                    ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec :left :meta)                              ,(lambda (pane x y key) (tool-meta-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :right :meta)                             ,(lambda (pane x y key) (tool-meta-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :up :meta)                                ,(lambda (pane x y key) (tool-meta-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :down :meta)                              ,(lambda (pane x y key) (tool-meta-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :left :shift)                             ,(lambda (pane x y key) (tool-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :right :shift)                            ,(lambda (pane x y key) (tool-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :up :shift)                               ,(lambda (pane x y key) (tool-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :down :shift)                             ,(lambda (pane x y key) (tool-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :left :control :shift)                    ,(lambda (pane x y key) (tool-ctrl-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :right :control :shift)                   ,(lambda (pane x y key) (tool-ctrl-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :up :control :shift)                      ,(lambda (pane x y key) (tool-ctrl-shift-arrow (board-current-tool pane) x y key)))
     ((:gesture-spec :down :control :shift)                    ,(lambda (pane x y key) (tool-ctrl-shift-arrow (board-current-tool pane) x y key)))
     
     ((:gesture-spec #\f :control)                             ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\b :control)                             ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\p :control)                             ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\n :control)                             ,(lambda (pane x y key) (tool-arrow-key (board-current-tool pane) x y key)))
     ((:gesture-spec #\Space :control)                         ,(lambda (pane x y key) (tool-ctrl-space (board-current-tool pane) x y key)))
     ((:gesture-spec #\a :control)                             ,(lambda (pane x y key) (tool-line-start (board-current-tool pane) x y key)))
     ((:gesture-spec #\e :control)                             ,(lambda (pane x y key) (tool-line-end (board-current-tool pane) x y key)))
     ((:gesture-spec #\l :control)                             ,(lambda (pane x y key) (declare (ignore x y key)) (board-recenter pane)))
     
     ((:gesture-spec :left #+darwin :hyper #-darwin :control)  ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(-1 . 0))))
     ((:gesture-spec :right #+darwin :hyper #-darwin :control) ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(1 . 0))))
     ((:gesture-spec :up #+darwin :hyper #-darwin :control)    ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(0 . -1))))
     ((:gesture-spec :down #+darwin :hyper #-darwin :control)  ,(lambda (pane x y key) (declare (ignore x y key)) (move-board pane '(0 . 1))))
     
     (:gesture-spec                                            ,(lambda (pane x y key) (tool-key (board-current-tool pane) x y key))))))

(defmethod initialize-instance :after ((board board) &key)
  (let ((fdesc (gp:make-font-description :family @board.project.font-family
                                         :size @board.project.font-size)))
    (setf @board.fdesc fdesc
          (capi:simple-pane-font board) fdesc))
  (setf @board.current-layer (first (project-layers @board.project))))

(defun board-display-callback (board x y w h)
  (capi::editor-pane-display board x y w h)
  ;; Grid
  (capi:with-geometry board
    (let ((width (gp:get-font-width board))
          (height (gp:get-font-height board))
          lines)
      (dorange (fx 0 capi:%width% width)
        (let ((ix (round fx)))
          (when (<= x ix (+ x w))
            (setq lines (nconc lines (list ix y ix (+ y h)))))))
      (dorange (fy 0 capi:%height% height)
        (let ((iy (round fy)))
          (when (<= y iy (+ y h))
            (setq lines (nconc lines (list x iy (+ x w) iy))))))
      (gp:draw-lines board lines :foreground (color:make-rgb 1 1 1 0.1))))
  ;; Selected pixels border
  (with-slots (selected-pixels x-offset y-offset) board
    (when (and selected-pixels (pixels-not-empty-p selected-pixels))
      (with-pixels-boundary selected-pixels
        (let ((x (* (- %left x-offset) @board.char-width))
              (y (* (- %top y-offset) @board.char-height))
              (w (* (- %right %left) @board.char-width))
              (h (* (- %bottom %top) @board.char-height)))
          (gp:draw-rectangle board x y w h
                             :foreground :black :thickness 4)
          (gp:draw-rectangle board x y w h
                             :foreground :white :thickness 4 :dashed t))))))

;; Edit actions

(defmethod board-undo (itf)
  (tool-cleanup @itf.board.current-tool)
  (paint-pixels @itf.board (ring-pop @itf.board.current-layer.undo-ring)))

(defmethod capi:pane-interface-undo-p ((board board) itf)
  (plusp (ring-length @board.current-layer.undo-ring)))

(defmethod capi:pane-interface-cut-p ((board board) itf)
  @board.selected-pixels)

(defun board-cut (itf)
  (with-slots (board) itf
    (with-slots (current-tool selected-pixels) board
      (let ((pixels (or selected-pixels (layer-pixels @board.current-layer)))
            (undo-ring (layer-undo-ring @board.current-layer)))
        (capi:set-clipboard
         board
         (copy-pixels pixels)
         (with-output-to-string (out)
           (case @itf.copy-option
             (:ansi (export-to-ansi pixels out))
             (:html (export-to-html pixels out))
             (:text (export-to-plain-text pixels out))))
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
            (ring-push (layer-pixels @board.current-layer) undo-ring)
            (setf (layer-pixels @board.current-layer) (make-pixels))
            (refresh-board board)))))))

(defmethod capi:pane-interface-copy-p ((board board) itf)
  (or @board.selected-pixels (pixels-not-empty-p (layer-pixels @board.current-layer))))

(defun board-copy (itf)
  (with-slots (board) itf
    (let ((pixels (or @board.selected-pixels (layer-pixels @board.current-layer))))
      (capi:set-clipboard
       board
       (copy-pixels pixels)
       (with-output-to-string (out)
         (case @itf.copy-option
           (:ansi (export-to-ansi pixels out))
           (:html (export-to-html pixels out))
           (:text (export-to-plain-text pixels out))))
       (list :image (export-to-image board pixels))))))

(defmethod capi:pane-interface-paste-p ((board board) itf)
  (or (capi:clipboard board :string)
      (capi:clipboard board :value)
      (capi:clipboard board :image)))

(defun board-paste (itf)
  (with-slots (board tools) itf
    (with-slots (x-offset y-offset) board
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
             (let ((tool (find 'select tools :key #'class-name-of)))
               (if (eq @board.current-tool tool)
                 (tool-return tool nil nil nil)
                 (tool-cleanup tool))
               (set-tool-internal itf board tool)
               (setf @tool.pixels (copy-pixels new)))
             (set-selected-pixels board new)
             (move-cursor board (pixels-left new) (pixels-top new)))))
        ((capi:clipboard board :image)
         (let ((tool (find 'import-image tools :key #'class-name-of)))
           (with-slots (image dx dy old-tool width height) tool
             (let* ((w (gp:image-width it))
                    (h (gp:image-height it))
                    (ratio (min (/ (* 7/8 @board.width) w)
                                (/ (* 7/8 @board.height) h))))
               (setf image it
                     dx (1+ x-offset)
                     dy (1+ y-offset)
                     width (floor (* w (* ratio 2)))
                     height (floor (* h ratio))
                     old-tool (class-name-of @board.current-tool))
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
                          (add-pixel x y (make-pixel :char c :fg @board.fg :bg @board.bg
                                                     :bold-p @board.bold-p
                                                     :italic-p @board.italic-p
                                                     :underline-p @board.underline-p)
                                     new)
                          (incf x))))
                it)
           (set-selected-pixels board new)
           (let ((tool (find 'select tools :key #'class-name-of)))
             (setf (slot-value tool 'pixels) (make-pixels))
             (set-tool-internal itf board tool))))))))

(defmethod capi:pane-interface-paste-object ((board board) itf)
  (board-paste itf))

(defun change-cursor-movement (data itf)
  (setf @itf.board.cursor-movement data
        (capi:choice-selected-item @itf.cursor-movement-option) data))

;; Project and layer

(defstruct project
  (name "Unnamed")
  (font-family *default-font-family*)
  (font-size *default-font-size*)
  (layers (list (make-layer))))

(defstruct layer
  (name "Default Layer")
  (undo-ring (make-ring *undo-ring-size* 'charapainter-undo-ring))
  (pixels (make-pixels)))

(defmacro with-layers-boundary (layers &body body)
  `(loop for layer in ,layers
         for pixels = (layer-pixels layer)
         minimize (pixels-left pixels) into %left
         maximize (pixels-right pixels) into %right
         minimize (pixels-top pixels) into %top
         maximize (pixels-bottom pixels) into %bottom
         finally (return (progn ,@body))))

(defun board-layers-with-selection (board)
  (if (and @board.selected-pixels (pixels-not-empty-p @board.selected-pixels))
    (let* ((layers (project-layers @board.project))
           (index (position @board.current-layer layers))
           (temp-layer (make-layer :pixels (union-pixels (layer-pixels @board.current-layer)
                                                         @board.selected-pixels))))
      (append (subseq layers 0 index) (list temp-layer) (subseq layers (1+ index))))
    (project-layers @board.project)))

(defun composed-foreground (x y layers)
  (let (color)
    (dolist (layer layers)
      (if color
        (awhen (nfind-pixel x y (layer-pixels layer))
          (awhen (pixel-fg it)
            (let ((premultiplied (color:color-to-premultiplied (term-color-spec-with-alpha it))))
              (setq color (compose-two-colors premultiplied color)))))
        (awhen (nfind-pixel x y (layer-pixels layer))
          (awhen (pixel-fg it)
            (setq color
                  (color:color-to-premultiplied (term-color-spec-with-alpha it)))))))
    color))

(defun composed-background (x y layers)
  (let (color)
    (dolist (layer layers)
      (if color
        (awhen (nfind-pixel x y (layer-pixels layer))
          (awhen (pixel-bg it)
            (let ((premultiplied (color:color-to-premultiplied (term-color-spec-with-alpha it))))
              (setq color (compose-two-colors premultiplied color)))))
        (awhen (nfind-pixel x y (layer-pixels layer))
          (awhen (pixel-bg it)
            (setq color
                  (color:color-to-premultiplied (term-color-spec-with-alpha it)))))))
    color))

(defun composed-character (x y layers)
  (dolist (layer layers)
    (when-let (pixel (nfind-pixel x y (layer-pixels layer)))
      (let ((char (pixel-char pixel)))
        (unless (eql char #\Space)
          (return (values char (list :bold-p (pixel-bold-p pixel)
                                     :italic-p (pixel-italic-p pixel)
                                     :underline-p (pixel-underline-p pixel)))))))))

(defun composed-pixel (x y layers)
  (let (bg fg char bold-p italic-p underline-p)
    (dolist (layer layers)
      (when-let (pixel (nfind-pixel x y (layer-pixels layer)))
        (awhen (pixel-bg pixel)
          (if bg
            (let ((premultiplied (color:color-to-premultiplied (term-color-spec-with-alpha it))))
              (setq bg (compose-two-colors premultiplied bg)))
            (setq bg (color:color-to-premultiplied (term-color-spec-with-alpha it)))))
        (awhen (pixel-fg pixel)
          (if fg
            (let ((premultiplied (color:color-to-premultiplied (term-color-spec-with-alpha it))))
              (setq fg (compose-two-colors premultiplied fg)))
            (setq fg (color:color-to-premultiplied (term-color-spec-with-alpha it)))))
        (when (and (not (eql (pixel-char pixel) #\Space))
                   (null char))
          (setq char (pixel-char pixel)
                bold-p (pixel-bold-p pixel)
                italic-p (pixel-italic-p pixel)
                underline-p (pixel-underline-p pixel)))))
    (when (or bg fg char bold-p italic-p underline-p)
      (make-pixel :fg (when fg (term-color-from-spec fg))
                  :bg (when bg (term-color-from-spec bg))
                  :char (or char #\Space)
                  :bold-p bold-p :italic-p italic-p :underline-p underline-p))))

(defparameter *chunk-size* 50)
(defparameter *process-count* 8)

(defun fast-composed-pixels (layers board terminate-slot &key rect pixels)
  (let* (chunks
         (chunks-lock (mp:make-lock))
         (arr (make-array *chunk-size* :element-type 'cons :fill-pointer 0))
         (barrier (mp:make-barrier *process-count*))
         (table (make-hash-table :test #'equal))
         terminate-var
         (func (lambda ()
                 (let (chunk)
                   (loop (when (or terminate-var (slot-value board terminate-slot))
                           (setf terminate-var t)
                           (return (mp:barrier-wait barrier)))
                         (mp:with-lock (chunks-lock)
                           (setq chunk (pop chunks)))
                         (if chunk
                           (loop for cell across chunk
                                 for (x . y) = cell
                                 do (setf (gethash cell table) (composed-pixel x y layers)))
                           (return (mp:barrier-wait barrier))))))))
    (flet ((push-arr (x y)
             (vector-push (cons x y) arr)
             (when (= (fill-pointer arr) *chunk-size*)
               (push arr chunks)
               (setq arr (make-array *chunk-size* :element-type 'cons :fill-pointer 0)))))
      (cond ((and rect pixels)
             (gp:rect-bind (x y w h) rect
               (loop-pixels pixels
                 (when (and (<= x %x (1- (+ x w)))
                            (<= y %y (1- (+ y h))))
                   (push-arr %x %y)))))
            (rect
             (gp:rect-bind (x y w h) rect
               (dorange$fixnum (%y y (+ y h))
                 (dorange$fixnum (%x x (+ x w))
                   (push-arr %x %y)))))
            (pixels
             (loop-pixels pixels
               (push-arr %x %y)))))
    (when (plusp (fill-pointer arr))
      (push arr chunks))
    (dotimes (i (1- *process-count*))
      (mp:process-run-function (gensym i) () func))
    (funcall func)
    (make-pixels :table table)))

(defun composed-pixels (layers &optional left top right bottom (bit 24))
  (let ((result (make-pixels)))
    (dolist (layer layers)
      (loop-pixels (layer-pixels layer)
        (when (or (null left)
                  (and (<= left %x right)
                       (<= top %y bottom)))
          (if-let (prev-pixel (nfind-pixel %x %y result))
              (let* ((old-fg (pixel-fg prev-pixel))
                     (old-bg (pixel-bg prev-pixel))
                     (new-fg (pixel-fg %pixel))
                     (new-bg (pixel-bg %pixel))
                     (old-fg-spec (when old-fg (term-color-spec old-fg)))
                     (old-bg-spec (when old-bg (term-color-spec old-bg)))
                     (new-fg-spec (when new-fg (color:color-to-premultiplied (term-color-spec-with-alpha new-fg))))
                     (new-bg-spec (when new-bg (color:color-to-premultiplied (term-color-spec-with-alpha new-bg))))
                     (pixel (copy-pixel %pixel))
                     (fg (or old-fg-spec new-fg-spec))
                     (bg (or old-bg-spec new-bg-spec)))
                (when (and old-fg-spec new-fg-spec)
                  (setq fg (compose-two-colors new-fg-spec old-fg-spec)))
                (when (and old-bg-spec new-bg-spec)
                  (setq bg (compose-two-colors new-bg-spec old-bg-spec)))
                (setf (pixel-fg pixel) (when fg (term-color-from-spec fg))
                      (pixel-bg pixel) (when bg (term-color-from-spec bg)))
                (when (not (eql (pixel-char prev-pixel) #\Space))
                  (setf (pixel-char pixel) (pixel-char prev-pixel)
                        (pixel-bold-p pixel) (pixel-bold-p prev-pixel)
                        (pixel-italic-p pixel) (pixel-italic-p prev-pixel)
                        (pixel-underline-p pixel) (pixel-underline-p prev-pixel)))
                (add-pixel %x %y pixel result))
            (let ((pixel (copy-pixel %pixel)))
              (when (pixel-fg %pixel)
                (setf (pixel-fg pixel)
                      (term-color-from-spec (color:color-to-premultiplied (term-color-spec-with-alpha (pixel-fg %pixel))))))
              (when (pixel-bg %pixel)
                (setf (pixel-bg pixel)
                      (term-color-from-spec (color:color-to-premultiplied (term-color-spec-with-alpha (pixel-bg %pixel))))))
              (add-pixel %x %y pixel result))))))
    (case bit
      (4 (loop-pixels result
           (setf (pixel-fg %pixel) (coerce-color-to 4 (pixel-fg %pixel))
                 (pixel-bg %pixel) (coerce-color-to 4 (pixel-bg %pixel)))))
      (8 (loop-pixels result
           (setf (pixel-fg %pixel) (coerce-color-to 8 (pixel-fg %pixel))
                 (pixel-bg %pixel) (coerce-color-to 8 (pixel-bg %pixel))))))
    result))

;; Pixel drawing method

(defun get-pixel (x y board)
  (declare (inline get-pixel))
  (find-pixel x y (layer-pixels @board.current-layer)))

(defun draw-a-pixel (board x y)
  (with-slots (x-offset y-offset) board
    (when (and (<= x-offset x (+ x-offset @board.width -1))
               (<= y-offset y (+ y-offset @board.height -1)))
      (let* ((window (capi:editor-window board))
             (buffer (window-buffer window))
             (layers (board-layers-with-selection board)))
        (process-character
         (lambda (arg) (declare (ignore arg))
           (with-buffer-locked (buffer)
             (with-point ((point (buffers-start buffer)))
               (line-offset point (- y y-offset))
               (editor::move-to-column point (- x x-offset))
               (let ((fg (composed-foreground x y layers))
                     (bg (composed-background x y layers)))
                 (multiple-value-bind (char other-styles)
                     (composed-character x y layers)
                   (unless char (setq char #\Space))
                   (editor::big-replace-string
                    point
                    (editor::make-buffer-string
                     :%string (string char)
                     :properties `((0 1 (editor:face ,(apply #'make-face nil :foreground fg :background bg other-styles)))))
                    1))))))
         window)))))

(defun draw-pixels (board pixels)
  (let* ((window (capi:editor-window board))
         (x-offset (slot-value board 'x-offset))
         (y-offset (slot-value board 'y-offset))
         (layers (board-layers-with-selection board)))
    (setf @board.drawing-pending-p t)
    (process-character
     (lambda (arg) (declare (ignore arg))
       (setf @board.drawing-pending-p nil)
       (let* ((buffer (window-buffer window))
              (pixels (fast-composed-pixels layers board 'drawing-pending-p
                                            :rect (list x-offset y-offset @board.width @board.height)
                                            :pixels pixels)))
         (unless @board.drawing-pending-p
           (with-point ((point (buffers-start buffer)))
             (with-buffer-locked (buffer)
               (let ((func (symbol-function 'editor::copy-to-buffer-string))
                     (blank-str (editor::make-buffer-string :%string " " :properties `((0 1 nil)))))
                 (setf pixels (nunion-pixels @board.pending-pixels pixels)
                       @board.pending-pixels (make-pixels))
                 (unless @board.drawing-pending-p
                   (setf (symbol-function 'editor::copy-to-buffer-string) (constantly (editor::make-buffer-string :%string "")))
                   (unwind-protect
                       (loop-pixels pixels
                         (when @board.drawing-pending-p (return))
                         (if (plusp (- %y y-offset))
                           (line-offset point (- %y y-offset) (- %x x-offset))
                           (editor::move-to-column point (- %x x-offset)))
                         (if %pixel
                           (editor::big-replace-string
                            point
                            (editor::make-buffer-string
                             :%string (string (pixel-char %pixel))
                             :properties `((0 1 (editor:face ,(pixel-face %pixel)))))
                            1)
                           (editor::big-replace-string point blank-str 1))
                         (move-point point (buffers-start buffer)))
                     (setf (symbol-function 'editor::copy-to-buffer-string) func))
                   (when @board.drawing-pending-p
                     (loop-pixels pixels
                       (add-pixel %x %y nil @board.pending-pixels))))))))))
     window)))

(defun paint-pixel (board x y pixel)
  (setf (slot-value board 'saved) nil)
  (if (pixel-empty-p pixel)
    (delete-pixel x y (layer-pixels @board.current-layer))
    (add-pixel x y pixel (layer-pixels @board.current-layer)))
  (draw-a-pixel board x y))

(defun paint-pixels (board pixels)
  (setf (slot-value board 'saved) nil)
  (let ((copy (copy-pixels pixels)))
    (loop-pixels copy
      (if (pixel-empty-p %pixel)
        (delete-pixel %x %y (layer-pixels @board.current-layer))
        (add-pixel %x %y %pixel (layer-pixels @board.current-layer))))
    (draw-pixels board (copy-pixels pixels))))

(defun translate-position (board px py)
  (with-slots (x-offset y-offset) board
    (let* ((rel-x (clamp (floor px @board.char-width) 0 (1- @board.width)))
           (rel-y (clamp (floor py @board.char-height) 0 (1- @board.height)))
           (x (+ rel-x x-offset))
           (y (+ rel-y y-offset)))
      (values x y))))

(defun move-cursor-pixelwise (pane x y)
  (let* ((window (capi:editor-window pane))
         (col (floor y (gp:get-font-height pane))))
    (process-character
     (lambda (arg)
       (declare (ignore arg))
       (editor::lock-window-and-call
        (lambda (&rest args) (declare (ignore args))
          (let ((point (buffer-point (window-buffer window))))
            (when (editor::good-point-p point)
              (editor::cursorpos-to-point x col window point)
              (when (eql (character-at point 0) #\Newline)
                (editor::point-before point)))))
        window))
     window)))

(defun move-cursor-relative (pane x y)
  (setq x (clamp x 0 @pane.width)
        y (clamp y 0 @pane.height))
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
    (if (and (<= x-offset x (+ x-offset @pane.width -1))
             (<= y-offset y (+ y-offset @pane.height -1)))
      (progn
        (decf x x-offset)
        (decf y y-offset)
        (move-cursor-relative pane x y))
      (let* ((1/2w (floor @pane.width 2))
             (1/2h (floor @pane.height 2)))
        (setf x-offset (- x 1/2w)
              y-offset (- y 1/2h))
        (refresh-board pane)
        (move-cursor-relative pane 1/2w 1/2h))))) 

(defun set-selected-pixels (board pixels)
  (with-slots (selected-pixels) board
    (if pixels
      (let ((need-update (if selected-pixels
                           (union-pixels selected-pixels pixels)
                           pixels)))
        (setf selected-pixels pixels)
        (draw-pixels board need-update))
      (if selected-pixels
        (let ((selected selected-pixels))
          (setf selected-pixels nil)
          (draw-pixels board selected))))
    (gp:invalidate-rectangle board)))

;; Drawing / Moving the board

(defun board-recenter (board)
  (let* ((point (buffer-point (capi:editor-pane-buffer board)))
         (x (point-column point))
         (y (point-linenum point))
         (cx (floor @board.width 2))
         (cy (floor @board.height 2)))
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

(defun refresh-board (board)
  (with-slots (x-offset y-offset) board
    (let* ((window (capi:editor-window board))
           (buffer (window-buffer window))
           (layers (board-layers-with-selection board))
           (pos (point-position (buffer-point buffer)))
           (blank-str (editor::make-buffer-string :%string " " :properties '((0 1 nil)))))
      (setf @board.refresh-pending-p t)
      (process-character
       (lambda (arg) (declare (ignore arg))
         (setf @board.refresh-pending-p nil)
         (let* ((new-buf (make-buffer (symbol-name (gensym)) :temporary t :flag :charapainter))
                (point (buffer-point new-buf))
                (pixels (fast-composed-pixels layers board 'refresh-pending-p
                                              :rect (list x-offset y-offset @board.width @board.height))))
           (buffer-start point)
           (block draw
             (dorange$fixnum (y y-offset (+ @board.height y-offset))
               (dorange$fixnum (x x-offset (+ @board.width x-offset))
                 (when @board.refresh-pending-p
                   (editor::delete-buffer new-buf)
                   (return-from draw))
                 (if-let (pixel (nfind-pixel x y pixels))
                     (editor::insert-buffer-string
                      point
                      (editor::make-buffer-string
                       :%string (string (pixel-char pixel))
                       :properties `((0 1 (editor:face ,(pixel-face pixel))))))
                   (editor::insert-buffer-string point blank-str)))
               (insert-character point #\Newline))
             (editor::delete-characters point -1)
             (setf (point-position point) pos)
             (editor::%set-window-buffer window new-buf)
             (editor::delete-buffer buffer))))
       window))
    (let ((itf (capi:element-interface board)))
      (setf (capi:text-input-pane-text @itf.x-offset-input) (princ-to-string x-offset)
            (capi:text-input-pane-text @itf.y-offset-input) (princ-to-string y-offset)))))

;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defclass board-tool ()
  ((board :initarg :board
          :accessor tool-board)
   (settings-layout :initform nil)
   (default-message :initform ""
                    :initarg :default-message)))

(defgeneric set-tool (interface board name)
  (:method (interface board name)
   (with-slots (tools) interface
     (let ((tool (find name tools :key #'class-name-of)))
       (set-tool-internal interface board tool)))))

(defgeneric set-tool-internal (interface board tool)
  (:method (itf board tool)
   (with-slots (current-tool) board
     (when current-tool (tool-cleanup current-tool))
     (setf current-tool tool)
     (set-message @tool.default-message itf))))

(defgeneric tool-cleanup     (tool)         (:method (tool)))

(defgeneric tool-press            (tool x y)     (:method (tool x y)))
(defgeneric tool-drag             (tool x y)     (:method (tool x y)))
(defgeneric tool-release          (tool x y)     (:method (tool x y)))
(defgeneric tool-motion           (tool x y)     (:method (tool x y)))
(defgeneric tool-shift-press      (tool x y)     (:method (tool x y)))
(defgeneric tool-shift-drag       (tool x y)     (:method (tool x y)))
(defgeneric tool-ctrl-space       (tool x y key) (:method (tool x y key)))
(defgeneric tool-escape           (tool x y key) (:method (tool x y key)))
(defgeneric tool-meta-arrow       (tool x y key) (:method (tool x y key)))
(defgeneric tool-shift-arrow      (tool x y key) (:method (tool x y key)))
(defgeneric tool-ctrl-shift-arrow (tool x y key) (:method (tool x y key)))

(defgeneric tool-set-fg-hook      (tool)         (:method (tool)))
(defgeneric tool-set-bg-hook      (tool)         (:method (tool)))
(defgeneric tool-set-char-hook    (tool)         (:method (tool)))

(defun board-pixel (board)
  (declare (inline board-pixel))
  (make-pixel :char @board.char :fg @board.fg :bg @board.bg
              :bold-p @board.bold-p
              :italic-p @board.italic-p
              :underline-p @board.underline-p))

(defgeneric tool-return (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (let* ((point (editor-pane-point board))
              (x (+ x-offset (point-column point)))
              (y (+ y-offset (point-linenum point)))
              (origin (get-pixel x y board)))
         (tagbody
          (when (plusp (ring-length @board.current-layer.undo-ring))
            (let ((undo (ring-ref @board.current-layer.undo-ring 0))
                  (maybe-prev-loc
                   (case @board.cursor-movement
                     (:left (cons (1+ x) y)) (:right (cons (1- x) y))
                     (:up   (cons x (1+ y))) (:down  (cons x (1- y))))))
              (if (and (pixels-line-p undo)
                       (second (multiple-value-list (gethash maybe-prev-loc (pixels-table undo))))
                       (null (second (multiple-value-list (nfind-pixel x y undo)))))
                (progn
                  (add-pixel x y origin undo)
                  (go end))
                (go add-one))))
          add-one
          (let ((pixels (make-pixels)))
            (add-pixel x y origin pixels)
            (ring-push pixels @board.current-layer.undo-ring))
          end)
         (paint-pixel board x y (board-pixel board))
         (case @board.cursor-movement
           (:left (decf x)) (:right (incf x))
           (:up   (decf y)) (:down  (incf y)))
         (move-cursor board x y))))))

(defgeneric tool-backspace (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (let* ((point (editor-pane-point board))
              (x (+ x-offset (point-column point)))
              (y (+ y-offset (point-linenum point)))
              (prev-x x) (prev-y y))
         (case @board.cursor-movement
           (:left (incf x)) (:right (decf x))
           (:up   (incf y)) (:down  (decf y)))
         (let ((origin (get-pixel x y board)))
           (tagbody
            (when (plusp (ring-length @board.current-layer.undo-ring))
              (let ((undo (ring-ref @board.current-layer.undo-ring 0)))
                (if (and (pixels-line-p undo)
                         (second (multiple-value-list (nfind-pixel prev-x prev-y undo)))
                         (null (second (multiple-value-list (nfind-pixel x y undo)))))
                  (progn
                    (add-pixel x y origin undo)
                    (go end))
                  (go add-one))))
            add-one
            (let ((pixels (make-pixels)))
              (add-pixel x y origin pixels)
              (ring-push pixels @board.current-layer.undo-ring))
            end))
         
         (paint-pixel board x y nil)
         (move-cursor board x y))))))

(defgeneric tool-arrow-key (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (editor-pane-point board))
            (x (point-column point))
            (y (point-linenum point)))
       (case (sys:gesture-spec-data key)
         ((or :up 112)
          (decf y))
         ((or :down 110)
          (incf y))
         ((or :left 98)
          (decf x))
         ((or :right 102)
          (incf x)))
       (setq x (clamp x 0 @board.width)
             y (clamp y 0 @board.height))
       (move-cursor-relative board x y)))))

(defgeneric tool-line-start (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (editor-pane-point board))
            (y (point-linenum point)))
       (move-cursor-relative board 0 y)))))

(defgeneric tool-line-end (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (editor-pane-point board))
            (y (point-linenum point)))
       (move-cursor-relative board @board.width y)))))

(defgeneric tool-key (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (awhen (system:gesture-spec-to-character key :errorp nil)
         (let* ((point (editor-pane-point board))
                (x (+ x-offset (point-column point)))
                (y (+ y-offset (point-linenum point)))
                (origin (get-pixel x y board)))
           (tagbody
            (when (plusp (ring-length @board.current-layer.undo-ring))
              (let ((undo (ring-ref @board.current-layer.undo-ring 0))
                    (maybe-prev-loc
                     (case @board.cursor-movement
                       (:left (cons (1+ x) y)) (:right (cons (1- x) y))
                       (:up   (cons x (1+ y))) (:down  (cons x (1- y))))))
                (if (and (pixels-line-p undo)
                         (second (multiple-value-list (gethash maybe-prev-loc (pixels-table undo))))
                         (null (second (multiple-value-list (nfind-pixel x y undo)))))
                  (progn
                    (add-pixel x y origin undo)
                    (go end))
                  (go add-one))))
            add-one
            (let ((pixels (make-pixels)))
              (add-pixel x y origin pixels)
              (ring-push pixels @board.current-layer.undo-ring))
            end)
           (paint-pixel board x y (make-pixel :char it :fg @board.fg :bg @board.bg
                                              :bold-p @board.bold-p
                                              :italic-p @board.italic-p
                                              :underline-p @board.underline-p))
           (case @board.cursor-movement
             (:left (decf x)) (:right (incf x))
             (:up   (decf y)) (:down  (incf y)))
           (move-cursor board x y)))))))

(defgeneric make-settings-layout (interface tool)
  (:method (itf tool) (make #+lispworks8.1 'capi:dummy-pane
                            #-lispworks8.1 'capi::dumm-pane)))

;; Pan

(defclass pan (board-tool)
  ((drag-start-x-offset :initform nil)
   (drag-start-y-offset :initform nil)
   (drag-start-x        :initform nil)
   (drag-start-y        :initform nil))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Use  ⌘ ←↑↓→  to move the board anytime!"
                     :properties '((0 4 (face editor::default))
                                   (4 12 (face hl-background))
                                   (12 39 (face editor::default))))))

(defmethod set-tool :after (itf board (name (eql 'pan)))
  (setf (capi:simple-pane-cursor board) :open-hand))

(defmethod tool-press ((tool pan) x y)
  (with-slots (board drag-start-x drag-start-y drag-start-x-offset drag-start-y-offset) tool
    (with-slots (x-offset y-offset) board
      (setf drag-start-x-offset x-offset
            drag-start-y-offset y-offset
            drag-start-x x
            drag-start-y y
            (capi:simple-pane-cursor board) #+darwin :closed-hand #-darwin :move))))

(defmethod tool-drag ((tool pan) x y)
  (with-slots (board drag-start-x drag-start-y drag-start-x-offset drag-start-y-offset) tool
    (with-slots (x-offset y-offset) board
      (let ((new-x-offset (+ drag-start-x-offset (floor (- drag-start-x x) @board.char-width)))
            (new-y-offset (+ drag-start-y-offset (floor (- drag-start-y y) @board.char-height))))
        (unless (and (= new-x-offset x-offset)
                     (= new-y-offset y-offset))
          (setf x-offset new-x-offset
                y-offset new-y-offset)
          (refresh-board board))))))

(defmethod tool-release ((tool pan) x y)
  (setf (capi:simple-pane-cursor @tool.board)
        #+darwin :open-hand
        #-darwin :move))

(defmethod tool-cleanup ((tool pan))
  (setf (capi:simple-pane-cursor @tool.board) nil))


;; Painting Tools
;; Class Definition

(defclass paint-tool (board-tool)
  ((brush           :initform      nil)
   (diameter        :initform      1)
   (old-pixels      :initform      nil
                    :documentation "The old PIXELS before last update.")
   (new-pixels      :initform      (make-pixels)
                    :documentation "New pixels being added this time.

It stores the result of GENERATE-PIXELS, and will be merged into PIXELS.")
   (pixels          :initform      (make-pixels)
                    :documentation "All pixels being drawn.")
   (updated-pixels  :initform      nil
                    :documentation "Pixels that have been modified since last update.

New pixels added into PIXELS slot will be collected, and pixels that
have been removed from PIXELS slot will be collected with it's
original color value.")
   (original-pixels :initform      (make-pixels)
                    :documentation "Original pixels and colors of PIXELS.")
   (track           :initform      nil)
   (paint-option :initform '(:foreground :background :character)))
  (:optimize-slot-access nil))

;; Generic Functions

(defgeneric generate-pixels (tool new-x new-y)
  (:documentation "Generate new pixels.

This generic function will be called each time user make movement. It
should return new pixels generated.")
  (:method ((tool paint-tool) x y)
   (with-slots (board track) tool
     (multiple-value-bind (x0 y0) (translate-position board x y)
       (let ((pixels (make-pixels))
             (option @tool.paint-option)
             (blank-pixel (make-pixel :char #\Space)))
         (flet ((modify-and-add (x y)
                  (let ((pixel (multiple-value-bind (pixel found)
                                   (find-pixel x y @tool.original-pixels)
                                 (or pixel (unless found (get-pixel x y board)) (make-pixel)))))
                    (when (member :background option)
                      (setf (pixel-bg pixel) @board.bg))
                    (when (member :foreground option)
                      (setf (pixel-fg pixel) @board.fg))
                    (when (member :character option)
                      (setf (pixel-char pixel)        @board.char
                            (pixel-bold-p pixel)      @board.bold-p
                            (pixel-italic-p pixel)    @board.italic-p
                            (pixel-underline-p pixel) @board.underline-p))
                    (add-pixel x y pixel pixels))))
           (if track
             (multiple-value-bind (x1 y1) (apply #'translate-position board (car (last track)))
               (let ((line (line-pixels blank-pixel x1 y1 x0 y0)))
                 (loop-pixels line
                   (modify-and-add %x %y))))
             (modify-and-add x0 y0)))
         pixels)))))

(defgeneric update-pixels (tool new-x new-y)
  (:method ((tool paint-tool) new-x new-y)
   (with-slots (board pixels old-pixels new-pixels updated-pixels original-pixels track) tool
     (setf old-pixels     (copy-pixels pixels)
           new-pixels     (or (generate-pixels tool new-x new-y) (make-pixels))
           pixels         (nunion-pixels pixels new-pixels)
           updated-pixels (make-pixels))
     (push-end (list new-x new-y) track)
     (loop-pixels old-pixels
       (unless (equalp (find-pixel %x %y pixels) %pixel)
         (add-pixel %x %y (find-pixel %x %y original-pixels) updated-pixels)))
     (loop-pixels pixels
       (unless (equalp (find-pixel %x %y old-pixels) %pixel)
         (add-pixel %x %y %pixel updated-pixels)
         (unless (second (multiple-value-list (nfind-pixel %x %y original-pixels)))
           (add-pixel %x %y (get-pixel %x %y board) original-pixels)))))))

(defgeneric end-stroke (tool)
  (:method ((tool paint-tool))
   (with-slots (old-pixels new-pixels pixels updated-pixels original-pixels track) tool
     (setf old-pixels      nil
           new-pixels      (make-pixels)
           pixels          (make-pixels)
           updated-pixels  nil
           original-pixels (make-pixels)
           track           nil))))

(defgeneric tool-paint (tool)
  (:method ((tool paint-tool))
   (with-slots (board updated-pixels) tool
     (when (and updated-pixels (pixels-not-empty-p updated-pixels))
       (paint-pixels board updated-pixels)))))

(defmethod tool-press ((tool paint-tool) x y)
  (update-pixels tool x y)
  (tool-paint tool)
  (move-cursor-pixelwise (tool-board tool) x y))

(defmethod tool-drag ((tool paint-tool) x y)
  (update-pixels tool x y)
  (tool-paint tool)
  (move-cursor-pixelwise (tool-board tool) x y))

(defmethod tool-release ((tool paint-tool) x y)
  (with-slots (board original-pixels) tool
    (ring-push original-pixels @board.current-layer.undo-ring)
    (end-stroke tool)
    (set-message
     @tool.default-message
     (capi:element-interface @tool.board))))

(defmethod tool-motion ((tool paint-tool) x y)
  (when (pixels-not-empty-p @tool.pixels)
    (tool-release tool x y))
  (move-cursor-pixelwise @tool.board x y))

(defmethod tool-cleanup ((tool paint-tool))
  (with-slots (board original-pixels track) tool
    (when track
      (ring-push original-pixels @board.current-layer.undo-ring))
    (end-stroke tool)))

(defmethod tool-arrow-key ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (editor-pane-point board))
           (x (* (point-column point) @board.char-width))
           (y (* (point-linenum point) @board.char-height)))
      (case (sys:gesture-spec-data key)
        ((or :up 112)
         (decf y @board.char-height))
        ((or :down 110)
         (incf y @board.char-height))
        ((or :left 98)
         (decf x @board.char-width))
        ((or :right 102)
         (incf x @board.char-width)))
      (setq x (clamp x 0 (* @board.width @board.char-width))
            y (clamp y 0 (* @board.height @board.char-height)))
      (when track
        (tool-release tool x y))
      (tool-motion tool x y))))

(defmethod tool-meta-arrow ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (editor-pane-point board))
           (x (* (point-column point) @board.char-width))
           (y (* (point-linenum point) @board.char-height)))
      (unless track
        (tool-press tool x y))
      (case (sys:gesture-spec-data key)
        ((or :up 112)
         (decf y @board.char-height))
        ((or :down 110)
         (incf y @board.char-height))
        ((or :left 98)
         (decf x @board.char-width))
        ((or :right 102)
         (incf x @board.char-width)))
      (setq x (clamp x 0 (* @board.width @board.char-width))
            y (clamp y 0 (* @board.height @board.char-height)))
      (tool-drag tool x y))))

(defmethod tool-line-start ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (editor-pane-point board))
           (y (* (point-linenum point) @board.char-height)))
      (if track
        (tool-drag tool 0 y)
        (tool-motion tool 0 y)))))

(defmethod tool-line-end ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (editor-pane-point board))
           (x (* @board.width @board.char-width))
           (y (* (point-linenum point) @board.char-height)))
      (if track
        (tool-drag tool x y)
        (tool-motion tool x y)))))

(defmethod tool-escape ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (when track
      (let* ((point (editor-pane-point board))
             (x (* (point-column point) @board.char-width))
             (y (* (point-linenum point) @board.char-height)))
        (tool-release tool x y)))))


;; Brush & Eraser

(defclass brush (paint-tool) ()
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Write one:  ⏎ 		Erase one:  ⌫ 		Start:  ⌃Space "
                     :properties '((0 11 (face editor::default))
                                   (11 14 (face hl-background))
                                   (14 27 (face editor::default))
                                   (27 30 (face hl-background))
                                   (30 39 (face editor::default))
                                   (39 47 (face hl-background))))))

(defclass eraser (paint-tool) ()
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Write one:  ⏎ 		Erase one:  ⌫ 		Start:  ⌃Space "
                     :properties '((0 11 (face editor::default))
                                   (11 14 (face hl-background))
                                   (14 27 (face editor::default))
                                   (27 30 (face hl-background))
                                   (30 39 (face editor::default))
                                   (39 47 (face hl-background))))))

(defmethod generate-pixels ((tool eraser) x y)
  (with-slots (board track) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (let ((pixels (make-pixels))
            (option @tool.paint-option)
            (blank-pixel (make-pixel :char #\Space)))
        (flet ((erase-and-add (x y)
                 (let ((pixel (find-pixel x y pixels)))
                   (if pixel
                     (progn
                       (when (member :background option)
                         (setf (pixel-bg pixel) nil))
                       (when (member :foreground option)
                         (setf (pixel-fg pixel) nil))
                       (when (member :character option)
                         (setf (pixel-char pixel) #\Space))
                       (add-pixel x y pixel pixels))
                     (add-pixel x y blank-pixel pixels)))))
          (if track
            (multiple-value-bind (x1 y1) (apply #'translate-position board (car (last track)))
              (let ((line (line-pixels blank-pixel x1 y1 x0 y0)))
                (loop-pixels line
                  (erase-and-add %x %y))))
            (erase-and-add x0 y0)))
        pixels))))

(defclass brush-preview (capi:pinboard-object need-invalidate-after-style-change)
  ((tool :initarg :tool)))

(defmethod capi:draw-pinboard-object (port (self brush-preview) &key)
  (let* ((board @(capi:element-interface port).board)
         (font (gp:find-best-font
                port
                (gp:make-font-description :family @board.project.font-family
                                          :size *default-font-size*
                                          :weight (if @board.bold-p :bold :normal)
                                          :slant (if @board.italic-p :italic :roman))))
         (width (gp:get-font-width port font))
         (height (gp:get-font-height port font))
         (ascent (gp:get-font-ascent port font))
         (descent (gp:get-font-descent port font)))
    (capi:set-hint-table self (list :visible-min-height (* height 6)
                                    :visible-max-height t))
    (capi:with-geometry self
      (let* ((cx (+ capi:%x% (/ capi:%width% 2)))
             (cy (+ capi:%y% (/ capi:%height% 2)))
             (rows (loop for i from -1 to 4
                         collect (+ (- cy descent ascent descent)
                                    (* height i))))
             (eraser-p (typep @self.tool 'eraser))
             (fg (if eraser-p
                   (if @board.fg (term-color-spec-with-alpha @board.fg) *default-foreground*)
                   (if (capi:top-level-interface-dark-mode-p (capi:element-interface port))
                     :gray40 :gray60)))
             (bg (if eraser-p
                   (if @board.bg (term-color-spec-with-alpha @board.bg) *default-background*)
                   nil))
             (char #\§))
        (gp:draw-rectangle port (1+ capi:%x%) (1+ capi:%y%) (- capi:%width% 2) (- capi:%height% 2)
                           :foreground :transparent ;*default-background*
                           :filled t)
        (dotimes (y 6)
          (dorange (x -10 10)
            (gp:draw-character port char (+ cx (* width x)) (nth y rows)
                               :foreground fg
                               :background bg
                               :font font
                               :block t)))
        #|(gp:draw-rectangle port (1+ capi:%x%) (1+ capi:%y%) (- capi:%width% 2) (- capi:%height% 2)
                           :foreground (if (capi:top-level-interface-dark-mode-p (capi:element-interface port))
                                         :white :black)
                           :thickness 2)|#
        (when (member :foreground @self.tool.paint-option)
          (if eraser-p
            (setq fg *default-foreground*)
            (setq fg (if @board.fg (term-color-spec-with-alpha @board.fg) *default-foreground*))))
        (when (member :background @self.tool.paint-option)
          (if eraser-p
            (setq bg *default-background*)
            (setq bg (if @board.bg (term-color-spec-with-alpha @board.bg) *default-background*))))
        (when (member :character @self.tool.paint-option)
          (if eraser-p
            (setq char #\Space)
            (setq char @board.char)))
        (loop for x from -8 below 8
              for y in '(3 3 2 2 2 2 2 3 4 5 5 5 5 5 4 4)
              do (gp:draw-character port char (+ cx (* width x)) (nth (1- y) rows)
                                    :foreground fg
                                    :background bg
                                    :font font
                                    :block t))))))

(defmethod make-settings-layout (itf (tool brush))
  (let ((brush-preview (make 'brush-preview :tool tool)))
    (make 'capi:column-layout
          :adjust :center
          :description
          (list (make 'capi:check-button-panel
                      :title              "Paint:"
                      :title-position     :left
                      :layout-class       'capi:column-layout
                      :items              '(:foreground :background :character)
                      :print-function     #'string-capitalize
                      :callback-type      :data
                      :selected-items     @tool.paint-option
                      :selection-callback (op (pushnew _ @tool.paint-option)
                                            (capi:redraw-pinboard-object brush-preview))
                      :retract-callback   (op (setf @tool.paint-option (delete _ @tool.paint-option))
                                            (capi:redraw-pinboard-object brush-preview)))
                (make 'capi:simple-pinboard-layout
                      :visible-min-height '(character 12)
                      :background         :transparent
                      :description        (list brush-preview))
                (make #+lispworks8.1 'capi:dummy-pane
                      #-lispworks8.1 'capi::dumm-pane)))))

(defmethod make-settings-layout (itf (tool eraser))
  (let ((brush-preview (make 'brush-preview :tool tool)))
    (make 'capi:column-layout
          :adjust :center
          :description
          (list (make 'capi:check-button-panel
                      :title              "Erase:"
                      :title-position     :left
                      :layout-class       'capi:column-layout
                      :items              '(:foreground :background :character)
                      :print-function     #'string-capitalize
                      :callback-type      :data
                      :selected-items     @tool.paint-option
                      :selection-callback (op (pushnew _ @tool.paint-option)
                                            (capi:redraw-pinboard-object brush-preview))
                      :retract-callback   (op (setf @tool.paint-option (delete _ @tool.paint-option))
                                            (capi:redraw-pinboard-object brush-preview)))
                (make 'capi:simple-pinboard-layout
                      :visible-min-height '(character 12)
                      :background         :transparent
                      :description        (list brush-preview))
                (make #+lispworks8.1 'capi:dummy-pane
                      #-lispworks8.1 'capi::dumm-pane)))))


;; Stroke

(defclass stroke (paint-tool)
  ((charset :initform :ascii)
   (arrow-p :initform nil)
   (connect-surroundings :initform nil))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Start:  ⌃Space 		Change direction:  ⇧ ←↑↓→ 		Draw arrow:  ⌃⇧ ←↑↓→"
                     :properties '(( 0  7 (face editor::default))
                                   ( 7 15 (face hl-background))
                                   (15 35 (face editor::default))
                                   (35 43 (face hl-background))
                                   (43 57 (face editor::default))
                                   (57 65 (face hl-background))))))

(defclass selector-board (capi:pinboard-layout)
  ((tool :initarg :tool)
   (hover :initform nil)
   (refresh-timer :initform nil))
  (:default-initargs
   :background :transparent
   :description
   (list (make 'capi:grid-layout
               :columns 2
               :description
               (list (make 'stroke-tool-selector :charset :ascii       :selected t)
                     (make 'stroke-tool-selector :charset :ascii       :arrow-p t)
                     (make 'stroke-tool-selector :charset :box-light)
                     (make 'stroke-tool-selector :charset :box-light   :arrow-p t)
                     (make 'stroke-tool-selector :charset :box-heavy)
                     (make 'stroke-tool-selector :charset :box-heavy   :arrow-p t)
                     (make 'stroke-tool-selector :charset :box-rounded)
                     (make 'stroke-tool-selector :charset :box-rounded :arrow-p t)
                     (make 'stroke-tool-selector :charset :box-double)
                     (make 'stroke-tool-selector :charset :box-double  :arrow-p t))))
   :resize-callback
   (lambda (self x y w h)
     (setf (capi:static-layout-child-geometry
            (first (capi:layout-description self)))
           (values x y w h)))
   :input-model `(((:button-1 :release)
                   ,(lambda (pane x y)
                      (when-let (obj (capi:pinboard-object-at-position pane x y))
                                             
                        (let ((old-obj (find-if (lambda (obj)
                                                  (and (eql @obj.charset @pane.tool.charset)
                                                       (eql @obj.arrow-p @pane.tool.arrow-p)))
                                                (capi:layout-description
                                                 (car (capi:layout-description pane))))))
                          (when old-obj
                            (setf @old-obj.selected nil)
                            (capi:redraw-pinboard-object old-obj nil))
                          (setf @pane.tool.charset @obj.charset
                                @pane.tool.arrow-p @obj.arrow-p
                                @obj.selected t)
                          (capi:redraw-pinboard-object obj)))))
                  (:motion ,(lambda (pane x y)
                              (when-let (obj (capi:pinboard-object-at-position pane x y))
                                (when @pane.hover
                                  (capi:unhighlight-pinboard-object pane @pane.hover))
                                (capi:highlight-pinboard-object pane obj)
                                (setf @pane.hover obj)
                                (gp:invalidate-rectangle pane)
                                (mp:schedule-timer-relative @pane.refresh-timer 1 1)))))))

(defmethod initialize-instance :after ((self selector-board) &key)
  (setf @self.refresh-timer
        (mp:make-timer
         (lambda ()
           (unless (capi:pane-has-focus-p self)
             (capi:apply-in-pane-process self #'capi:unhighlight-pinboard-object self @self.hover)
             (setf @self.hover nil)
             :stop)))))

(defclass stroke-tool-selector (capi:pinboard-object need-invalidate-after-style-change)
  ((charset :initform :ascii
            :initarg :charset)
   (selected :initform nil
             :initarg :selected)
   (arrow-p :initform nil
            :initarg :arrow-p)))

(defmethod capi:draw-pinboard-object (port (self stroke-tool-selector) &key)
  (capi:with-geometry self
    (let* ((itf (capi:element-interface port))
           (board @itf.board)
           (fg (if (or @self.selected
                       (capi:pinboard-object-highlighted-p self))
                 (if (member :foreground @board.current-tool.paint-option)
                   (if @board.fg (term-color-spec-with-alpha @board.fg) *default-foreground*)
                   (if (capi:top-level-interface-dark-mode-p itf)
                     :gray60 :gray40))
                 (if (capi:top-level-interface-dark-mode-p itf)
                   :gray60 :gray40)))
           (bg (if (member :background @board.current-tool.paint-option)
                 (if @board.bg (term-color-spec-with-alpha @board.bg) *default-background*)
                 nil))
           (font (gp:find-best-font
                  port
                  (gp:make-font-description :family @board.project.font-family
                                            :size *default-font-size*
                                            :weight (if @board.bold-p :bold :normal)
                                            :slant (if @board.italic-p :italic :roman)))))
      (gp:draw-rectangle port capi:%x% capi:%y% capi:%width% capi:%height%
                         :foreground :transparent ;*default-background*
                         :filled t)
      (gp:with-graphics-state (port :foreground fg :background bg :font font)
        (let* ((h (charset-get @self.charset :h))
               (lb (charset-get @self.charset :lb))
               (rt (charset-get @self.charset :rt))
               (end (if @self.arrow-p (charset-get @self.charset :arr-r) h))
               (width (gp:get-font-width port font))
               (ascent (gp:get-font-ascent port font))
               (descent (gp:get-font-descent port font))
               (height (gp:get-font-height port font))
               (cx (+ capi:%x% (/ capi:%width% 2)))
               (cy (+ capi:%y% (/ capi:%height% 2)))
               (line1-y (- cy descent))
               (line2-y (+ cy ascent))
               (line1-x (- cx (* width 7/2)))
               (line2-x (- cx (/ width 2))))
          (gp:with-graphics-mask (port (list 0 (- cy height) (+ capi:%x% capi:%width%) (+ cy height)))
            (gp:draw-character port h   line1-x                 line1-y :block t)
            (gp:draw-character port h   (+ line1-x width)       line1-y :block t)
            (gp:draw-character port h   (+ line1-x (* width 2)) line1-y :block t)
            (gp:draw-character port lb  (+ line1-x (* width 3)) line1-y :block t)
            (gp:draw-character port rt  line2-x                 line2-y :block t)
            (gp:draw-character port h   (+ line2-x width)       line2-y :block t)
            (gp:draw-character port h   (+ line2-x (* width 2)) line2-y :block t)
            (gp:draw-character port end (+ line2-x (* width 3)) line2-y :block t))))
      (gp:draw-rectangle port (1+ capi:%x%) (1+ capi:%y%) (- capi:%width% 2) (- capi:%height% 2)
                         :foreground (if (or @self.selected
                                             (capi:pinboard-object-highlighted-p self))
                                       (if (capi:top-level-interface-dark-mode-p itf)
                                         :white :black)
                                       (if (capi:top-level-interface-dark-mode-p itf)
                                         :gray40 :gray60))
                         :thickness 2))))

(defmethod capi:draw-pinboard-object-highlighted (port (self stroke-tool-selector) &key)
  (capi:draw-pinboard-object port self))

(defmethod make-settings-layout (itf (tool stroke))
  (let ((selector-board (make 'selector-board :tool tool)))
    (make 'capi:column-layout
          :adjust :center
          :description
          (list (make 'capi:check-button-panel
                      :title "Paint:" :title-position :left
                      :layout-class 'capi:column-layout
                      :items '(:foreground :background)
                      :print-function #'string-capitalize
                      :callback-type :data
                      :selected-items @tool.paint-option
                      :selection-callback (op (pushnew _ @tool.paint-option)
                                            (capi:with-geometry selector-board
                                              (capi:redraw-pinboard-layout selector-board 0 0 capi:%width% capi:%height%)))
                      :retract-callback (op (setf @tool.paint-option (delete _ @tool.paint-option))
                                          (capi:with-geometry selector-board
                                            (capi:redraw-pinboard-layout selector-board 0 0 capi:%width% capi:%height%))))
                (make 'capi:check-button
                      :selected @tool.connect-surroundings
                      :text "Connect surrounding characters"
                      :callback-type :none
                      :selection-callback (op (setf @tool.connect-surroundings t))
                      :retract-callback (op (setf @tool.connect-surroundings nil)))
                selector-board))))

(defmethod update-pixels ((tool stroke) new-x new-y)
  (with-slots (board pixels old-pixels new-pixels updated-pixels original-pixels track charset) tool
    (setf old-pixels     (copy-pixels pixels)
          new-pixels     (or (generate-pixels tool new-x new-y) (make-pixels)))
    (when @tool.connect-surroundings
      (let ((new (make-pixels)))
        (loop-pixels new-pixels
          (loop for x in (list (1- %x) (1+ %x) %x %x)
                for y in (list %y %y (1- %y) (1+ %y))
                unless (or (nfind-pixel x y new-pixels)
                           (nfind-pixel x y old-pixels))
                  do (when-let (pixel (get-pixel x y board))
                       (add-pixel x y pixel new))))
        (nunion-pixels new-pixels new)))
    (setf pixels         (join-pixels charset (nunion-pixels pixels new-pixels))
          updated-pixels (make-pixels))
    (when @tool.arrow-p
      (multiple-value-bind (x y) (translate-position board new-x new-y)
        (let ((u (union-pixels pixels new-pixels))
              char)
          (cond ((nfind-pixel x (1- y) u)
                 (setq char (charset-get charset :arr-b)))
                ((nfind-pixel x (1+ y) u)
                 (setq char (charset-get charset :arr-t)))
                ((or (nfind-pixel (1- x) (1- y) u)
                     (nfind-pixel (1- x) y u)
                     (nfind-pixel (1- x) (1+ y) u))
                 (setq char (charset-get charset :arr-r)))
                ((or (nfind-pixel (1+ x) (1- y) u)
                     (nfind-pixel (1+ x) y u)
                     (nfind-pixel (1+ x) (1+ y) u))
                 (setq char (charset-get charset :arr-l))))
          (when char
            (add-pixel x y (make-pixel :char char :fg @board.fg :bg @board.bg
                                       :bold-p @board.bold-p :italic-p @board.italic-p :underline-p @board.underline-p)
                       pixels)))))
    (push-end (list new-x new-y) track)
    (loop-pixels old-pixels
      (unless (nfind-pixel %x %y pixels)
        (add-pixel %x %y (nfind-pixel %x %y original-pixels) updated-pixels)))
    (loop-pixels pixels
      (unless (equalp (find-pixel %x %y old-pixels) %pixel)
        (add-pixel %x %y %pixel updated-pixels)
        (unless (second (multiple-value-list (nfind-pixel %x %y original-pixels)))
          (add-pixel %x %y (get-pixel %x %y board) original-pixels))))))

(defmethod tool-shift-arrow ((tool stroke) x y key)
  (with-slots (board charset) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x (+ x-offset (point-column point)))
             (y (+ y-offset (point-linenum point)))
             (adjacent-pixels (make-pixels))
             (char (charset-get charset :cross)))
        (add-pixel (1- x) y (get-pixel (1- x) y board) adjacent-pixels)
        (add-pixel (1+ x) y (get-pixel (1+ x) y board) adjacent-pixels)
        (add-pixel x (1- y) (get-pixel x (1- y) board) adjacent-pixels)
        (add-pixel x (1+ y) (get-pixel x (1+ y) board) adjacent-pixels)
        (add-pixel x y (make-pixel :fg @board.fg :bg @board.bg) adjacent-pixels)
        (case (sys:gesture-spec-data key)
          (:up (add-pixel x (1- y) (make-pixel :char char) adjacent-pixels))
          (:down (add-pixel x (1+ y) (make-pixel :char char) adjacent-pixels))
          (:left (add-pixel (1- x) y (make-pixel :char char) adjacent-pixels))
          (:right (add-pixel (1+ x) y (make-pixel :char char) adjacent-pixels)))
        (let ((original-pixels (make-pixels)))
          (add-pixel x y (get-pixel x y board) original-pixels)
          (ring-push original-pixels @board.current-layer.undo-ring))
        (paint-pixel board x y (find-pixel x y (join-pixels charset adjacent-pixels)))
        (move-cursor board x y)))))

(defmethod tool-ctrl-shift-arrow ((tool stroke) x y key)
  (with-slots (board charset) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x (+ x-offset (point-column point)))
             (y (+ y-offset (point-linenum point))))
        (let ((original-pixels (make-pixels)))
          (add-pixel x y (get-pixel x y board) original-pixels)
          (ring-push original-pixels @board.current-layer.undo-ring))
        (paint-pixel board x y
                     (make-pixel :char (charset-get charset (case (sys:gesture-spec-data key)
                                                              (:up :arr-t)
                                                              (:down :arr-b)
                                                              (:left :arr-l)
                                                              (:right :arr-r)))
                                 :fg @board.fg :bg @board.bg
                                 :bold-p @board.bold-p :italic-p @board.italic-p :underline-p @board.underline-p))
        (move-cursor board x y)))))

(defclass rectangle (paint-tool)
  ((border-style :initform :ascii)
   (border-foreground :initform :fill)
   (border-background :initform :fill)
   (border-character :initform :fill)
   (filling-foreground :initform :none)
   (filling-background :initform :none)
   (filling-character :initform :none))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Start:  ⌃Space "
                     :properties '((0 7 (face editor::default))
                                   (7 15 (face hl-background))))))

(defclass rectangle-preview (capi:pinboard-object need-invalidate-after-style-change)
  ((tool :initarg :tool))
  (:default-initargs
   :visible-min-height '(character 12)))

(defmethod capi:draw-pinboard-object (port (self rectangle-preview) &key)
  (let* ((itf (capi:element-interface port))
         (board @itf.board)
         (fg (if @board.fg (term-color-spec-with-alpha @board.fg) *default-foreground*))
         (alt-fg (if (capi:top-level-interface-dark-mode-p itf)
                   :gray40 :gray60))
         (bg (if @board.bg (term-color-spec-with-alpha @board.bg) *default-background*))
         (font (gp:find-best-font
                board
                (gp:make-font-description :family @board.project.font-family
                                          :size *default-font-size*
                                          :weight (if @board.bold-p :bold :normal)
                                          :slant (if @board.italic-p :italic :roman))))
         (width (gp:get-font-width port font))
         (height (gp:get-font-height port font))
         (descent (gp:get-font-descent port font)))
    (capi:set-hint-table self (list :visible-min-height (* height 6)
                                    :visible-max-height t))
    (capi:with-geometry self
      (gp:with-graphics-state (port :font font)
        (gp:draw-rectangle port capi:%x% capi:%y% capi:%width% capi:%height%
                           :foreground :transparent ;*default-background*
                           :filled t)
        (let* ((cx (+ capi:%x% (/ capi:%width% 2)))
               (cy (+ capi:%y% (/ capi:%height% 2)))
               (start-x (- cx (* 9 width)))
               (start-y (- cy descent (* 2 height)))
               v h lt lb rt rb stroke-fg stroke-bg
               fill fill-fg fill-bg)
          (case @self.tool.border-character
            (:none (setq v #\§ h #\§ lt #\§ lb #\§ rt #\§ rb #\§))
            (:clear (setq v #\Space h #\Space lt #\Space lb #\Space rt #\Space rb #\Space))
            (:fill (case @self.tool.border-style
                     (:default (setq v @board.char h @board.char lt @board.char lb @board.char rt @board.char rb @board.char))
                     (t (let ((it @self.tool.border-style))
                          (setq v (charset-get it :v) h (charset-get it :h)
                                lt (charset-get it :lt) lb (charset-get it :lb)
                                rt (charset-get it :rt) rb (charset-get it :rb)))))))
          (case @self.tool.border-foreground
            (:none (setq stroke-fg alt-fg))
            (:clear (setq stroke-fg *default-foreground*))
            (:fill (setq stroke-fg fg)))
          (case @self.tool.border-background
            (:none (setq stroke-bg nil))
            (:clear (setq stroke-bg *default-background*))
            (:fill (setq stroke-bg bg)))
          (case @self.tool.filling-character
            (:none (setq fill #\§))
            (:clear (setq fill #\Space))
            (:fill (setq fill @board.char)))
          (case @self.tool.filling-foreground
            (:none (setq fill-fg alt-fg))
            (:clear (setq fill-fg *default-foreground*))
            (:fill (setq fill-fg fg)))
          (case @self.tool.filling-background
            (:none (setq fill-bg nil))
            (:clear (setq fill-bg *default-background*))
            (:fill (setq fill-bg bg)))
          (dotimes (y 6)
            (dotimes (x 18)
              (if (and (< 1 y 4) (< 2 x 15))
                (gp:draw-character port fill (+ start-x (* x width)) (+ start-y (* y height))
                                   :foreground fill-fg :background fill-bg :block t)
                (unless (and (<= 1 y 4) (<= 2 x 15))
                  (gp:draw-character port #\§ (+ start-x (* x width)) (+ start-y (* y height))
                                     :foreground alt-fg :background nil :block t)))))
          #|(gp:draw-rectangle port (1+ capi:%x%) (1+ capi:%y%) (- capi:%width% 2) (- capi:%height% 2)
                             :foreground (if (capi:top-level-interface-dark-mode-p itf)
                                           :white :black)
                             :thickness 2)|#
          (dorange (x 3 15)
            (gp:draw-character port h (+ start-x (* x width)) (+ start-y height)
                               :foreground stroke-fg :background stroke-bg :block t)
            (gp:draw-character port h (+ start-x (* x width)) (+ start-y (* 4 height))
                               :foreground stroke-fg :background stroke-bg :block t))
          (dorange (y 2 4)
            (gp:draw-character port v (+ start-x (* 2 width)) (+ start-y (* y height))
                               :foreground stroke-fg :background stroke-bg :block t)
            (gp:draw-character port v (+ start-x (* 15 width)) (+ start-y (* y height))
                               :foreground stroke-fg :background stroke-bg :block t))
          (gp:draw-character port rb (+ start-x (* 2 width)) (+ start-y height)
                             :foreground stroke-fg :background stroke-bg :block t)
          (gp:draw-character port lb (+ start-x (* 15 width)) (+ start-y height)
                             :foreground stroke-fg :background stroke-bg :block t)
          (gp:draw-character port rt (+ start-x (* 2 width)) (+ start-y (* 4 height))
                             :foreground stroke-fg :background stroke-bg :block t)
          (gp:draw-character port lt (+ start-x (* 15 width)) (+ start-y (* 4 height))
                             :foreground stroke-fg :background stroke-bg :block t))))))

(defmethod make-settings-layout (itf (tool rectangle))
  (let* ((preview (make 'rectangle-preview :tool tool)))
    (make-instance
     'capi:column-layout
     :adjust :center
     :description
     (list (make 'capi:option-pane
                 :title "Border Style:"
                 :title-position :left
                 :visible-max-width :text-width
                 :items (serapeum:plist-keys *charsets*)
                 :selected-item @tool.border-style
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.border-style _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Border Foreground:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.border-foreground
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.border-foreground _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Border Background:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.border-background
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.border-background _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Border Character:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.border-character
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.border-character _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Filling Foreground:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.filling-foreground
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.filling-foreground _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Filling Background:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.filling-background
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.filling-background _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:radio-button-panel
                 :title "Filling Character:"
                 :title-adjust :center
                 :items '(:clear :fill :none)
                 :selected-item @tool.filling-character
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf @tool.filling-character _)
                                       (capi:redraw-pinboard-object preview)))
           (make 'capi:simple-pinboard-layout
                 :background :transparent
                 :description (list preview))))))

(defmethod generate-pixels ((tool rectangle) x y)
  (with-slots (board pixels track original-pixels
                     border-style border-foreground border-background border-character
                     filling-foreground filling-background filling-character) tool
    (multiple-value-bind (x2 y2) (translate-position board x y)
      (if (car track)
        (multiple-value-bind (x1 y1) (apply #'translate-position board (car track))
          (flet ((set-border-pixel (x y charset-char pixels)
                   (let ((pixel (make-pixel))
                         (orig (multiple-value-bind (pixel found)
                                   (nfind-pixel x y original-pixels)
                                 (or pixel (unless found (get-pixel x y board))))))
                     (setf (pixel-fg pixel) (case border-foreground
                                              (:none (when orig (pixel-fg orig)))
                                              (:clear nil)
                                              (:fill @board.fg))
                           (pixel-bg pixel) (case border-background
                                              (:none (when orig (pixel-bg orig)))
                                              (:clear nil)
                                              (:fill @board.bg)))
                     (case border-character
                       (:none (setf (pixel-char pixel) (if orig (pixel-char orig) #\Space)))
                       (:clear (setf (pixel-char pixel) #\Space))
                       (:fill
                        (if (eql border-style :default)
                          (setf (pixel-char pixel) @board.char)
                          (setf (pixel-char pixel) charset-char))
                        (setf (pixel-bold-p pixel) @board.bold-p
                              (pixel-italic-p pixel) @board.italic-p
                              (pixel-underline-p pixel) @board.underline-p)))
                     (add-pixel x y pixel pixels))))
            (if (or (= x1 x2) (= y1 y2))
              (let ((new-pixels (line-pixels nil x1 y1 x2 y2)))
                (setf pixels (make-pixels))
                (loop-pixels new-pixels
                  (set-border-pixel %x %y (charset-get border-style (if (= x1 x2) :v :h)) pixels)))
              (let ((x1 (min x1 x2))
                    (x2 (max x1 x2))
                    (y1 (min y1 y2))
                    (y2 (max y1 y2))
                    (h-pixels (nunion-pixels
                               (line-pixels nil x1 y1 x2 y1)
                               (line-pixels nil x1 y2 x2 y2)))
                    (v-pixels (nunion-pixels
                               (line-pixels nil x1 y1 x1 y2)
                               (line-pixels nil x2 y1 x2 y2))))
                (setf pixels (make-pixels))
                (loop-pixels h-pixels
                  (set-border-pixel %x %y (charset-get border-style :h) pixels))
                (loop-pixels v-pixels
                  (set-border-pixel %x %y (charset-get border-style :v) pixels))
                (set-border-pixel x1 y1 (charset-get border-style :rb) pixels)
                (set-border-pixel x2 y1 (charset-get border-style :lb) pixels)
                (set-border-pixel x1 y2 (charset-get border-style :rt) pixels)
                (set-border-pixel x2 y2 (charset-get border-style :lt) pixels)
                (dorange$fixnum (y (1+ y1) y2)
                  (dorange$fixnum (x (1+ x1) x2)
                    (let ((pixel (make-pixel))
                          (orig (multiple-value-bind (pixel found)
                                    (nfind-pixel x y original-pixels)
                                  (or pixel (unless found (get-pixel x y board))))))
                      (setf (pixel-fg pixel) (case filling-foreground
                                               (:none (when orig (pixel-fg orig)))
                                               (:clear nil)
                                               (:fill @board.fg))
                            (pixel-bg pixel) (case filling-background
                                               (:none (when orig (pixel-bg orig)))
                                               (:clear nil)
                                               (:fill @board.bg)))
                      (case filling-character
                        (:none (if orig 
                                 (setf (pixel-char pixel) (pixel-char orig)
                                       (pixel-bold-p pixel) (pixel-bold-p orig)
                                       (pixel-italic-p pixel) (pixel-italic-p orig)
                                       (pixel-underline-p pixel) (pixel-underline-p orig))
                                 (setf (pixel-char pixel) #\Space)))
                        (:clear (setf (pixel-char pixel) #\Space))
                        (:fill (setf (pixel-char pixel) @board.char
                                     (pixel-bold-p pixel) @board.bold-p
                                     (pixel-italic-p pixel) @board.italic-p
                                     (pixel-underline-p pixel) @board.underline-p)))
                      (add-pixel x y pixel pixels))))))))
        (add-pixel x2 y2 (make-pixel :char #\Space) pixels))
      (make-pixels))))


;; Select

(defclass select (board-tool)
  ((left              :initform nil)
   (top               :initform nil)
   (right             :initform nil)
   (bottom            :initform nil)
   (pixels            :initform nil)
   (origin            :initform nil)
   (start-x           :initform nil)
   (start-y           :initform nil)
   (start-dx          :initform nil)
   (start-dy          :initform nil)
   (dx                :initform 0)
   (dy                :initform 0)
   (mouse-down        :initform nil)

   (select-background :initform nil)
   (fill              :initform ()))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Drag or press  ⌥ + ←↑↓→  to select area"
                     :properties '((0 14 (face editor::default))
                                   (14 24 (face hl-background))
                                   (24 39 (face editor::default))))))

(defmethod set-tool-internal :before (itf board (name select))
  (setf (capi:simple-pane-cursor board) :crosshair))

(defmethod make-settings-layout (itf (tool select))
  (let ((clear-button (make 'capi:push-button
                            :text "Clear Filling"
                            :enabled nil
                            :callback-type :element
                            :callback (op (setf @tool.fill nil
                                                (capi:button-enabled _) nil)
                                        (refresh-selected-pixels tool)))))
    (make 'capi:column-layout
          :adjust :center
          :description
          (list (make 'capi:check-button
                      :text "Include background"
                      :callback-type :none
                      :selection-callback (op (setf @tool.select-background t))
                      :retract-callback   (op (setf @tool.select-background nil)))
                (make 'capi:check-button-panel
                      :title "Filling:" :title-position :left
                      :layout-class 'capi:column-layout
                      :print-function #'string-capitalize
                      :items '(:foreground :background :character)
                      :callback-type :data
                      :selection-callback (op (pushnew _ @tool.fill)
                                         (refresh-selected-pixels tool)
                                         (symbol-macrolet ((clear (capi:button-enabled clear-button)))
                                           (unless clear (setf clear t))))
                      :retract-callback (op (deletef @tool.fill _)
                                          (refresh-selected-pixels tool)
                                          (symbol-macrolet ((clear (capi:button-enabled clear-button)))
                                           (unless clear (setf clear t)))))
                clear-button
                (make 'capi:output-pane :visible-max-height 0)))))

(defmethod tool-set-fg-hook ((tool select))
  (when (member :foreground @tool.fill)
    (refresh-selected-pixels tool)))
(defmethod tool-set-bg-hook ((tool select))
  (when (member :background @tool.fill)
    (refresh-selected-pixels tool)))
(defmethod tool-set-char-hook ((tool select))
  (when (member :character @tool.fill)
    (refresh-selected-pixels tool)))

(defun refresh-selected-pixels (tool)
  (with-slots (board dx dy pixels) tool
    (let ((new (make-pixels)))
      (loop-pixels pixels
        (if %pixel
          (let ((p (copy-pixel %pixel)))
            (when (member :foreground @tool.fill)
              (setf (pixel-fg p) @board.fg))
            (when (member :background @tool.fill)
              (setf (pixel-fg p) @board.bg))
            (when (member :character @tool.fill)
              (setf (pixel-fg p) @board.char))
            (add-pixel (+ %x dx) (+ %y dy) p new))
          (add-pixel (+ %x dx) (+ %y dy) nil new)))
      (set-selected-pixels board new))))

(defmethod tool-meta-arrow ((tool select) x y key)
  (with-slots (board left top right bottom pixels) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (when pixels (tool-escape tool x y nil))
        (unless left
          (setf left x0 top y0)
          (set-selected-pixels board (make-pixels)))
        (case (sys:gesture-spec-data key)
          ((or :up 112) (decf y0))
          ((or :down 110) (incf y0))
          ((or :left 98) (decf x0))
          ((or :right 102) (incf x0)))
        (let ((new (make-pixels)))
          (setf right x0 bottom y0)
          (dorange$fixnum (y (min top bottom) (max top bottom))
            (dorange$fixnum (x (min left right) (max left right))
              (aif (get-pixel x y board)
                   (add-pixel x y it new)
                   (when @tool.select-background
                     (add-pixel x y nil new)))))
          (set-selected-pixels board new))
        (move-cursor board x0 y0)))))

(defmethod tool-arrow-key ((tool select) x y key)
  (with-slots (board left start-x start-y start-dx start-dy pixels) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (case (sys:gesture-spec-data key)
          ((or :up 112) (decf y0))
          ((or :down 110) (incf y0))
          ((or :left 98) (decf x0))
          ((or :right 102) (incf x0)))
        (when left
          (tool-release tool x y))
        (when pixels
          (setf start-x nil start-y nil
                start-dx nil start-dy nil))
        (move-cursor board x0 y0)))))

(defmethod tool-shift-arrow ((tool select) x y key)
  (with-slots (board left start-x start-y start-dx start-dy dx dy pixels) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (when (and pixels
                   (or (null start-x)
                       (or (/= dx (+ start-dx (- x0 start-x)))
                           (/= dy (+ start-dy (- y0 start-y))))))
          (setf start-x x0 start-y y0
                start-dx dx start-dy dy))
        (case (sys:gesture-spec-data key)
          ((or :up 112) (decf y0))
          ((or :down 110) (incf y0))
          ((or :left 98) (decf x0))
          ((or :right 102) (incf x0)))
        (when left
          (tool-release tool x y))
        (when pixels
          (setf dx (+ start-dx (- x0 start-x))
                dy (+ start-dy (- y0 start-y)))
          (refresh-selected-pixels tool))
        (move-cursor board x0 y0)))))

(defmethod tool-press ((tool select) x y)
  (with-slots (board left top start-x start-y start-dx start-dy pixels dx dy mouse-down) tool
    (setf mouse-down t)
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (if pixels
        (progn
          (setf start-x  x0   start-y  y0
                start-dx dx   start-dy dy
                (capi:simple-pane-cursor board) #+darwin :closed-hand #-darwin :move))
        (setf left x0 top y0 dx 0 dy 0)))
    (set-selected-pixels board (make-pixels))))

(defmethod tool-drag ((tool select) x y)
  (with-slots (board left top right bottom start-x start-y start-dx start-dy pixels dx dy mouse-down) tool
    (unless mouse-down (setf mouse-down t))
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (if pixels
        (if start-x
          (let ((new-dx (+ start-dx (- x0 start-x)))
                (new-dy (+ start-dy (- y0 start-y)))
                (new (make-pixels)))
            (when (or (/= dx new-dx) (/= dy new-dy))
              (setf dx new-dx dy new-dy)
              (refresh-selected-pixels tool)
              (move-cursor board
                           (max (pixels-left new) @board.x-offset)
                           (max (pixels-top new) @board.y-offset))))
          (tool-press tool x y))
        (if left
          (let ((new (make-pixels)))
            (setf right x0 bottom y0)
            (dorange$fixnum (y (min top bottom) (max top bottom))
              (dorange$fixnum (x (min left right) (max left right))
                (aif (get-pixel x y board)
                     (add-pixel x y it new)
                     (when @tool.select-background
                       (add-pixel x y nil new)))))
            (set-selected-pixels board new)
            (if (pixels-not-empty-p new)
              (move-cursor board (pixels-left new) (pixels-top new))
              (move-cursor-pixelwise board x y)))
          (tool-press tool x y))))))

(defmethod tool-release ((tool select) x y)
  (with-slots (board left top right bottom start-x start-y pixels origin mouse-down) tool
    (with-slots (selected-pixels) board
      (setf left nil top nil right nil bottom nil
            start-x nil start-y nil
            mouse-down nil)
      (if pixels
        (setf (capi:simple-pane-cursor board)
              #+darwin :open-hand
              #-darwin :move)
        (if (and selected-pixels (pixels-not-empty-p selected-pixels))
          (progn
            (setf pixels (copy-pixels selected-pixels)
                  origin (copy-pixels selected-pixels))
            (loop-pixels selected-pixels
              (delete-pixel %x %y (layer-pixels @board.current-layer)))
            (move-cursor board (pixels-left selected-pixels) (pixels-top selected-pixels))
            (set-message
             (editor::make-buffer-string
              :%string "Move region:  ⇧ ←↑↓→ 		Remove region:  ⌫ 		Confirm:  ⏎ 		Discard:  esc "
              :properties '((0 13 (face editor::default))
                            (13 21 (face hl-background))
                            (21 38 (face editor::default))
                            (38 41 (face hl-background))
                            (41 52 (face editor::default))
                            (52 55 (face hl-background))
                            (55 66 (face editor::default))
                            (66 71 (face hl-background))))
             (capi:element-interface board))
            (setf (capi:simple-pane-cursor board)
                  #+darwin :open-hand
                  #-darwin :move))
          (setf selected-pixels nil))))))

(defmethod tool-motion ((tool select) x y)
  (with-slots (board pixels mouse-down) tool
    (unless pixels
      (move-cursor-pixelwise board x y))
    (when mouse-down
      (tool-release tool x y))))

(defmethod tool-cleanup ((tool select))
  (with-slots (board left top right bottom start-x start-y pixels start-dx start-dy dx dy) tool
    (set-selected-pixels board nil)
    (setf left nil top nil right nil bottom nil
          start-x nil start-y nil start-dx nil start-dy nil
          pixels nil
          dx 0 dy 0)
    (set-message @tool.default-message (capi:element-interface board))
    (setf (capi:simple-pane-cursor board) :crosshair)))

(defmethod tool-return ((tool select) x y key)
  (with-slots (board pixels) tool
    (with-slots (selected-pixels) board
      (when selected-pixels
        (let ((origin (make-pixels)))
          (loop-pixels selected-pixels
            (add-pixel %x %y (get-pixel %x %y board) origin))
          (ring-push (union-pixels origin pixels) @board.current-layer.undo-ring))
        (paint-pixels board selected-pixels)
        (tool-cleanup tool)))))

(defmethod tool-escape ((tool select) x y key)
  (when @tool.origin
    (paint-pixels @tool.board @tool.origin)
    (refresh-board @tool.board)
    (tool-cleanup tool)))

(defmethod tool-backspace :around ((tool select) x y key)
  (if @tool.pixels (tool-cleanup tool) (call-next-method)))


;; Import

(defclass import-image (board-tool)
  ((old-tool        :initform nil)
   (image           :initform nil)
   (method          :initform :1-by-1)
   (bit             :initform 24)
   (charset         :initform " .:-=+*%#@")
   (width           :initform 40)
   (height          :initform 28)
   (lock-ratio      :initform t)
   (rotate          :initform 0)
   (start-x         :initform nil)
   (start-y         :initform nil)
   (dx              :initform 0)
   (dy              :initform 0)
   (start-dx        :initform nil)
   (start-dy        :initform nil)
   (original-pixels :initform nil))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Move region:  ←↑↓→     Remove region:  ⌫     Confirm:  ⏎     Discard:  esc "
                     :properties '((0 13 (face editor::default))
                                   (13 19 (face hl-background))
                                   (19 38 (face editor::default))
                                   (38 41 (face hl-background))
                                   (41 54 (face editor::default))
                                   (54 57 (face hl-background))
                                   (57 70 (face editor::default))
                                   (70 75 (face hl-background))))))

(defmethod set-tool (itf board (name (eql 'import-image)))
  (with-slots (tools board) itf
    (with-slots (x-offset y-offset current-tool) board
      (let ((tool (find name tools :key #'class-name-of)))
        (with-slots (image dx dy old-tool width height) tool
          (multiple-value-bind (file okp)
              (capi:prompt-for-file
               "Select image to import"
               :operation :open
               :pathname (sys:get-folder-path :documents)
               :filter "*.png;*.jpg;*.jpeg;*.bmp"
               :filters '("Image File" "*.png;*.jpg;*.jpeg;*.bmp"))
            (if okp
              (let* ((img (gp:load-image board file))
                     (w (gp:image-width img))
                     (h (gp:image-height img))
                     (ratio (min (/ (* 7/8 @board.width) w)
                                 (/ (* 7/8 @board.height) h))))
                (setf image img
                      dx (1+ x-offset)
                      dy (1+ y-offset)
                      width (floor (* w (* ratio 2)))
                      height (floor (* h ratio))
                      old-tool (class-name-of current-tool))
                (set-tool-internal itf board tool)
                (refresh-import-image tool))
              (set-tool itf board (class-name-of current-tool)))))))))

(defmethod set-tool-internal :before (itf board (name import-image))
  (setf (capi:simple-pane-cursor board)
        #+darwin :open-hand
        #-darwin :move))

(defun refresh-import-image (tool)
  (with-slots (board image method bit charset dx dy width height rotate original-pixels) tool
    (when (and (plusp width) (plusp height))
      (let* ((theta (deg-to-rad rotate))
             (pixels (case method
                       (:1-by-1 (1-by-1-interpolation board image width height theta bit))
                       (:1-by-2 (1-by-2-interpolation board image width height theta bit))
                       (:gray-scale (gray-scale-interpolation board image width height theta charset))))
             (new-pixels (make-pixels)))
        (setf original-pixels pixels)
        (loop-pixels pixels
          (add-pixel (+ %x dx) (+ %y dy) %pixel new-pixels))
        (set-selected-pixels board new-pixels)
        (move-cursor board (pixels-left new-pixels) (pixels-top new-pixels))))))

(defmethod make-settings-layout (itf (tool import-image))
  (with-slots (settings-layout method bit charset width height rotate lock-ratio) tool
    (let (width-input height-input)
      (setq width-input (make 'capi:text-input-range
                              :title "W:" :title-position :left
                              :start 1 :end 99999 :value width
                              :visible-min-width '(character 5)
                              :callback-type :data
                              :callback (lambda (data)
                                          (setf width data)
                                          (when lock-ratio
                                            (setf height (round (/ data lock-ratio))
                                                  (capi:text-input-range-value height-input) height))
                                          (refresh-import-image tool)))
            height-input (make 'capi:text-input-range
                               :title "H:" :title-position :left
                               :start 1 :end 99999 :value height
                               :visible-min-width '(character 5)
                               :callback-type :data
                               :callback (lambda (data)
                                                       (setf height data)
                                                       (when lock-ratio
                                                         (setf width (round (* data lock-ratio))
                                                               (capi:text-input-pane-text width-input) width))
                                                       (refresh-import-image tool))))
      (make 'capi:column-layout
            :adjust :center
            :description
            (list (make 'capi:radio-button-panel
                        :title "Convert method:"
                        :title-position :left
                        :print-function #'string-capitalize
                        :layout-class 'capi:column-layout
                        :items '(:1-by-1 :1-by-2 :gray-scale)
                        :selected-item method
                        :selection-callback (lambda (data itf)
                                              (setf method data)
                                              (refresh-import-image tool)
                                              (setf settings-layout (make-settings-layout itf tool)
                                                    (capi:layout-description @itf.tool-settings-container) (list settings-layout))))
                  (make 'capi:toolbar
                        :button-width 32 :button-height 32
                        :flatp t
                        :items
                        (list width-input
                              (make 'capi:toolbar-button
                                    :text (string (if lock-ratio (code-char 128274) (code-char 128275)))
                                    :callback-type :element
                                    :callback (lambda (self)
                                                (setf lock-ratio (if lock-ratio nil
                                                                   (/ width height))
                                                      (capi:item-text self)
                                                      (string (if lock-ratio (code-char 128274) (code-char 128275))))))
                              height-input))
                  (make 'capi:text-input-range
                        :title "Rotate:" :title-position :left
                        :start 0 :end 359 :value rotate
                        :callback-type :data
                        :callback (lambda (data)
                                    (setf rotate data)
                                    (refresh-import-image tool)))
                  :separator
                  (case method
                    ((or :1-by-1 :1-by-2)
                     (make 'capi:radio-button-panel
                           :title "Bit:" :title-position :left
                           :items '(4 8 24)
                           :selected-item bit
                           :callback-type :data
                           :selection-callback (lambda (data)
                                                 (setf bit data)
                                                 (refresh-import-image tool))))
                    (:gray-scale
                     (make 'capi:text-input-pane
                           :title "Charset:" :title-position :left
                           :text charset
                           :font @tool.board.fdesc
                           :change-callback-type :data
                           :text-change-callback (lambda (data)
                                                   (setf charset data)
                                                   (refresh-import-image tool)))))
                  (make 'capi:output-pane :visible-max-height 0))))))

(defmethod tool-press ((tool import-image) x y)
  (with-slots (board start-x start-y start-dx dx start-dy dy) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (setf start-x x0 start-y y0
            start-dx dx start-dy dy)
      (setf (capi:simple-pane-cursor board)
            #+darwin :closed-hand
            #-darwin :move))))

(defmethod tool-arrow-key ((tool import-image) x y key)
  (with-slots (board start-x start-y original-pixels start-dx start-dy dx dy) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (editor-pane-point board))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (when (or (null start-x)
                  (or (/= dx (+ start-dx (- x0 start-x)))
                      (/= dy (+ start-dy (- y0 start-y)))))
          (setf start-x x0 start-y y0
                start-dx dx start-dy dy))
        (case (sys:gesture-spec-data key)
          ((or :up 112) (decf y0))
          ((or :down 110) (incf y0))
          ((or :left 98) (decf x0))
          ((or :right 102) (incf x0)))
        (let ((new (make-pixels)))
          (setf dx (+ start-dx (- x0 start-x))
                dy (+ start-dy (- y0 start-y)))
          (loop-pixels original-pixels
            (add-pixel (+ %x dx) (+ %y dy) %pixel new))
          (set-selected-pixels board new))
        (move-cursor board x0 y0)))))

(defmethod tool-shift-arrow ((tool import-image) x y key)
  (tool-arrow-key tool x y key))

(defmethod tool-drag ((tool import-image) x y)
  (with-slots (board start-x start-y original-pixels start-dx start-dy dx dy) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (if start-x
        (let ((new (make-pixels))
              (new-dx (+ start-dx (- x0 start-x)))
              (new-dy (+ start-dy (- y0 start-y))))
          (when (or (/= dx new-dx) (/= dy new-dy))
            (setf dx new-dx dy new-dy)
            (loop-pixels original-pixels
              (add-pixel (+ %x dx) (+ %y dy) %pixel new))
            (set-selected-pixels board new)
            (move-cursor board
                         (max (pixels-left new) @board.x-offset)
                         (max (pixels-top new) @board.y-offset))))
        (tool-press tool x y)))))

(defmethod tool-release ((tool import-image) x y)
  (with-slots (start-x start-y) tool
    (setf start-x nil start-y nil)
    (setf (capi:simple-pane-cursor @tool.board)
          #+darwin :open-hand
          #-darwin :move)))

(defmethod tool-cleanup ((tool import-image))
  (with-slots (board start-x start-y start-dx start-dy original-pixels dx dy image) tool
    (set-selected-pixels board nil)
    (setf start-x nil start-y nil start-dx nil start-dy nil
          original-pixels nil
          dx 0 dy 0
          image nil)
    (setf (capi:simple-pane-cursor board) nil)
    (set-message @tool.default-message (capi:element-interface board))))

(defmethod tool-return ((tool import-image) x y key)
  (with-slots (board old-tool) tool
    (with-slots (selected-pixels) board
      (when selected-pixels
        (let ((origin (make-pixels)))
          (loop-pixels selected-pixels
            (add-pixel %x %y (get-pixel %x %y board) origin))
          (ring-push origin @board.current-layer.undo-ring))
        (paint-pixels board selected-pixels)
        (tool-cleanup tool)
        (set-tool (capi:element-interface board) board old-tool)))))

(defmethod tool-escape ((tool import-image) x y key)
  (tool-cleanup tool))

(defmethod tool-backspace ((tool import-image) x y key)
  (tool-cleanup tool))


;; Color picker

(defclass picker (board-tool)
  ((picking :initform '(:foreground :background :character)))
  (:default-initargs
   :default-message (editor::make-buffer-string
                     :%string "Write one:  ⏎    Erase one:  ⌫    Pick value:  ⌃Space "
                     :properties '((0 11 (face editor::default))
                                   (11 14 (face hl-background))
                                   (14 28 (face editor::default))
                                   (28 31 (face hl-background))
                                   (31 46 (face editor::default))
                                   (46 54 (face hl-background))))))

(defmethod make-settings-layout (itf (tool picker))
  (make 'capi:column-layout
        :adjust :center
        :description
        (list (make 'capi:check-button-panel
                    :title "Picking:" :title-position :left
                    :items '(:foreground :background :character)
                    :layout-class 'capi:column-layout
                    :print-function #'string-capitalize
                    :selected-items @tool.picking
                    :callback-type :element
                    :selection-callback (op (setf @tool.picking (capi:choice-selected-items _))))
              (make 'capi:output-pane :visible-max-height 0))))

(defmethod tool-motion ((tool picker) x y)
  (move-cursor-pixelwise @tool.board x y))

(defmethod tool-press ((tool picker) x y)
  (let ((board @tool.board))
    (multiple-value-bind (x0 y0)
        (translate-position board x y)
      (let ((pixel (get-pixel x0 y0 board)))
        (when (member :foreground @tool.picking)
          (set-fg (when pixel (pixel-fg pixel)) board))
        (when (member :background @tool.picking)
          (set-bg (when pixel (pixel-bg pixel)) board))
        (when (member :character @tool.picking)
          (when pixel
            (setf @board.bold-p (pixel-bold-p pixel)
                  @board.italic-p (pixel-italic-p pixel)
                  @board.underline-p (pixel-underline-p pixel)))
          (set-char (if pixel (pixel-char pixel) #\Space) board))))))

(defmethod tool-return ((tool picker) x y key)
  (let* ((board @tool.board)
         (point (editor-pane-point board))
         (x (+ @board.x-offset (point-column point)))
         (y (+ @board.y-offset (point-linenum point)))
         (pixel (get-pixel x y board)))
    (when (member :foreground @tool.picking)
      (set-fg (when pixel (pixel-fg pixel)) board))
    (when (member :background @tool.picking)
      (set-bg (when pixel (pixel-bg pixel)) board))
    (when (member :character @tool.picking)
      (when pixel
        (setf @board.bold-p (pixel-bold-p pixel)
              @board.italic-p (pixel-italic-p pixel)
              @board.underline-p (pixel-underline-p pixel)))
      (set-char (if pixel (pixel-char pixel) #\Space) board))))

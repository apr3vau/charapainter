;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defclass board-tool ()
  ((board :initarg :board
          :accessor tool-board)
   (settings-layout :initform nil)))

(defgeneric set-tool (interface board name)
  (:method (interface board name)
   (with-slots (tools) interface
     (let ((tool (find name tools :key (op (class-name (class-of _))))))
       (set-tool-internal interface board tool)))))

(defgeneric set-tool-internal (interface board tool)
  (:method (itf board tool)
   (with-slots (current-tool) board
     (when current-tool (tool-cleanup current-tool))
     (setf current-tool tool))))

(defgeneric tool-cleanup     (tool)         (:method (tool)))
(defgeneric tool-description (tool)         (:method (tool)))

(defgeneric tool-press       (tool x y)     (:method (tool x y)))
(defgeneric tool-drag        (tool x y)     (:method (tool x y)))
(defgeneric tool-release     (tool x y)     (:method (tool x y)))
(defgeneric tool-motion      (tool x y)     (:method (tool x y)))
(defgeneric tool-shift-press (tool x y)     (:method (tool x y)))
(defgeneric tool-shift-drag  (tool x y)     (:method (tool x y)))
(defgeneric tool-ctrl-space  (tool x y key) (:method (tool x y key)))
(defgeneric tool-escape      (tool x y key) (:method (tool x y key)))

(defgeneric tool-return      (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (let* ((point (buffer-point (capi:editor-pane-buffer board)))
              (x (+ x-offset (point-column point)))
              (y (+ y-offset (point-linenum point))))
         (paint-pixel board x y (make-pixel :char *char* :fg *fg* :bg *bg*))
         (case (slot-value board 'cursor-movement)
           (:right (incf x))
           (:down (incf y)))
         (move-cursor board x y))))))

(defgeneric tool-backspace (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (let* ((point (buffer-point (capi:editor-pane-buffer board)))
              (x (+ x-offset (point-column point)))
              (y (+ y-offset (point-linenum point))))
         (case (slot-value board 'cursor-movement)
           (:right (decf x))
           (:down (decf y)))
         (paint-pixel board x y nil)
         (move-cursor board x y))))))

(defgeneric tool-arrow-key (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (buffer-point (capi:editor-pane-buffer board)))
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
       (setq x (clamp x 0 *board-width*)
             y (clamp y 0 *board-height*))
       (move-cursor-relative board x y)))))

(defgeneric tool-line-start (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (buffer-point (capi:editor-pane-buffer board)))
            (y (point-linenum point)))
       (move-cursor-relative board 0 y)))))

(defgeneric tool-line-end (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (let* ((point (buffer-point (capi:editor-pane-buffer board)))
            (y (point-linenum point)))
       (move-cursor-relative board *board-width* y)))))

(defgeneric tool-key (tool x y key)
  (:method ((tool board-tool) x y key)
   (with-slots (board) tool
     (with-slots (x-offset y-offset) board
       (awhen (system:gesture-spec-to-character key :errorp nil)
         (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                (x (+ x-offset (point-column point)))
                (y (+ y-offset (point-linenum point)))
                (original-pixels (make-pixels)))
           (add-pixel x y (get-pixel x y board) original-pixels)
           (ring-push original-pixels (board-undo-ring board))
           (paint-pixel board x y (make-pixel :char it :fg *fg* :bg *bg*))
           (case (slot-value board 'cursor-movement)
             (:right (incf x))
             (:down (incf y)))
           (move-cursor board x y)))))))

(defgeneric make-settings-layout (interface tool)
  (:method (itf tool) (make-instance 'capi:title-pane :text "Welcome to Charapainter! :D"))
  (:method :around (itf tool)
   (multiple-value-bind (desc keys) (tool-description tool)
     (make 'capi:column-layout
           :adjust :center
           :description
           (list (make-instance 'capi:multi-column-list-panel
                                :title desc :title-position :top :title-adjust :center
                                :columns '((:title "Key") (:title "Function"))
                                :items keys
                                :visible-min-width '(character 30))
                 (call-next-method))))))

;; Pan

(defclass pan (board-tool)
  ((drag-start-x-offset :initform nil)
   (drag-start-y-offset :initform nil)
   (drag-start-x        :initform nil)
   (drag-start-y        :initform nil)))

(defmethod set-tool :after (itf board (name (eql 'pan)))
  (setf (capi:simple-pane-cursor board) :open-hand))

(defmethod tool-description ((tool pan))
  (values "Pan: move the drawing board"
          '(("🖱️" "Move the drawing board")
            ("⌘↕↔" "Move the drawing board"))))

(defmethod tool-press ((tool pan) x y)
  (with-slots (board drag-start-x drag-start-y drag-start-x-offset drag-start-y-offset) tool
    (with-slots (x-offset y-offset) board
      (setf drag-start-x-offset x-offset
            drag-start-y-offset y-offset
            drag-start-x x
            drag-start-y y
            (capi:simple-pane-cursor board) :closed-hand))))

(defmethod tool-drag ((tool pan) x y)
  (with-slots (board drag-start-x drag-start-y drag-start-x-offset drag-start-y-offset) tool
    (with-slots (x-offset y-offset) board
      (let ((new-x-offset (+ drag-start-x-offset (floor (- drag-start-x x) *char-width*)))
            (new-y-offset (+ drag-start-y-offset (floor (- drag-start-y y) *char-height*))))
        (unless (and (= new-x-offset x-offset)
                     (= new-y-offset y-offset))
          (setf x-offset new-x-offset
                y-offset new-y-offset)
          (refresh-board board)))
      )))

(defmethod tool-release ((tool pan) x y)
  (setf (capi:simple-pane-cursor @tool.board) :open-hand))

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
   (track           :initform      nil))
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
             (pixel (make-pixel :char *char* :fg *fg* :bg *bg*)))
         (if track
           (multiple-value-bind (x1 y1) (apply #'translate-position board (car (last track)))
             (nunion-pixels pixels (line-pixels pixel x1 y1 x0 y0)))
           (add-pixel x0 y0 pixel pixels))
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
           (add-pixel %x %y (get-pixel %x %y board) original-pixels))))
     )))

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
    (ring-push original-pixels (board-undo-ring board))
    (end-stroke tool)))

(defmethod tool-motion ((tool paint-tool) x y)
  (when @tool.pixels (end-stroke tool))
  (move-cursor-pixelwise @tool.board x y))

(defmethod tool-cleanup ((tool paint-tool))
  (with-slots (board original-pixels track) tool
    (when track
      (ring-push original-pixels (board-undo-ring board)))
    (end-stroke tool)))

(defmethod tool-ctrl-space ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (buffer-point (capi:editor-pane-buffer board)))
           (x (* (point-column point) *char-width*))
           (y (* (point-linenum point) *char-height*)))
      (if track
        (tool-release tool x y)
        (tool-press tool x y)))))

(defmethod tool-arrow-key ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (buffer-point (capi:editor-pane-buffer board)))
           (x (* (point-column point) *char-width*))
           (y (* (point-linenum point) *char-height*)))
      (case (sys:gesture-spec-data key)
        ((or :up 112)
         (decf y *char-height*))
        ((or :down 110)
         (incf y *char-height*))
        ((or :left 98)
         (decf x *char-width*))
        ((or :right 102)
         (incf x *char-width*)))
      (setq x (clamp x 0 (* *board-width* *char-width*))
            y (clamp y 0 (* *board-height* *char-height*)))
      (if track
        (tool-drag tool x y)
        (tool-motion tool x y)))))

(defmethod tool-line-start ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (buffer-point (capi:editor-pane-buffer board)))
           (y (* (point-linenum point) *char-height*)))
      (if track
        (tool-drag tool 0 y)
        (tool-motion tool 0 y)))))

(defmethod tool-line-end ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (let* ((point (buffer-point (capi:editor-pane-buffer board)))
           (x (* *board-width* *char-width*))
           (y (* (point-linenum point) *char-height*)))
      (if track
        (tool-drag tool x y)
        (tool-motion tool x y)))))

(defmethod tool-escape ((tool paint-tool) x y key)
  (with-slots (board track) tool
    (when track
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x (* (point-column point) *char-width*))
             (y (* (point-linenum point) *char-height*)))
        (tool-release tool x y)))))

;; Brush & Eraser

(defclass brush (paint-tool) ())
(defclass eraser (paint-tool) ())

(defmethod generate-pixels ((tool eraser) x y)
  (with-slots (board track) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (let ((pixels (make-pixels))
            (pixel (make-pixel :char #\Space)))
        (if track
          (multiple-value-bind (x1 y1) (apply #'translate-position board (car (last track)))
            (nunion-pixels pixels (line-pixels pixel x1 y1 x0 y0)))
          (add-pixel x0 y0 pixel pixels))
        pixels))))

(defmethod tool-description ((tool brush))
  (values "Brush: Paint the character"
          '(("🖱️" "Draw a stroke")
            ("⏎" "Draw current character")
            ("⌫" "Erase previous character")
            ("^Space" "Start or stop a stroke")
            ("↕↔" "Move or drag a stroke"))))

(defmethod tool-description ((tool eraser))
  (values "Eraser: Erase the character"
          '(("🖱️" "Erase characters")
            ("⏎" "Draw current character")
            ("⌫" "Erase previous character")
            ("^Space" "Start or stop a stroke")
            ("↕↔" "Move or erase a stroke"))))

(defclass stroke (paint-tool)
  ((charset :initform :ascii)))

(defmethod tool-description ((tool stroke))
  (values "Stroke: Draw a continuous line"
          '(("🖱️" "Draw a continuous line")
            ("^Space" "Start or stop a stroke")
            ("↕↔" "Move or drag a stroke")
            ("⇧↕↔" "Change character direction")
            ("^⇧↕↔" "Draw arrow character")
            ("⏎" "Draw current character")
            ("⌫" "Erase previous character"))))

(defmethod make-settings-layout (itf (tool stroke))
  (make-instance 'capi:option-pane
                 :title "Charset:"
                 :title-position :left
                 :items (serapeum:plist-keys *charsets*)
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf (slot-value tool 'charset) _))))

(defmethod update-pixels ((tool stroke) new-x new-y)
  (with-slots (board pixels old-pixels new-pixels updated-pixels original-pixels track charset) tool
    (setf old-pixels     (copy-pixels pixels)
          new-pixels     (or (generate-pixels tool new-x new-y) (make-pixels))
          pixels         (join-pixels charset (nunion-pixels pixels new-pixels))
          updated-pixels (make-pixels))
    (push-end (list new-x new-y) track)
    (loop-pixels old-pixels
      (unless (find-pixel %x %y pixels)
        (add-pixel %x %y (find-pixel %x %y original-pixels) updated-pixels)))
    (loop-pixels pixels
      (unless (equalp (find-pixel %x %y old-pixels) %pixel)
        (add-pixel %x %y %pixel updated-pixels)
        (unless (second (multiple-value-list (nfind-pixel %x %y original-pixels)))
          (add-pixel %x %y (get-pixel %x %y board) original-pixels))))))

(defmethod tool-key :around ((tool stroke) x y key)
  (with-slots (board charset) tool
    (with-slots (x-offset y-offset) board
      (cond ((and (= (sys:gesture-spec-modifiers key) sys:gesture-spec-shift-bit)
                  (member (sys:gesture-spec-data key) '(:up :down :left :right)))
             (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                    (x (+ x-offset (point-column point)))
                    (y (+ y-offset (point-linenum point)))
                    (adjacent-pixels (make-pixels))
                    (char (charset-get charset :cross)))
               (add-pixel (1- x) y (get-pixel (1- x) y board) adjacent-pixels)
               (add-pixel (1+ x) y (get-pixel (1+ x) y board) adjacent-pixels)
               (add-pixel x (1- y) (get-pixel x (1- y) board) adjacent-pixels)
               (add-pixel x (1+ y) (get-pixel x (1+ y) board) adjacent-pixels)
               (add-pixel x y (make-pixel :fg *fg* :bg *bg*) adjacent-pixels)
               (case (sys:gesture-spec-data key)
                 (:up (add-pixel x (1- y) (make-pixel :char char) adjacent-pixels))
                 (:down (add-pixel x (1+ y) (make-pixel :char char) adjacent-pixels))
                 (:left (add-pixel (1- x) y (make-pixel :char char) adjacent-pixels))
                 (:right (add-pixel (1+ x) y (make-pixel :char char) adjacent-pixels)))
               (let ((original-pixels (make-pixels)))
                 (add-pixel x y (get-pixel x y board) original-pixels)
                 (ring-push original-pixels (board-undo-ring board)))
               (paint-pixel board x y (find-pixel x y (join-pixels charset adjacent-pixels)))
               (move-cursor board x y)))
            ((and (= (sys:gesture-spec-modifiers key) (+ sys:gesture-spec-shift-bit
                                                         sys:gesture-spec-control-bit))
                  (member (sys:gesture-spec-data key) '(:up :down :left :right)))
             (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                    (x (+ x-offset (point-column point)))
                    (y (+ y-offset (point-linenum point))))
               (let ((original-pixels (make-pixels)))
                 (add-pixel x y (get-pixel x y board) original-pixels)
                 (ring-push original-pixels (board-undo-ring board)))
               (paint-pixel board x y
                            (make-pixel :char (charset-get charset (case (sys:gesture-spec-data key)
                                                                     (:up :arr-t)
                                                                     (:down :arr-b)
                                                                     (:left :arr-l)
                                                                     (:right :arr-r)))
                                        :fg *fg* :bg *bg*))
               (move-cursor board x y)))
            (t (call-next-method))))))

(defclass rectangle (paint-tool)
  ((border :initform :ascii)
   (filling :initform :none)))

(defmethod tool-description ((tool rectangle))
  (values "Stroke: Draw a continuous line"
          '(("🖱️" "Draw a rectangle")
            ("^Space" "Set the vertex of the rectangle")
            ("↕↔" "Move or drag a rectangle")
            ("⇧↕↔" "Change character direction")
            ("^⇧↕↔" "Draw arrow character")
            ("⏎" "Draw current character")
            ("⌫" "Erase previous character"))))

(defmethod make-settings-layout (itf (tool rectangle))
  (with-slots (border filling) tool
    (make-instance
     'capi:column-layout
     :description
     (list (make-instance 'capi:option-pane
                          :title "Border:"
                          :title-position :left
                          :items (nconc (serapeum:plist-keys *charsets*)
                                        '(:none :clear :color :character :color-and-character))
                          :print-function #'string-capitalize
                          :callback-type :data
                          :selection-callback (op (setf border _)))
           (make-instance 'capi:option-pane
                          :title "Filling:"
                          :title-position :left
                          :items '(:none :clear :color :character :color-and-character)
                          :print-function #'string-capitalize
                          :selected-item filling
                          :callback-type :data
                          :selection-callback (op (setf filling _)))))))

(defmethod generate-pixels ((tool rectangle) x y)
  (with-slots (board pixels track border filling) tool
    (multiple-value-bind (x1 y1) (translate-position board x y)
      (if (car track)
        (multiple-value-bind (x0 y0) (apply #'translate-position board (car track))
          (setf pixels (rectangle-pixels x0 y0 x1 y1 border filling)))
        (let ((new-pixels (make-pixels)))
          (add-pixel x1 y1 (make-pixel :char *char* :fg *fg* :bg *bg*)
                     new-pixels)
          new-pixels)))))

(defmethod tool-key :around ((tool rectangle) x y key)
  (with-slots (board border) tool
    (with-slots (x-offset y-offset) board
      (cond ((and (= (sys:gesture-spec-modifiers key) sys:gesture-spec-shift-bit)
                  (member (sys:gesture-spec-data key) '(:up :down :left :right)))
             (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                    (x (+ x-offset (point-column point)))
                    (y (+ y-offset (point-linenum point)))
                    (adjacent-pixels (make-pixels))
                    (char (charset-get border :cross)))
               (let ((original-pixels (make-pixels)))
                 (add-pixel x y (get-pixel x y board) original-pixels)
                 (ring-push original-pixels (board-undo-ring board)))
               (if (member border '(:none :clear :color :character :color-and-character))
                 (paint-pixel board x y
                              (case border
                                (:clear (make-pixel :char #\Space))
                                (:color (make-pixel :char #\Space :bg *fg*))
                                (:character (make-pixel :char *char*))
                                (:color-and-character (make-pixel :char *char* :fg *fg* :bg *bg*))))
                 (progn
                   (add-pixel (1- x) y (get-pixel (1- x) y board) adjacent-pixels)
                   (add-pixel (1+ x) y (get-pixel (1+ x) y board) adjacent-pixels)
                   (add-pixel x (1- y) (get-pixel x (1- y) board) adjacent-pixels)
                   (add-pixel x (1+ y) (get-pixel x (1+ y) board) adjacent-pixels)
                   (add-pixel x y (make-pixel :fg *fg* :bg *bg*) adjacent-pixels)
                   (case (sys:gesture-spec-data key)
                     (:up (add-pixel x (1- y) (make-pixel :char char) adjacent-pixels))
                     (:down (add-pixel x (1+ y) (make-pixel :char char) adjacent-pixels))
                     (:left (add-pixel (1- x) y (make-pixel :char char) adjacent-pixels))
                     (:right (add-pixel (1+ x) y (make-pixel :char char) adjacent-pixels)))
                   (paint-pixel board x y (find-pixel x y (join-pixels border adjacent-pixels)))))
               (move-cursor board x y)))
            ((and (= (sys:gesture-spec-modifiers key) (+ sys:gesture-spec-shift-bit
                                                         sys:gesture-spec-control-bit))
                  (member (sys:gesture-spec-data key) '(:up :down :left :right)))
             (let* ((point (buffer-point (capi:editor-pane-buffer board)))
                    (x (+ x-offset (point-column point)))
                    (y (+ y-offset (point-linenum point))))
               (let ((original-pixels (make-pixels)))
                 (add-pixel x y (get-pixel x y board) original-pixels)
                 (ring-push original-pixels (board-undo-ring board)))
               (if (member border '(:none :clear :color :character :color-and-character))
                 (paint-pixel board x y
                              (case border
                                (:clear (make-pixel :char #\Space))
                                (:color (make-pixel :char #\Space :bg *fg*))
                                (:character (make-pixel :char *char*))
                                (:color-and-character (make-pixel :char *char* :fg *fg* :bg *bg*))))
                 (paint-pixel board x y
                              (make-pixel :char (charset-get border (case (sys:gesture-spec-data key)
                                                                      (:up :arr-t)
                                                                      (:down :arr-b)
                                                                      (:left :arr-l)
                                                                      (:right :arr-r)))
                                          :fg *fg* :bg *bg*)))
               (move-cursor board x y)))
            (t (call-next-method))))))

(defclass coloring (paint-tool)
  ((coloring-to :initform :background)))

(defmethod tool-description ((tool coloring))
  (values "Coloring: Set the color of characters"
          '(("🖱️" "Set characters color")
            ("⏎" "Set character color")
            ("⌫" "Erase character color")
            ("^Space" "Start or stop a stroke")
            ("↕↔" "Move or drag a stroke"))))

(defmethod make-settings-layout (itf (tool coloring))
  (make-instance 'capi:option-pane
                 :title "Coloring to:"
                 :title-position :left
                 :items '(:background :foreground)
                 :print-function #'string-capitalize
                 :callback-type :data
                 :selection-callback (op (setf (slot-value tool 'coloring-to) _))))

(defmethod generate-pixels ((tool coloring) x y)
  (with-slots (board coloring-to) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (let ((pixels (make-pixels))
            (pixel (make-pixel :char nil :fg *fg* :bg *bg*)))
        (let ((origin (get-pixel x0 y0 board)))
          (setf (pixel-char pixel) (if origin (pixel-char origin) #\Space))
          (if (eq coloring-to :background)
            (setf (pixel-fg pixel) (when origin (pixel-fg origin)))
            (setf (pixel-bg pixel) (when origin (pixel-bg origin))))
          (add-pixel x0 y0 pixel pixels))
        pixels))))

(defmethod tool-return ((tool coloring) x y key)
  (with-slots (board coloring-to) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x (+ x-offset (point-column point)))
             (y (+ y-offset (point-linenum point)))
             (pixel (or (get-pixel x y board) (make-pixel :char #\Space))))
        (if (eq coloring-to :background)
          (setf (pixel-bg pixel) *bg*)
          (setf (pixel-fg pixel) *fg*))
        (paint-pixel board x y pixel)
        (case (slot-value board 'cursor-movement)
          (:right (incf x))
          (:down (incf y)))
        (move-cursor board x y)))))

(defmethod tool-backspace ((tool coloring) x y key)
  (with-slots (board coloring-to) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x (+ x-offset (point-column point)))
             (y (+ y-offset (point-linenum point))))        
        (case (slot-value board 'cursor-movement)
          (:right (decf x))
          (:down (decf y)))
        (when-let (pixel (get-pixel x y board))
          (if (eq coloring-to :background)
            (setf (pixel-bg pixel) nil)
            (setf (pixel-bg pixel) nil))
          (paint-pixel board x y nil)
          (move-cursor board x y))))))

;; Select

(defclass select (board-tool)
  ((left :initform nil)
   (top :initform nil)
   (right :initform nil)
   (bottom :initform nil)
   (pixels :initform nil)
   (origin :initform nil)
   (start-x :initform nil)
   (start-y :initform nil)
   (start-dx :initform nil)
   (start-dy :initform nil)
   (dx :initform 0)
   (dy :initform 0)))

(defmethod tool-description ((tool select))
  (values "Select: select and modify a region."
          '(("🖱️" "Select or move a region")
            ("⏎" "Confirm modification")
            ("⎋" "Discard modification")
            ("⌫" "Delete selection")
            ("^Space" "Start or stop a selection")
            ("↕↔" "Resize or move a selection"))))

(defmethod tool-press ((tool select) x y)
  (with-slots (board left top start-x start-y start-dx start-dy pixels dx dy) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (if pixels
        (setf start-x x0 start-y y0
              start-dx dx start-dy dy)
        (setf left x0 top y0 dx 0 dy 0)))
    (set-selected-pixels board (make-pixels))))

(defmethod tool-ctrl-space ((tool select) x y key)
  (with-slots (board left top pixels) tool
    (with-slots (x-offset y-offset selected-pixels) board
      (when pixels (tool-return tool x y nil))
      (if left
        (progn
          (tool-release tool x y)
          (when selected-pixels
            (move-cursor board (pixels-left selected-pixels) (pixels-top selected-pixels))))
        (let* ((point (buffer-point (capi:editor-pane-buffer board)))
               (x0 (+ x-offset (point-column point)))
               (y0 (+ y-offset (point-linenum point))))
          (setf left x0 top y0)
          (set-selected-pixels board (make-pixels)))))))

(defmethod tool-arrow-key ((tool select) x y key)
  (with-slots (board left top right bottom start-x start-y start-dx start-dy dx dy pixels) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (when (and pixels
                   (or (null start-x)
                       (or (/= dx (+ start-dx (- x0 start-x)))
                           (/= dy (+ start-dy (- y0 start-y))))))
          (setf start-x x0 start-y y0
                start-dx dx start-dy dy))
        (case (sys:gesture-spec-data key)
          ((or :up 112)
           (decf y0))
          ((or :down 110)
           (incf y0))
          ((or :left 98)
           (decf x0))
          ((or :right 102)
           (incf x0)))
        (if pixels
          (let ((new (make-pixels)))
            (setf dx (+ start-dx (- x0 start-x))
                  dy (+ start-dy (- y0 start-y)))
            (loop-pixels pixels
              (add-pixel (+ %x dx) (+ %y dy) %pixel new))
            (set-selected-pixels board new))
          (if left
            (let ((new (copy-pixels @board.selected-pixels)))
              (setf right x0 bottom y0)
              (dorange$fixnum (x left right)
                (dorange$fixnum (y top bottom)
                  (awhen (get-pixel x y board)
                    (add-pixel x y it new)
                    (delete-pixel x y (project-pixels (board-project board))))))
              (set-selected-pixels board new))
            (tool-press tool x y)))
        (move-cursor board x0 y0)))))

(defmethod tool-drag ((tool select) x y)
  (with-slots (board left top right bottom start-x start-y start-dx start-dy pixels dx dy) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (if pixels
        (if start-x
          (let ((new-dx (+ start-dx (- x0 start-x)))
                (new-dy (+ start-dy (- y0 start-y)))
                (new (make-pixels)))
            (when (or (/= dx new-dx) (/= dy new-dy))
              (setf dx new-dx dy new-dy)
              (loop-pixels pixels
                (add-pixel (+ %x dx) (+ %y dy) %pixel new))
              (set-selected-pixels board new)
              (move-cursor board
                           (max (pixels-left new) @board.x-offset)
                           (max (pixels-top new) @board.y-offset))))
          (tool-press tool x y))
        (if left
          (let ((new (copy-pixels @board.selected-pixels)))
            (setf right x0 bottom y0)
            (dorange$fixnum (y top bottom)
              (dorange$fixnum (x left right)
                (awhen (get-pixel x y board)
                  (add-pixel x y it new)
                  (delete-pixel x y (project-pixels (board-project board))))))
            (set-selected-pixels board new)
            (if (pixels-not-empty-p new)
              (move-cursor board (pixels-left new) (pixels-top new))
              (move-cursor-pixelwise board x y)))
          (tool-press tool x y))))))

(defmethod tool-release ((tool select) x y)
  (with-slots (board left top right bottom start-x start-y pixels origin) tool
    (with-slots (selected-pixels) board
      (setf left nil top nil right nil bottom nil
            start-x nil start-y nil)
      (unless pixels
        (if (and selected-pixels (pixels-not-empty-p selected-pixels))
          (progn
            (setf pixels (copy-pixels selected-pixels)
                  origin (copy-pixels selected-pixels))
            (move-cursor board (pixels-left selected-pixels) (pixels-top selected-pixels)))
          (setf selected-pixels nil))))))

(defmethod tool-motion ((tool select) x y)
  (with-slots (board pixels) tool
    (unless pixels
      (move-cursor-pixelwise board x y))))

(defmethod tool-cleanup ((tool select))
  (with-slots (board left top right bottom start-x start-y pixels start-dx start-dy dx dy) tool
    (set-selected-pixels board nil)
    (setf left nil top nil right nil bottom nil
          start-x nil start-y nil start-dx nil start-dy nil
          pixels nil
          dx 0 dy 0)))

(defmethod tool-return ((tool select) x y key)
  (with-slots (board pixels) tool
    (with-slots (selected-pixels) board
      (when selected-pixels
        (let ((origin (make-pixels)))
          (loop-pixels selected-pixels
            (add-pixel %x %y (get-pixel %x %y board) origin))
          (ring-push (union-pixels pixels origin) (board-undo-ring board)))
        (paint-pixels board selected-pixels)
        (tool-cleanup tool)))))

(defmethod tool-escape ((tool select) x y key)
  (when @tool.origin
    (paint-pixels @tool.board @tool.origin)
    (tool-cleanup tool)))

(defmethod tool-backspace :around ((tool select) x y key)
  (if @tool.pixels (tool-cleanup tool) (call-next-method)))

;; Import

(defclass import-image (board-tool)
  ((old-tool :initform nil)
   (image :initform nil)
   (method :initform :1-by-1)
   (bit :initform 24)
   (charset :initform " .:-=+*%#@")
   (width :initform 40)
   (height :initform 28)
   (rotate :initform 0)
   (start-x :initform nil)
   (start-y :initform nil)
   (dx :initform 0)
   (dy :initform 0)
   (start-dx :initform nil)
   (start-dy :initform nil)
   (original-pixels :initform nil)))

(defmethod tool-description ((tool import-image))
  (values "Import image"
          '(("🖱️" "Move the area")
            ("⏎" "Write down the area")
            ("⎋" "Discard the area")
            ("⌫" "Discard the area")
            ("↕↔" "Move the area"))))

(defmethod set-tool (itf board (name (eql 'import-image)))
  (with-slots (tools board) itf
    (with-slots (x-offset y-offset current-tool) board
      (let ((tool (find name tools :key (op (class-name (class-of _))))))
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
                     (ratio (min (/ (* 7/8 *board-width*) w)
                                 (/ (* 7/8 *board-height*) h))))
                (setf image img
                      dx (1+ x-offset)
                      dy (1+ y-offset)
                      width (floor (* w (* ratio 2)))
                      height (floor (* h ratio))
                      old-tool (class-name (class-of current-tool)))
                (set-tool-internal itf board tool)
                (refresh-import-image tool))
              (set-tool itf board (class-name (class-of current-tool))))))))))

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
  (with-slots (settings-layout method bit charset width height rotate) tool
    (make-instance
     'capi:column-layout
     :description
     (list (make-instance
            'capi:option-pane
            :title "Convert method:"
            :title-position :left
            :print-function #'string-capitalize
            :items '(:1-by-1 :1-by-2 :gray-scale)
            :selected-item method
            :selection-callback (lambda (data itf)
                                  (setf method data)
                                  (refresh-import-image tool)
                                  (with-slots (tool-settings-container) itf
                                    (setf settings-layout (make-settings-layout itf tool))
                                    (setf (capi:layout-description tool-settings-container) (list settings-layout)))))
           (make-instance
            'capi:row-layout
            :description
            (list (make-instance 'capi:text-input-pane
                                 :title "Width:" :title-position :left
                                 :text (princ-to-string width)
                                 :visible-min-width '(character 5)
                                 :change-callback-type '(:data :element)
                                 :text-change-callback (lambda (data self)
                                                         (let* ((str (ppcre:regex-replace-all "[^0-9]" data ""))
                                                                (num (if (zerop (length str)) 0 (parse-integer str))))
                                                           (setf (capi:text-input-pane-text self) (princ-to-string num)
                                                                 width num)
                                                           (refresh-import-image tool))))
                  (make-instance 'capi:text-input-pane
                                 :title "Height:" :title-position :left
                                 :text (princ-to-string height)
                                 :visible-min-width '(character 5)
                                 :change-callback-type '(:data :element)
                                 :text-change-callback (lambda (data self)
                                                         (let* ((str (ppcre:regex-replace-all "[^0-9]" data ""))
                                                                (num (if (zerop (length str)) 0 (parse-integer str))))
                                                           (setf (capi:text-input-pane-text self) str
                                                                 height num)
                                                           (refresh-import-image tool))))
                  ))
           (make-instance 'capi:text-input-pane
                          :title "Rotate:" :title-position :left
                          :text (princ-to-string rotate)
                          :change-callback-type '(:data :element)
                          :text-change-callback (lambda (data self)
                                                  (let* ((str (ppcre:regex-replace-all "[^0-9]" data ""))
                                                         (num (if (zerop (length str)) 0 (parse-integer str))))
                                                    (setf (capi:text-input-pane-text self) str
                                                          rotate num)
                                                    (refresh-import-image tool))))
           (case method
             ((or :1-by-1 :1-by-2)
              (make-instance
               'capi:option-pane
               :title "Bit" :title-position :left
               :items '(4 8 24)
               :selected-item bit
               :callback-type :data
               :selection-callback (lambda (data)
                                     (setf bit data)
                                     (refresh-import-image tool))))
             (:gray-scale
              (make-instance
               'capi:text-input-pane
               :title "Charset" :title-position :left
               :text charset
               :font *fdesc*
               :change-callback-type :data
               :text-change-callback (lambda (data)
                                       (setf charset data)
                                       (refresh-import-image tool)))))))))

(defmethod tool-press ((tool import-image) x y)
  (with-slots (board start-x start-y start-dx dx start-dy dy) tool
    (multiple-value-bind (x0 y0) (translate-position board x y)
      (setf start-x x0 start-y y0
            start-dx dx start-dy dy))))

(defmethod tool-arrow-key ((tool import-image) x y key)
  (with-slots (board start-x start-y original-pixels start-dx start-dy dx dy) tool
    (with-slots (x-offset y-offset) board
      (let* ((point (buffer-point (capi:editor-pane-buffer board)))
             (x0 (+ x-offset (point-column point)))
             (y0 (+ y-offset (point-linenum point))))
        (when (or (null start-x)
                  (or (/= dx (+ start-dx (- x0 start-x)))
                      (/= dy (+ start-dy (- y0 start-y)))))
          (setf start-x x0 start-y y0
                start-dx dx start-dy dy))
        (case (sys:gesture-spec-data key)
          ((or :up 112)
           (decf y0))
          ((or :down 110)
           (incf y0))
          ((or :left 98)
           (decf x0))
          ((or :right 102)
           (incf x0)))
        (let ((new (make-pixels)))
          (setf dx (+ start-dx (- x0 start-x))
                dy (+ start-dy (- y0 start-y)))
          (loop-pixels original-pixels
            (add-pixel (+ %x dx) (+ %y dy) %pixel new))
          (set-selected-pixels board new))
        (move-cursor board x0 y0)))))

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
    (setf start-x nil start-y nil)))

(defmethod tool-cleanup ((tool import-image))
  (with-slots (board start-x start-y start-dx start-dy original-pixels dx dy image) tool
    (set-selected-pixels board nil)
    (setf start-x nil start-y nil start-dx nil start-dy nil
          original-pixels nil
          dx 0 dy 0
          image nil)))

(defmethod tool-return ((tool import-image) x y key)
  (with-slots (board old-tool) tool
    (with-slots (selected-pixels) board
      (when selected-pixels
        (let ((origin (make-pixels)))
          (loop-pixels selected-pixels
            (add-pixel %x %y (get-pixel %x %y board) origin))
          (ring-push origin (board-undo-ring board)))
        (paint-pixels board selected-pixels)
        (tool-cleanup tool)
        (set-tool (capi:element-interface board) board old-tool)))))

(defmethod tool-escape ((tool import-image) x y key)
  (tool-cleanup tool))

(defmethod tool-backspace ((tool import-image) x y key)
  (tool-cleanup tool))

;; Color picker

(defclass picker (board-tool)
  ((picking :initform :foreground)))

(defmethod tool-description ((tool picker))
  (values "Pick selected attribute of item"
          '(("🖱️" "Pick the pointing item")
            ("^Space" "Pick the pointing item"))))

(defmethod make-settings-layout (itf (tool picker))
  (make 'capi:option-pane
        :title "Picking:" :title-position :left
        :items '(:foreground :background :character)
        :print-function #'string-capitalize
        :selected-item @tool.picking
        :callback-type :data
        :selection-callback (op (setf @tool.picking _))))

(defmethod tool-press ((tool picker) x y)
  (multiple-value-bind (x0 y0)
      (translate-position @tool.board x y)
    (let ((pixel (get-pixel x0 y0 @tool.board)))
      (case @tool.picking
        (:foreground (set-fg (when pixel (pixel-fg pixel))))
        (:background (set-bg (when pixel (pixel-bg pixel))))
        (:character (when pixel (set-char (pixel-char pixel))))))))

(defmethod tool-ctrl-space ((tool picker) x y key)
  (let* ((point (buffer-point (capi:editor-pane-buffer @tool.board)))
         (x (+ @tool.board.x-offset (point-column point)))
         (y (+ @tool.board.y-offset (point-linenum point)))
         (pixel (get-pixel x y @tool.board)))
    (case @tool.picking
      (:foreground (set-fg (when pixel (pixel-fg pixel))))
      (:background (set-bg (when pixel (pixel-bg pixel))))
      (:character (when pixel (set-char (pixel-char pixel)))))))
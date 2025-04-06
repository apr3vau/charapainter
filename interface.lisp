;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

;; Color picker

(defun 4-bit-board-display-callback (pane x y w h)
  (declare (ignore x y w h))
  (capi:with-geometry pane
    (let* ((gw (/ capi:%width% 8))
           black-lines white-lines)
      (dotimes (y 2)
        (dotimes (x 8)
          (let ((sx (* x gw)) (sy (* y gw)))
            (nconcf black-lines
                    (list sx sy (+ sx gw) sy)
                    (list sx sy sx (+ sy gw)))
            (nconcf white-lines
                    (list (+ sx gw) sy (+ sx gw) (+ sy gw))
                    (list sx (+ sy gw) (+ sx gw) (+ sy gw)))
            (gp:draw-rectangle pane sx sy gw gw 
                               :foreground (term-color-spec
                                            (aref *4-bit-colors* (+ (* y 8) x)))
                               :filled t))))
      (gp:draw-lines pane (mapcar #'1- white-lines) :foreground :white)
      (gp:draw-lines pane black-lines :foreground :black))))

(defun 8-bit-grayscale-display-callback (pane x y w h)
  (declare (ignore x y w h))
  (capi:with-geometry pane
    (capi:set-hint-table pane (list :visible-min-height (/ capi:%width% 24)
                                    :visible-max-height t)))
  (capi:with-geometry pane
    (let* ((gw (/ capi:%width% 24))
           black-lines white-lines)
      (dotimes (x 24)
        (let ((sx (* x gw)) (sy 0))
          (nconcf black-lines
                  (list sx sy (+ sx gw) sy)
                  (list sx sy sx (+ sy gw)))
          (nconcf white-lines
                  (list (+ sx gw) sy (+ sx gw) (+ sy gw))
                  (list sx (+ sy gw) (+ sx gw) (+ sy gw)))
          (gp:draw-rectangle pane sx sy gw gw 
                             :foreground (term-color-spec
                                          (aref *8-bit-colors* (+ 232 x)))
                             :filled t)))
      (gp:draw-lines pane (mapcar #'1- white-lines) :foreground :white)
      (gp:draw-lines pane black-lines :foreground :black))))

(defun spectrum-callback (pane x y w h)
  (declare (ignore x y w h))
  (capi:with-geometry pane
    (dorange$fixnum (y 0 capi:%height%)
      (dorange$fixnum (x 0 capi:%width%)
        (let* ((w/2 (/ capi:%width% 2))
               (hue (* (/ y capi:%height%) gp:2pi))
               (saturation (if (< x w/2) (/ x w/2) 1))
               (value (if (>= x w/2) (- 1 (- (/ x w/2) 1)) 1)))
          (gp:draw-point pane x y :foreground (color:make-hsv hue saturation value)))))))

(defun 8-bit-spectrum-callback (pane x y w h)
  (declare (ignore x y w h))
  (capi:with-geometry pane
    (dorange$fixnum (y 0 capi:%height%)
      (dorange$fixnum (x 0 capi:%width%)
        (let* ((w/2 (/ capi:%width% 2))
               (hue (* (/ y capi:%height%) gp:2pi))
               (saturation (if (< x w/2) (/ x w/2) 1))
               (value (if (>= x w/2) (- 1 (- (/ x w/2) 1)) 1)))
          (gp:draw-point pane x y
                         :foreground (term-color-spec
                                      (coerce-color-to 8 (color:make-hsv hue saturation value)))))))))

(defun spectrum-translate-position (pane x y)
  (capi:with-geometry pane
    (let* ((w/2 (/ capi:%width% 2))
           (hue (* (/ y capi:%height%) gp:2pi))
           (saturation (if (< x w/2) (/ x w/2) 1))
           (value (if (>= x w/2) (- 1 (- (/ x w/2) 1)) 1)))
      (color:make-hsv hue saturation value))))

(defun 4-bit-board-translate-position (pane x y)
  (capi:with-geometry pane
    (let ((gw (/ capi:%width% 8)))
      (aref *4-bit-colors*
            (+ (* (floor y gw) 8) (floor x gw))))))

(defun 8-bit-grayscale-translate-position (pane x y)
  (declare (ignore y))
  (capi:with-geometry pane
    (let ((gw (/ capi:%width% 24)))
      (aref *8-bit-colors* (+ (floor x gw) 232)))))


;; Refresh functions for setting variable

(defun refresh-font (pane &optional preserve-location)
  (let* ((itf (ensure-element-interface pane))
         (board @itf.board)
         (fdesc (gp:make-font-description :family @board.project.font-family
                                          :size @board.project.font-size))
         (font (gp:find-best-font board fdesc))
         (char-w (gp:get-font-width board font))
         (char-h (gp:get-font-height board font))

         (point (editor-pane-point board))
         (point-x (point-column point))
         (point-y (point-linenum point)))
    (capi:with-geometry board
      (setf (capi:simple-pane-font board) font
            @board.char-width char-w
            @board.char-height char-h
            @board.width (floor capi:%width% char-w)
            @board.height (floor capi:%height% char-h)
            (capi:text-input-range-value @itf.font-size-input) @board.project.font-size))
    (capi:map-pane-descendant-children
     itf
     (lambda (obj)
       (when (typep obj 'need-invalidate-after-style-change)
         (if (typep obj 'capi:pinboard-object)
           (capi:redraw-pinboard-object obj)
           (capi:apply-in-pane-process obj #'gp:invalidate-rectangle obj))))
     :do-pinboard-objects t
     :leaf-only t)
    (gp:invalidate-rectangle @itf.current)
    (refresh-board @itf.board)
    (when preserve-location
      (move-cursor-relative board point-x point-y))))

(defun refresh-style (pane)
  (let ((itf (ensure-element-interface pane)))
    (capi:map-pane-descendant-children
     itf
     (lambda (obj)
       (when (typep obj 'need-invalidate-after-style-change)
         (if (typep obj 'capi:pinboard-object)
           (capi:redraw-pinboard-object obj)
           (capi:apply-in-pane-process obj #'gp:invalidate-rectangle obj))))
     :do-pinboard-objects t
     :leaf-only t)
    (gp:invalidate-rectangle @itf.current)
    
    (setf (capi:choice-selected-items @itf.font-style-panel)
          (loop for i across (capi:collection-items @itf.font-style-panel)
                when (slot-value @itf.board (capi:item-data i)) collect i))))

(defun set-fg (color pane)
  (let ((itf (ensure-element-interface pane)))
    (setf @itf.board.fg color)
    (if (term-color-p color)
      (let ((spec (term-color-spec color))
            (alpha (term-color-alpha color)))
        (setf (capi:text-input-pane-text @itf.24-bit-fg-input)
              (spec-to-hex (color:color-with-alpha spec alpha)))
        (unless (= alpha @itf.fg-alpha)
          (setf @itf.fg-alpha alpha
                (capi:text-input-range-value @itf.fg-alpha-value) (round (* alpha 100))
                (capi:range-slug-start @itf.fg-alpha-slider) (round (* alpha 100)))))
      (progn
        (setf (capi:text-input-pane-text @itf.24-bit-fg-input) "")
        (setf @itf.fg-alpha 1.0
              (capi:text-input-range-value @itf.fg-alpha-value) 100
              (capi:range-slug-start @itf.fg-alpha-slider) 100)))
    (tool-set-fg-hook @itf.board.current-tool)
    (refresh-style itf)))

(defun set-bg (color pane)
  (let ((itf (ensure-element-interface pane)))
    (setf @itf.board.bg color)
    (if (term-color-p color)
      (let ((spec (term-color-spec color))
            (alpha (term-color-alpha color)))
        (setf (capi:text-input-pane-text @itf.24-bit-bg-input)
              (spec-to-hex (color:color-with-alpha spec alpha)))
        (unless (= alpha @itf.bg-alpha)
          (setf @itf.bg-alpha alpha
                (capi:text-input-range-value @itf.bg-alpha-value) (round (* alpha 100))
                (capi:range-slug-start @itf.bg-alpha-slider) (round (* alpha 100)))))
      (progn
        (setf (capi:text-input-pane-text @itf.24-bit-bg-input) "")
        (setf @itf.bg-alpha 1.0
              (capi:text-input-range-value @itf.bg-alpha-value) 100
              (capi:range-slug-start @itf.bg-alpha-slider) 100)))
    (tool-set-bg-hook @itf.board.current-tool)
    (refresh-style itf)))

(defun set-char (char pane)
  (let ((itf (ensure-element-interface pane)))
    (setf @itf.board.char char)
    (tool-set-char-hook @itf.board.current-tool)
    (refresh-style itf)))


;; Current state displayer

(defclass current-pane (capi:output-pane need-invalidate-after-style-change) ()
  (:default-initargs
   :title "Current"
   :name 'current
   :visible-min-width (list 'string (make-string 3 :initial-element #\Ideographic-Space))
   :visible-max-width t
   :display-callback (lambda (pane x y w h) (declare (ignore x y))
                       (let ((board @(capi:element-interface pane).board))
                         (capi:with-geometry pane
                           (let* ((font (gp:find-best-font pane (gp:make-font-description
                                                                 :family @board.project.font-family
                                                                 :size @board.project.font-size
                                                                 :weight (if @board.bold-p :bold :normal)
                                                                 :slant (if @board.italic-p :italic :roman))))
                                  (ascent (gp:get-font-ascent pane font))
                                  (char-width (gp:get-font-width pane font))
                                  (char-height (gp:get-font-height pane font)))
                             (gp:draw-rectangle pane 0 0 (or capi:%width% w) (or capi:%height% h)
                                                :foreground (if @board.bg (compose-two-colors
                                                                           *default-background*
                                                                           (color:color-to-premultiplied
                                                                            (term-color-spec-with-alpha @board.bg)))
                                                              *default-background*)
                                                :filled t)
                             (gp:draw-character pane @board.char
                                                (max 0 (/ (- (or capi:%width% w) char-width) 2))
                                                (+ (/ (- (or capi:%height% h) char-height) 2) ascent)
                                                :foreground (when @board.fg (term-color-spec @board.fg))
                                                :font font)))))
   :input-model `(((:key :press) ,(lambda (pane x y key)
                                    (declare (ignore x y))
                                    (awhen (gesture-spec-char key) (set-char it pane)))))))

(defmethod capi:pane-interface-paste-p ((current current-pane) itf)
  (capi:clipboard current))

(defmethod capi:pane-interface-paste-object ((current current-pane) itf)
  (set-char (char (capi:clipboard current) 0) itf))


;; Characters board

(defclass char-board (capi:output-pane need-invalidate-after-style-change)
  ((hover         :initform nil)
   (refresh-timer :initform nil)
   (pad-start     :initform 0))
  (:default-initargs
   :vertical-scroll t
   :display-callback #'char-board-display-callback
   :pane-menu (make 'capi:menu
                    :items         '("Custom Character Board")
                    :callback-type :none
                    :callback      #'custom-char-board)
   :input-model `(((:button-1 :release)
                   ,(lambda (pane x y)
                      (ignore-errors
                        (let ((w (gp:get-font-width pane))
                              (h (gp:get-font-height pane)))
                          (set-char (char *characters*
                                          (+ (* (floor y (+ h 2)) *char-board-columns*)
                                             (floor (- x @pane.pad-start) (+ w 2))))
                                    pane)
                          (gp:invalidate-rectangle pane)))))
                  (:motion ,(lambda (pane x y)
                              (ignore-errors
                                (let ((w (gp:get-font-width pane))
                                      (h (gp:get-font-height pane)))
                                  (setf @pane.hover (char *characters*
                                                          (+ (* (floor y (+ h 2)) *char-board-columns*)
                                                             (floor (- x @pane.pad-start) (+ w 2)))))
                                  (gp:invalidate-rectangle pane)
                                  (mp:schedule-timer-relative @pane.refresh-timer 1 1))))))))

(defmethod initialize-instance :after ((self char-board) &key)
  (setf @self.refresh-timer
        (mp:make-timer
         (lambda ()
           (unless (capi:pane-has-focus-p self)
             (setf @self.hover nil)
             (capi:apply-in-pane-process self #'gp:invalidate-rectangle self)
             :stop)))))

(defun char-board-display-callback (pane x y w h)
  (capi:with-geometry pane
    (let* ((itf         (capi:element-interface pane))
           (board       @itf.board)
           (dark-mode-p (capi:top-level-interface-dark-mode-p itf))
           (cols        *char-board-columns*)
           (rows        (floor (length *characters*) *char-board-columns*))
           (size        (floor (* (/ capi:%width% cols) 4/3)))
           (font        (gp:find-best-font
                         pane
                         (gp:make-font-description :family @board.project.font-family :size size)))
           (char-w      (gp:get-font-width pane font))
           (pad-start   (round (- capi:%width% (* (+ char-w 2) cols)) 2))
           (fg          (if @board.fg (color:color-to-premultiplied
                                       (term-color-spec-with-alpha @board.fg))
                          *default-foreground*))
           (bg          (if @board.bg (color:color-to-premultiplied
                                       (term-color-spec-with-alpha @board.bg))
                          *default-background*))
           (default-fg  (if dark-mode-p :gray40 :gray60))
           (default-bg  :transparent)
           (border-fg   (if dark-mode-p :gray30 :gray70))
           (gw          (+ char-w 2))
           (gh          (+ (gp:get-font-height pane font) 2))
           selected
           borders)
      (capi:set-hint-table pane (list :internal-min-height (* gh rows)
                                      :internal-max-height t))
      (setf @pane.pad-start pad-start
            (capi:simple-pane-font pane) font)
      (gp:draw-rectangle pane x y w h :foreground default-bg :filled t)
      (dorange$fixnum (y (floor y gh) (min (ceiling (+ y h) gh) rows))
        (dorange$fixnum (x (floor x gw) (min (ceiling (+ x w) gw) cols))
          (let* ((sx     (+ (* x gw) pad-start))
                 (sy     (* y gh))
                 (char   (char *characters* (+ (* y 16) x)))
                 (ascent (gp:get-char-ascent pane char font)))
            (gp:draw-character pane char
                               (+ sx 1) (+ sy 1 ascent)
                               :foreground (if (eql char @board.char) fg default-fg)
                               :background (if (eql char @board.char) bg default-bg)
                               :block t)
            (if (member char (list @board.char @pane.hover))
              (setq selected (nconc selected (list sx sy gw gh)))
              (setq borders  (nconc borders (list sx sy gw gh)))))))
      (gp:draw-rectangles pane borders :foreground border-fg)
      (gp:draw-rectangles pane selected
                          :foreground (if dark-mode-p :white :black)
                          :thickness  2))))

(capi:define-interface custom-char-board-popup ()
  ()
  (:panes
   (columns-range
    capi:text-input-range
    :title          "Columns:"
    :title-position :left
    :start 1        :end 99
    :value          *char-board-columns*
    :callback-type  :data-interface
    :callback       (lambda (data itf)
                      (capi:set-hint-table @itf.editor
                                           (list :visible-min-width (list 'character (1- data))
                                                 :visible-max-width t))))
    (editor
    capi:editor-pane
    :buffer             :temp
    :buffer-modes       '("Fundamental" "Visual Line")
    :text               *characters*
    :font               (gp:make-font-description :family *default-font-family* :size *default-font-size*)
    :line-wrap-marker   nil
    :line-wrap-face     nil
    :visible-min-width  (list 'character (1- *char-board-columns*))
    :visible-max-width  t
    :visible-min-height (list 'character (/ (length *characters*) *char-board-columns*)))
   (restore-default-button
    capi:push-button
    :text "Restore Default"
    :callback-type :interface
    :callback (lambda (itf)
                (setf (capi:text-input-range-value @itf.columns-range) *default-char-board-columns*
                      (capi:editor-pane-text @itf.editor)              *default-characters*)
                (capi:set-hint-table @itf.editor
                                     (list :visible-min-width (list 'character (1- *char-board-columns*))
                                           :visible-max-width t)))))
  (:layouts
   (main-layout
    capi:column-layout
    '(columns-range editor restore-default-button)
    :adjust :center)))

(defun custom-char-board ()
  (multiple-value-bind (pane okp)
      (capi:popup-confirmer
       (make 'custom-char-board-popup)
       "Custom Character Board")
    (when okp
      (setq *characters*         (capi:editor-pane-text @pane.editor)
            *char-board-columns* (capi:text-input-range-value @pane.columns-range))
      (dolist (itf (capi:collect-interfaces 'main-interface))
        (reinitialize-instance   @itf.char-board)
        (gp:invalidate-rectangle @itf.char-board)))))


;; Messager - "Echo Area"

(defclass messager (capi:editor-pane) ()
  (:default-initargs
   :buffer             :temp
   :background         :transparent
   :visible-min-height '(character 1)
   :visible-max-height t
   :vertical-scroll    nil
   :horizontal-scroll  nil
   :font               (gp:make-font-description :size 14)
   :enabled            nil))

#+lispworks8.1
(defun set-message (string itf)
  (let* ((pane @itf.messager)
         (window (capi:editor-window pane))
         (buffer (capi:editor-pane-buffer pane))
         (point (buffer-point buffer)))
    (let ((func (lambda ()
                  (clear-buffer buffer)
                  (editor::insert-buffer-string point string)
                  (buffer-start (buffer-point buffer))))) 
      (if window
        (let ((callback (capi:output-pane-display-callback pane)))
          (capi:output-pane-cache-display pane)
          (setf (capi:output-pane-display-callback pane) #'true)
          (process-character
           (lambda (p) (declare (ignore p))
             (funcall func)
             (capi:apply-in-pane-process
              pane
              (lambda ()
                (setf (capi:output-pane-display-callback pane) callback)
                (capi:output-pane-free-cached-display pane)
                (gp:invalidate-rectangle pane))))
           window))
        (funcall func)))))

#-lispworks8.1
(defun set-message (string itf)
  (let* ((pane @itf.messager)
         (window (capi:editor-window pane))
         (buffer (capi:editor-pane-buffer pane))
         (point (buffer-point buffer)))
    (let ((func (lambda ()
                  (clear-buffer buffer)
                  (editor::insert-buffer-string point string)
                  (buffer-start (buffer-point buffer)))))
      (if window
        (process-character
         (lambda (p) (declare (ignore p))
           (with-buffer-locked (buffer)
             (funcall func)))
         window)
        (funcall func)))))


;; Layer selector

(defclass layer-selector (capi:multi-column-list-panel) ()
  (:default-initargs
   :title                  "Layers"
   :title-position         :top
   :title-adjust           :center
   :alternating-background t
   :columns                '((:title "Name" :adjust :center :visible-min-width (string "Default Layer   "))
                             (:title "Visible" :adjust :right :visible-min-width :text-width) 
                             (:title "Opacity" :adjust :right :visible-min-width :text-width))
   :column-function        (lambda (layer)
                             (list (layer-name layer)
                                   (if (layer-visible layer) "✅" "❌")
                                   (round (* (layer-alpha layer) 100))))
   :print-function         #'layer-name
   :selection-callback     #'set-layer
   :action-callback        (lambda (layer itf)
                             (set-layer layer itf)
                             (toggle-layer-visible itf))
   #+lispworks8.1
   :editing-callback
   #+lispworks8.1
   (lambda (self index layer action data)
     (case action
       (:editp (member index '(0 2)))
       (:validp (case index
                  (0 (plusp (length data)))
                  (2 (every #'digit-char-p data))))
       (:set (let ((itf (capi:element-interface self)))
               (case index
                 (0 (setf (layer-name layer) data)
                    (capi:redisplay-collection-item self layer))
                 (2 (let ((data (parse-integer data :junk-allowed t)))
                      (setf @itf.board.current-layer.alpha (/ data 100)
                            (capi:range-slug-start @itf.layer-alpha-slider) data
                            (capi:text-input-range-value @itf.layer-alpha-value) data)
                      (capi:redisplay-collection-item @itf.layer-selector @itf.board.current-layer)
                      (refresh-board @itf.board))))))))))

(defmethod capi:make-pane-popup-menu ((pane layer-selector) itf &key)
  @itf.layers-menu)

;; Refresh functions

(defun set-layer (layer pane)
  (let ((itf (ensure-element-interface pane)))
    (unless (eq @itf.board.current-layer layer)
      (setf @itf.board.current-layer layer))
    (unless (eq (capi:choice-selected-item @itf.layer-selector) layer)
      (setf (capi:choice-selected-item @itf.layer-selector) layer))))

(defun set-project (project pane)
  (let ((itf (ensure-element-interface pane)))
    (setf @itf.board.saved t
          @itf.board.project project
          (capi:collection-items @itf.layer-selector) (project-layers project))
    (set-layer (first (project-layers project)) pane)
    (refresh-font itf)
    (refresh-board @itf.board)))

(defun toggle-layer-visible (pane)
  (let* ((itf (ensure-element-interface pane))
         (layer @itf.board.current-layer)
         (menu @itf.layer-visible-menu))
    (if (layer-visible layer)
      (setf (layer-visible layer) nil
            (capi:choice-selection menu) nil
            (capi:choice-selection @itf.layer-visible-component) nil)
      (setf (layer-visible layer) t
            (capi:choice-selection menu) 0
            (capi:choice-selection @itf.layer-visible-component) 0))
    (capi:redisplay-collection-item @itf.layer-selector layer)
    (refresh-board @itf.board)))


;; Main interface

#|(capi:define-layout expanded-column-layout (capi:column-layout) ())

(defmethod capi:calculate-layout :after ((layout expanded-column-layout) x y w h)
  (let (widths)
    (capi:map-pane-children
     layout
     (lambda (child)
       (capi:with-geometry child
         (push capi:%width% widths)))
     :do-pinboard-objects t)
    (let ((width (min w (reduce #'max widths))))
      (block nil
        (capi:map-pane-children
         layout
         (lambda (child)
           (capi:with-geometry child
             (setf capi:%width% width)
             (incf y capi:%min-height%)))
         :reverse t
         :do-pinboard-objects t)))))|#

(capi:define-interface main-interface ()
  ((tools :initform nil)
   (file :initform nil)
   (app-interface :initform nil
                  :initarg :app-interface)
   (board :initform nil)
   (fg-alpha :initform 1.0)
   (bg-alpha :initform 1.0)
   (copy-option :initform :text))
  (:panes
   (current current-pane)
   (char-board char-board)
   (messager messager)
   (layer-selector layer-selector)
   (cursor-movement-option
    capi:option-pane
    :name 'cursor-movement-option
    :title "Cursor Movement"
    :items '(:left :right :up :down)
    :selected-item :right
    :visible-min-width '(string "Right")
    :visible-max-width t
    :print-function #'string-capitalize
    :selection-callback (lambda (data itf) (setf @itf.board.cursor-movement data)))
   (settings-button
    capi:toolbar-button
    :text "Settings"
    :name 'settings
    :image 'settings
    :callback-type :interface
    :callback (lambda (itf)
                (capi:popup-confirmer
                 (make 'settings-interface :parent itf)
                 "Settings"
                 :cancel-button nil)))
   
   ;; Colors
   (4-bit-grid
    capi:output-pane
    :display-callback #'4-bit-board-display-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height (/ w 4))
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model (flet ((func1 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-fg (coerce-color-to 4 (4-bit-board-translate-position pane x y) @itf.fg-alpha) pane)
                            (capi:set-pane-focus @itf.board)))
                        (func3 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-bg (coerce-color-to 4 (4-bit-board-translate-position pane x y) @itf.bg-alpha) pane)
                            (capi:set-pane-focus @itf.board))))
                   `(((:button-1 :press ) ,#'func1)
                     ((:button-1 :motion) ,#'func1)
                     ((:button-3 :press ) ,#'func3)
                     ((:button-3 :motion) ,#'func3))))
   (8-bit-spectrum
    capi:output-pane
    :display-callback #'8-bit-spectrum-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height w)
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model (flet ((func1 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-fg (coerce-color-to 8 (spectrum-translate-position pane x y) @itf.fg-alpha) pane)
                            (capi:set-pane-focus @itf.board)))
                        (func3 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-bg (coerce-color-to 8 (spectrum-translate-position pane x y) @itf.bg-alpha) pane)
                            (capi:set-pane-focus @itf.board))))
                   `(((:button-1 :press ) ,#'func1)
                     ((:button-1 :motion) ,#'func1)
                     ((:button-3 :press ) ,#'func3)
                     ((:button-3 :motion) ,#'func3))))
   (8-bit-grayscale
    capi:output-pane
    :visible-min-height '(character 1)
    :display-callback #'8-bit-grayscale-display-callback
    :input-model (flet ((func1 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-fg (coerce-color-to 8 (8-bit-grayscale-translate-position pane x y) @itf.fg-alpha) pane)
                            (capi:set-pane-focus @itf.board)))
                        (func3 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-bg (coerce-color-to 8 (8-bit-grayscale-translate-position pane x y) @itf.bg-alpha) pane)
                            (capi:set-pane-focus @itf.board))))
                   `(((:button-1 :press ) ,#'func1)
                     ((:button-1 :motion) ,#'func1)
                     ((:button-3 :press ) ,#'func3)
                     ((:button-3 :motion) ,#'func3))))
   (24-bit-spectrum
    capi:output-pane
    :display-callback #'spectrum-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height w)
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model (flet ((func1 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-fg (coerce-color-to 24 (spectrum-translate-position pane x y) @itf.fg-alpha) pane)
                            (capi:set-pane-focus @itf.board)))
                        (func3 (pane x y)
                          (let ((itf (capi:element-interface pane)))
                            (set-bg (coerce-color-to 24 (spectrum-translate-position pane x y) @itf.bg-alpha) pane)
                            (capi:set-pane-focus @itf.board))))
                   `(((:button-1 :press ) ,#'func1)
                     ((:button-1 :motion) ,#'func1)
                     ((:button-3 :press ) ,#'func3)
                     ((:button-3 :motion) ,#'func3))))
   (24-bit-fg-input
    capi:text-input-pane
    :title "F:" :title-position :left
    :change-callback-type :data-interface
    :text-change-callback (lambda (data itf)
                            (if (> (length data) 0)
                              (awhen (hex-to-spec data)
                                (set-fg (coerce-color-to 24 it @itf.fg-alpha) itf))
                              (set-fg nil itf))))
   (24-bit-bg-input
    capi:text-input-pane
    :title "B:" :title-position :left
    :change-callback-type :data-interface
    :text-change-callback (lambda (data itf)
                            (if (> (length data) 0)
                              (awhen (hex-to-spec data)
                                (set-bg (coerce-color-to 24 it @itf.bg-alpha) itf))
                              (set-bg nil itf))))
   (color-picker-prompt-1
    capi:title-pane :text "Left click to set foreground")
   (color-picker-prompt-2
    capi:title-pane :text "Right click to set background")
   (restore-default-colors
    capi:push-button
    :text "Restore default colors"
    :callback-type :interface
    :callback (op (set-fg nil _1) (set-bg nil _1)))
   ;; Alpha
   (fg-alpha-value
    capi:text-input-range
    :start 0 :end 100 :value 100
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.fg-alpha (/ data 100)
                      (capi:range-slug-start @itf.fg-alpha-slider) data)
                (let ((new (copy-term-color @itf.board.fg)))
                  (setf (term-color-alpha new) @itf.fg-alpha)
                  (set-fg new itf))))
   (fg-alpha-slider
    capi:slider
    :title "F:" :title-position :left
    :start 0 :end 100 :slug-start 100
    :callback (lambda (self how where)
                (declare (ignore how where))
                (let ((itf (capi:element-interface self))
                      (data (capi:range-slug-start self)))
                  (setf @itf.fg-alpha (/ data 100)
                        (capi:text-input-range-value @itf.fg-alpha-value) data)
                  (let ((new (copy-term-color @itf.board.fg)))
                    (setf (term-color-alpha new) @itf.fg-alpha)
                    (set-fg new itf)))))
   (bg-alpha-value
    capi:text-input-range
    :start 0 :end 100 :value 100
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.bg-alpha (/ data 100)
                      (capi:range-slug-start @itf.bg-alpha-slider) data)
                (let ((new (copy-term-color @itf.board.bg)))
                  (setf (term-color-alpha new) @itf.bg-alpha)
                  (set-bg new itf))))
   (bg-alpha-slider
    capi:slider
    :title "B:" :title-position :left
    :start 0 :end 100 :slug-start 100
    :callback (lambda (self how where)
                (declare (ignore how where))
                (let ((itf (capi:element-interface self))
                      (data (capi:range-slug-start self)))
                  (setf @itf.bg-alpha (/ data 100)
                        (capi:text-input-range-value @itf.bg-alpha-value) data)
                  (let ((new (copy-term-color @itf.board.bg)))
                    (setf (term-color-alpha new) @itf.bg-alpha)
                    (set-bg new itf)))))

   ;; Offset
   (x-offset-input
    capi:text-input-pane
    :text "0"
    :alignment :right
    :visible-min-width '(character 4)
    :visible-max-width t
    :change-callback-type '(:data :element :interface)
    :text-change-callback (lambda (data self itf)
                            (let* ((str (remove-if-not (lambda (c) (or (digit-char-p c) (eql c #\-))) data))
                                   (num (parse-integer str :junk-allowed t)))
                              (when num
                                (setf (capi:text-input-pane-text self) (princ-to-string num)
                                      @itf.board.x-offset num)
                                (refresh-board @itf.board)))))
   (y-offset-input
    capi:text-input-pane
    :title "×" :title-position :left :title-gap 0
    :text "0"
    :alignment :right
    :visible-min-width '(character 4)
    :visible-max-width t
    :change-callback-type '(:data :element :interface)
    :text-change-callback (lambda (data self itf)
                            (let* ((str (remove-if-not (lambda (c) (or (digit-char-p c) (eql c #\-))) data))
                                   (num (parse-integer str :junk-allowed t)))
                              (when num
                                (setf (capi:text-input-pane-text self) (princ-to-string num)
                                      @itf.board.y-offset num)
                                (refresh-board @itf.board)))))

   ;; Font
   (font-size-input
    capi:text-input-range
    :name 'font-size-input
    :title "Font Size"
    :start 5 :end 999 :value *default-font-size*
    :visible-min-width '(character 4)
    :visible-max-width t
    :change-callback (lambda (data pane itf caret)
                       (declare (ignore pane caret))
                       (let* ((str (remove-if-not (lambda (c) (or (digit-char-p c) (eql c #\-))) data))
                              (num (parse-integer str :junk-allowed t)))
                         (when num
                           (setf @itf.board.project.font-size (max num 5))
                           (refresh-font itf t))))
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.board.project.font-size (max data 5))
                (refresh-font itf t)))
   (font-style-panel
    capi:check-button-panel
    :layout-class 'capi:row-layout
    :items (list (make 'capi:item :data 'bold-p :text "Bold")
                 (make 'capi:item :data 'italic-p :text "Italic")
                 (make 'capi:item :data 'underline-p :text "Underline"))
    :selection-callback (lambda (data itf)
                          (setf (slot-value @itf.board data) t)
                          (refresh-style itf))
    :retract-callback   (lambda (data itf)
                          (setf (slot-value @itf.board data) nil)
                          (refresh-style itf)))

   ;; Layers
   (layer-alpha-slider
    capi:slider
    :title "Opacity:" :title-position :left
    :start 0 :end 100 :slug-start 100
    :callback (lambda (self how where)
                (declare (ignore how where))
                (let ((itf (capi:element-interface self))
                      (data (capi:range-slug-start self)))
                  (setf @itf.board.current-layer.alpha (/ data 100)
                        (capi:text-input-range-value @itf.layer-alpha-value) data)
                  (capi:redisplay-collection-item @itf.layer-selector @itf.board.current-layer)
                  (refresh-board @itf.board))))
   (layer-alpha-value
    capi:text-input-range
    :start 0 :end 100 :value 100
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.board.current-layer.alpha (/ data 100)
                      (capi:range-slug-start @itf.layer-alpha-slider) data)
                (capi:redisplay-collection-item @itf.layer-selector @itf.board.current-layer)
                (refresh-board @itf.board)))
   (move-layer-up-button
    capi:toolbar-button
    :image 'up
    :remapped 'move-layer-up)
   (move-layer-down-button
    capi:toolbar-button
    :image 'down
    :remapped 'move-layer-down)
   (add-layer-button
    capi:toolbar-button
    :image 'add
    :remapped 'add-layer)
   (delete-layer-button
    capi:toolbar-button
    :image 'remove
    :remapped 'delete-layer)
   (layer-visible-button
    capi:toolbar-button
    :selected nil
    :image 'visible
    :selected-image 'invisible
    :remapped 'layer-invisible)
   (layer-visible-component
    capi:toolbar-component
    :items '(layer-visible-button)
    :selection nil
    :interaction :single-selection)
   (layout-toolbar
    capi:toolbar
    :flatp t
    :image-width 20 :image-height 20
    :button-width 32 :button-height 32))
  
  (:layouts
   (main-layout             capi:row-layout
                            '(left-column :divider center-column :divider right-column)
                            :ratios '(1 nil 3 nil 1))
   (center-column           capi:column-layout
                            '(container messager))
   (container               board-container nil)
   
   (left-column             capi:column-layout
                            '(offset-row
                              :separator
                              tool-settings-container
                              :divider
                              layer-selector
                              layout-alpha-row
                              layout-toolbar)
                            :adjust :right)
   (offset-row              capi:row-layout
                            '(x-offset-input
                              y-offset-input)
                            :adjust :right
                            :gap 0)
   (tool-settings-container capi:column-layout
                            '()
                            :background :transparent
                            :vertical-scroll t)
   (layout-alpha-row        capi:row-layout
                            '(layer-alpha-slider
                              layer-alpha-value))
   
   (right-column            capi:column-layout
                            '(colors-tab
                              alpha-column
                              restore-default-colors
                              :separator
                              font-style-panel
                              char-board)
                            :adjust :center)
   
   (colors-tab              capi:tab-layout
                            ()
                            :items '(4-bit 8-bit 24-bit)
                            :print-function #'string-downcase
                            :visible-child-function #'identity)
   (4-bit                   capi:column-layout
                            '(color-picker-prompt-1 color-picker-prompt-2 4-bit-grid)
                            :adjust :center)
   (8-bit                   capi:column-layout
                            '(8-bit-spectrum 8-bit-grayscale)
                            :adjust :center)
   (24-bit                  capi:column-layout
                            '(24-bit-spectrum 24-bit-input-row)
                            :adjust :center)
   (24-bit-input-row        capi:row-layout
                            '(24-bit-fg-input 24-bit-bg-input))
   
   (alpha-column            capi:column-layout
                            '(fg-alpha-row bg-alpha-row)
                            :title "Opacity"
                            :title-position :top
                            :title-adjust :center
                            :title-gap 0
                            :gap 0)
   (fg-alpha-row            capi:row-layout
                            '(fg-alpha-slider fg-alpha-value))
   (bg-alpha-row            capi:row-layout
                            '(bg-alpha-slider bg-alpha-value)))
  (:menus
   (file-menu
    "File"
    ((:component
      (("New"        :name 'file-new     :callback #'file-new)
       ("Open..."    :name 'file-open    :callback #'file-open)
       ("Save"       :name 'file-save    :callback #'file-save)
       ("Save As..." :name 'file-save-as :callback #'file-save-as)
       ("Export"     :name 'file-export  :callback #'file-export))
      :callback-type :interface)))
   (edit-menu
    "Edit"
    ((:component
      (("Undo" :callback #'(lambda (itf)
                             (if (capi:pane-has-focus-p @itf.board)
                               (board-undo itf)
                               (capi:active-pane-undo itf)))
        :enabled-function #'capi:active-pane-undo-p)))
     (:component
      (("Cut"   :callback #'(lambda (itf)
                              (if (capi:pane-has-focus-p @itf.board)
                                (board-cut itf)
                                (capi:active-pane-cut itf)))
        :enabled-function #'capi:active-pane-cut-p)
       ("Copy"  :callback #'(lambda (itf)
                              (if (capi:pane-has-focus-p @itf.board)
                                (board-copy itf)
                                (capi:active-pane-copy itf)))
        :enabled-function #'capi:active-pane-copy-p)
       ("Paste" :callback #'(lambda (itf)
                              (if (capi:pane-has-focus-p @itf.board)
                                (board-paste itf)
                                (capi:active-pane-paste itf)))
        :enabled-function #'capi:active-pane-paste-p)))

     (:component
      (("Copy to plain text"            :callback (op (setf @_.copy-option :text)))
       ("Copy to HTML"                  :callback (op (setf @_.copy-option :html)))
       ("Copy to ANSI Escaped sequence" :callback (op (setf @_.copy-option :ansi))))
      :interaction :single-selection)
     
     (:component
      (("Move Left after insert"  :data :left  :callback #'change-cursor-movement :accelerator "accelerator-shift-left")
       ("Move Right after insert" :data :right :callback #'change-cursor-movement :accelerator "accelerator-shift-right")
       ("Move Up after insert"    :data :up    :callback #'change-cursor-movement :accelerator "accelerator-shift-up")
       ("Move Down after insert"  :data :down  :callback #'change-cursor-movement :accelerator "accelerator-shift-down"))
      :selected-item :right
      :interaction :single-selection
      :callback-type :data-interface)
     
     ("Custom Character Board"
      :callback-type :none
      :callback #'custom-char-board))
    :callback-type :interface)
   (layer-visible-menu
    :component
    (("Visible" :name 'layer-invisible))
    :callback #'toggle-layer-visible
    :callback-type :interface
    :interaction :single-selection)
   (layers-menu
    "Layers"
    ((:component
      (("Add Layer" :name 'add-layer
        :callback (lambda (itf)
                    (let ((new (make-layer :name "New Layer")))
                      (push new @itf.board.project.layers)
                      (setf (capi:collection-items @itf.layer-selector) @itf.board.project.layers)
                      (set-layer new itf)
                      #-lispworks8.1
                      (setf (layer-name new)
                            (or (capi:prompt-for-string "Layer Name") "New Layer"))
                      #+lispworks8.1
                      (progn
                        (capi:collection-item-edit @itf.layer-selector new)
                        (capi:collection-item-set-editing-string @itf.layer-selector "New Layer")))))
       ("Delete Layer" :name 'delete-layer
        :callback (lambda (itf)
                    (let ((layer (capi:choice-selected-item @itf.layer-selector))
                          (layers @itf.board.project.layers))
                      (if (= (length layers) 1)
                        (capi:prompt-with-message "Cannot delete the only layer!")
                        (progn
                          (setf layers (delete layer layers)
                                @itf.board.project.layers layers
                                (capi:collection-items @itf.layer-selector) layers)
                          (set-layer (first layers) itf)
                          (refresh-board @itf.board))))))
       layer-visible-menu
       ("Move Current Layer Up" :name 'move-layer-up
        :callback (lambda (itf)
                    (let ((index (capi:choice-selection @itf.layer-selector)))
                      (when (plusp index)
                        (rotatef (nth (1- index) @itf.board.project.layers)
                                 (nth index @itf.board.project.layers))
                        (setf (capi:collection-items @itf.layer-selector) @itf.board.project.layers
                              (capi:choice-selection @itf.layer-selector) (1- index))
                        (refresh-board @itf.board)))))
       ("Move Current Layer Down" :name 'move-layer-down
        :callback (lambda (itf)
                    (let ((index (capi:choice-selection @itf.layer-selector)))
                      (when (< index (1- (length @itf.board.project.layers)))
                        (rotatef (nth index @itf.board.project.layers)
                                 (nth (1+ index) @itf.board.project.layers))
                        (setf (capi:collection-items @itf.layer-selector) @itf.board.project.layers
                              (capi:choice-selection @itf.layer-selector) (1+ index))
                        (refresh-board @itf.board))))))))
    :callback-type :interface)
   (tools-menu
    "Tools"
    ((:component nil
      :items-function
      (lambda (itf) (declare (ignore itf))
        (with-collector (c)
          (dolist* (i name *all-tools-names*)
            (let ((name name))
              (c (make 'capi:menu-item
                       :name name
                       :text (string-capitalize name)
                       :accelerator (char (princ-to-string i) 0)
                       :callback (lambda (itf) (set-tool itf @itf.board name))))))))
      :callback-type :interface)))
   (window-menu
    "Window"
    ((:component
      (("Close Window" :callback #'capi:quit-interface :accelerator #\w)
       ("Minimize" :callback (op (setf (capi:top-level-interface-display-state _) :iconic))
        :accelerator #\m))))
    :callback-type :interface)
   (help-menu
    "Help"
    ((:component
      (("Contact developer" :callback (op (sys:open-url "http://apr.sdf.org/contact.html")))
       ("Privacy Policy" :callback (op (capi:display (make 'privacy-interface)))))))
    :callback-type :none))
  (:menu-bar file-menu edit-menu layers-menu tools-menu window-menu help-menu)
  (:default-initargs
   :title "Charapainter"
   :confirm-destroy-callback #'check-saved
   :destroy-callback (op (awhen @_.app-interface
                           (when (null (capi:collect-interfaces 'main-interface))
                             (capi:destroy it))))
   :best-width '(* :screen-width 0.8)
   :best-height '(* :screen-height 0.65)
   :initial-focus 'container))

(defmethod capi:interface-keys-style ((self main-interface)) :emacs)

(defmethod initialize-instance :after ((self main-interface) &key)
  (setf @self.board @self.container.board
        @self.tools (mapcar (op (make _ :board @self.board)) *all-tools-names*))
  (setf (capi:interface-toolbar-items self)
        (list @self.current
              (make 'capi:toolbar-component
                    :items (mapcar (lambda (name)
                                     (make 'capi:toolbar-button
                                           :name name :data name :image name :remapped name
                                           :print-function #'string-capitalize))
                                   *all-tools-names*))
              @self.font-size-input
              @self.cursor-movement-option
              @self.settings-button)
        
        (capi:interface-toolbar-state self :items)
        (append '(:flexible-space current :space)
                *all-tools-names*
                '(font-size-input cursor-movement-option :space settings :flexible-space))
        
        (capi:collection-items @self.layer-selector) (project-layers @self.board.project)

        (capi:collection-items @self.layer-visible-component)
        (list @self.layer-visible-button)

        (capi:collection-items @self.layout-toolbar)
        (list @self.layer-visible-component
              @self.move-layer-up-button
              @self.move-layer-down-button
              @self.add-layer-button
              @self.delete-layer-button))
  (set-tool self @self.board 'brush))

(defmethod set-tool-internal :after ((itf main-interface) board tool)
  (when (eq @itf.board.current-tool tool)
    (unless @tool.settings-layout
      (setf @tool.settings-layout (make-settings-layout itf tool)))
    (setf (capi:layout-description @itf.tool-settings-container) (list @tool.settings-layout))))


;; Settings & other interfaces

(capi:define-interface settings-interface ()
  ((parent :initarg :parent))
  (:panes
   (font-option
    capi:push-button
    :title "" :title-position :left
    :text "Choose Font"
    :callback-type :element-interface
    :callback (lambda (self itf)
                (let ((proj @itf.parent.board.project))
                  (when-let (font (capi:prompt-for-font
                                   "Select a Font"
                                   :font (gp:make-font-description :family (project-font-family proj)
                                                                   :size (project-font-size proj))))
                    (let* ((desc (gp:font-description font))
                           (family (gp:font-description-attribute-value desc :family))
                           (size (gp:font-description-attribute-value desc :size)))
                      (setf (project-font-family proj) family
                            (project-font-size proj) size
                            (capi:titled-pane-title self) (make-font-describe-string family size))
                      (save-settings)
                      (refresh-font @itf.parent t))))))
   (cursor-option
    capi:option-pane
    :title "Cursor shape:" :title-position :left
    :items '(:inverse :underline :caret :left-bar :outline)
    :selected-item capi:*editor-cursor-active-style*
    :print-function #'string-capitalize
    :visible-max-width :text-width
    :callback-type :data
    :selection-callback (op (setq capi:*editor-cursor-active-style* _)
                          (save-settings)))
   (default-fg
    capi:output-pane
    :visible-min-width '(character 8)
    :visible-min-height '(character 2)
    :visible-max-width t
    :visible-max-height t
    :title "Default Foreground:" :title-position :left
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y w h))
                        (capi:with-geometry pane
                          (gp:draw-rectangle pane 0 0 capi:%width% capi:%height%
                                             :foreground *default-foreground* :filled t)))
    :input-model `(((:button-1 :press) ,(lambda (pane x y)
                                          (declare (ignore x y))
                                          (multiple-value-bind (r okp)
                                              (capi:prompt-for-color "Choose a color")
                                            (when okp
                                              (setq *default-foreground* r)
                                              (dolist (itf (capi:collect-interfaces 'main-interface))
                                                (setf (capi:simple-pane-foreground @itf.board) r
                                                      (capi:simple-pane-foreground @itf.current) r)
                                                (gp:invalidate-rectangle @itf.current))
                                              (gp:invalidate-rectangle pane)))))))
   (default-bg
    capi:output-pane
    :visible-min-width '(character 8)
    :visible-min-height '(character 2)
    :visible-max-width t
    :visible-max-height t
    :title "Default Foreground:" :title-position :left
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y w h))
                        (capi:with-geometry pane
                          (gp:draw-rectangle pane 0 0 capi:%width% capi:%height%
                                             :foreground *default-background* :filled t)))
    :input-model `(((:button-1 :press) ,(lambda (pane x y)
                                          (declare (ignore x y))
                                          (multiple-value-bind (r okp)
                                              (capi:prompt-for-color "Choose a color")
                                            (when okp
                                              (setq *default-background* r)
                                              (dolist (itf (capi:collect-interfaces 'main-interface))
                                                (setf (capi:simple-pane-background @itf.board) r
                                                      (capi:simple-pane-background @itf.current) r)
                                                (gp:invalidate-rectangle @itf.current))
                                              (gp:invalidate-rectangle pane)))))))
   (note
    capi:display-pane
    :visible-min-width '(+ :text-width 1)
    :text "Note for default colors:

These colors are only used inside the Charapainter editor and when you
export/copy to image.

When exporting to HTML or ANSI escaped sequences, the default
foreground & background color of your browser/terminal will be used.")
   (reset-default
    capi:push-button
    :text "Reset all settings to default"
    :callback-type :interface
    :callback (lambda (itf)
                (set-variables-default)
                (save-settings)
                (refresh-font itf)
                (gp:invalidate-rectangle @itf.selection-border-pane))))
  (:layouts
   (main-layout
    capi:column-layout
    '(font-option cursor-option default-fg default-bg :separator note :separator reset-default)
    :adjust :center)))

(defmethod initialize-instance :after ((self settings-interface) &key)
  (let ((proj @self.parent.board.project))
    (setf (capi:titled-pane-title @self.font-option)
          (make-font-describe-string (project-font-family proj)
                                     (project-font-size proj)))))

(capi:define-interface about-interface ()
  ()
  (:panes
   (logo
    capi:output-pane
    :visible-min-width (capi:screen-logical-resolution (capi:convert-to-screen))
    :visible-min-height (capi:screen-logical-resolution (capi:convert-to-screen))
    :visible-max-width t
    :visible-max-height t
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y w h))
                        (capi:with-geometry pane
                          (gp:draw-image pane (gp:load-image pane 'logo)
                                         0 0 :to-width capi:%width% :to-height capi:%height%))))
   (version capi:title-pane :text (string-append "Charapainter version " *version*))
   (desc1 capi:title-pane :text "The text drawing software by April & May")
   (desc2 capi:title-pane :text "Build with LispWorks")
   (desc3 capi:push-button :title "Icons by" :text "Icons8"
          :callback (lambda (&rest args) (declare (ignore args))
                      (sys:open-url "https://icons8.com")))
   (desc4 capi:title-pane :text "Supporting Neurodiversity / Transgender / Plurality"))
  (:layouts
   (main-layout
    capi:column-layout
    '(logo version desc1 desc2 desc3 desc4)
    :internal-border 30
    :adjust :center))
  (:default-initargs :title "About Charapainter"))

(capi:define-interface privacy-interface () ()
  (:panes (text capi:display-pane :text *privacy-policy*))
  (:default-initargs :title "Privacy Policy"))

#+darwin
(defclass app-interface (capi:cocoa-default-application-interface)
  ()
  (:default-initargs
   :message-callback
   (lambda (itf msg &rest args)
     (case msg
       (:open-file
        (let ((main (make 'main-interface :app-interface itf)))
          (file-open-internal main (first args))
          (capi:display main)))
       (:finished-launching
        (capi:display (make 'main-interface :app-interface itf)))))
   :application-menu
   (make 'capi:menu
         :title "Charapainter"
         :items (list (make 'capi:menu-item
                            :text "About Charapainter"
                            :callback (lambda (itf) (declare (ignore itf))
                                        (capi:display (make 'about-interface))))
                      (make 'capi:menu-item
                            :text "Quit Charapainter"
                            :accelerator #\q
                            :callback (op (unless (member nil (mapcar (op (capi:quit-interface _))
                                                                      (capi:collect-interfaces 'main-interface)))
                                            (capi:destroy _)))))
         :callback-type :interface)))


;; Main

(defun main ()
  (setq *debugger-hook*
        (lambda (cond self)
          (declare (ignore self))
          (let* ((cond-str (format nil "~A" cond))
                 (log-file (dbg:log-bug-form (format nil "Condition: ~A" cond-str))))
            (when (capi:prompt-for-confirmation
                   (format nil "Charapainter meets an internal error that cannot handle. Your most recent action cannot be done.

The error has been logged to ~A

Click 'Yes' to open the log folder, or 'No' to continue."
                           (namestring log-file)))
              (sys:call-system (string-append "open " (namestring (pathname-location log-file)))))
            (abort cond))))
  #+darwin
  (progn
    (capi:set-application-interface (make 'app-interface))
    (load-settings)
    (capi:convert-to-screen nil))
  #-darwin
  (progn
    (load-settings)
    (capi:display (make 'main-interface))))

(export 'main)

;(capi:contain (make 'main-interface))

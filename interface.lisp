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

;; Current state displayer

(defun set-fg (color)
  (setq *fg* color)
  (dolist (itf (capi:collect-interfaces 'main-interface))
    (setf (capi:text-input-pane-text @itf.24-bit-fg-input)
          (if color (spec-to-hex (term-color-spec color)) ""))
    (gp:invalidate-rectangle @itf.current)))

(defun set-bg (color)
  (setq *bg* color)
  (dolist (itf (capi:collect-interfaces 'main-interface))
    (setf (capi:text-input-pane-text @itf.24-bit-bg-input)
          (if color (spec-to-hex (term-color-spec color)) ""))
    (gp:invalidate-rectangle @itf.current)))

(defun set-char (char)
  (setq *char* char)
  (dolist (itf (capi:collect-interfaces 'main-interface))
    (gp:invalidate-rectangle @itf.current)))

(defclass current-pane (capi:output-pane) ()
  (:default-initargs
   :title "Current"
   :name 'current
   :visible-min-width (* *char-width* 3)
   :visible-max-width t
   :display-callback (lambda (pane x y w h) (declare (ignore x y))
                       (capi:with-geometry pane
                         (let* ((font (gp:find-best-font pane *fdesc*))
                                (ascent (gp:get-char-ascent pane *char* font)))
                           (if *bg*
                             (gp:draw-rectangle pane 0 0 (or capi:%width% w) (or capi:%height% h)
                                                :foreground (term-color-spec *bg*)
                                                :filled t)
                             (gp:clear-rectangle pane 0 0 (or capi:%width% w) (or capi:%height% h)))
                           (gp:draw-character pane *char* *char-width* (+ (/ (- (or capi:%height% h) *char-height*) 2) ascent)
                                              :foreground (when *fg* (term-color-spec *fg*))
                                              :font font))))
   :input-model `(((:key :press) ,(lambda (pane x y key)
                                    (declare (ignore pane x y))
                                    (awhen (gesture-spec-char key) (set-char it)))))))

(defmethod capi:pane-interface-paste-p ((current current-pane) itf)
  (capi:clipboard current))

(defmethod capi:pane-interface-paste-object ((current current-pane) itf)
  (set-char (char (capi:clipboard current) 0)))

;; Characters board

(defun char-board-display-callback (pane x y w h)
  (capi:with-geometry pane
    (let* ((font (gp:find-best-font
                  pane
                  (gp:make-font-description :family *font-family* :size *font-size*)))
           (gh (+ (gp:get-font-height pane font) 2))
           (gw (+ (gp:get-font-width pane font) 2))
           black-lines white-lines)
      (dorange$fixnum (y (floor y gh) (min (ceiling (+ y h) gh) (length *characters*)))
        (dorange$fixnum (x (floor x gw) (min (ceiling (+ x w) gw) (length (aref *characters* 0))))
          (let* ((sx (* x gw))
                 (sy (* y gh))
                 (char (char (svref *characters* y) x))
                 (ascent (gp:get-char-ascent pane char font)))
            (nconcf black-lines
                    (list sx sy (+ sx gw) sy)
                    (list sx sy sx (+ sy gh)))
            (nconcf white-lines
                    (list (+ sx gw) sy (+ sx gw) (+ sy gh))
                    (list sx (+ sy gh) (+ sx gw) (+ sy gh)))
            (gp:draw-character pane char
                               sx (+ sy ascent)
                               :foreground :white
                               :background :black
                               :block      t))))
      (gp:draw-lines pane (mapcar #'1- white-lines) :foreground :gray50))))

;; Main interface

(capi:define-interface main-interface ()
  ((tools :initform nil)
   (file :initform nil)
   (app-interface :initform nil
                  :initarg :app-interface))
  (:panes
   (board board)
   (current current-pane)
   (char-board
    capi:output-pane
    :title "Characters"
    :title-position :top
    :title-adjust :center
    :visible-min-width *char-board-width*
    :visible-min-height *char-board-height*
    :visible-max-width t
    :visible-max-height t
    :display-callback #'char-board-display-callback
    :font *fdesc*
    :input-model `(((:button-1 :press)
                    ,(lambda (pane x y)
                       (declare (ignore pane))
                       (set-char (char (aref *characters* (floor y *char-board-grid-height*))
                                       (floor x *char-board-grid-width*)))))))
   
   (cursor-movement-option
    capi:option-pane
    :name 'cursor-movement-option
    :title "Cursor Movement"
    :items '(:right :down )
    :visible-min-width '(string "Right")
    :visible-max-width t
    :print-function #'string-capitalize
    :selection-callback (lambda (data itf) (setf @itf.board.cursor-movement data)))
   (copy-option
    capi:option-pane
    :name 'copy-option
    :title "Copy to"
    :items '("HTML"
             "Escaped sequence"
             "Plain text")
    :visible-max-width t)
   (settings-button
    capi:toolbar-button
    :text "Settings"
    :name 'settings
    :image 'settings
    :callback (lambda (&rest args)
                (declare (ignore args))
                (capi:popup-confirmer (make 'settings-interface) "Settings"
                                      :cancel-button nil)))
   (4-bit-grid
    capi:output-pane
    :display-callback #'4-bit-board-display-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height (/ w 4))
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model `(((:button-1 :press)
                    ,(op (set-fg (4-bit-board-translate-position _ _ _))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))
                   ((:button-3 :press)
                    ,(op (set-bg (4-bit-board-translate-position _ _ _))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))))
   (8-bit-spectrum
    capi:output-pane
    :display-callback #'8-bit-spectrum-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height w)
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model `(((:button-1 :press)
                    ,(op (set-fg (coerce-color-to 8 (spectrum-translate-position _ _ _)))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))
                   ((:button-3 :press)
                    ,(op (set-bg (coerce-color-to 8 (spectrum-translate-position _ _ _)))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))))
   (8-bit-grayscale
    capi:output-pane
    :display-callback #'8-bit-grayscale-display-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height (/ w 24))
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model `(((:button-1 :press)
                    ,(op (set-fg (8-bit-grayscale-translate-position _ _ _))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))
                   ((:button-3 :press)
                    ,(op (set-bg (8-bit-grayscale-translate-position _ _ _))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))))
   (24-bit-spectrum
    capi:output-pane
    :display-callback #'spectrum-callback
    :resize-callback (lambda (pane x y w h)
                       (declare (ignore x y h))
                       (capi:set-geometric-hint pane :visible-min-height w)
                       (capi:set-geometric-hint pane :visible-max-height t))
    :input-model `(((:button-1 :press)
                    ,(op (set-fg (coerce-color-to 24 (spectrum-translate-position _ _ _)))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))
                   ((:button-3 :press)
                    ,(op (set-bg (coerce-color-to 24 (spectrum-translate-position _ _ _)))
                       (capi:set-pane-focus (slot-value (capi:element-interface _1) 'board))))))
   (24-bit-fg-input
    capi:text-input-pane
    :title "Fg:" :title-position :left
    :change-callback-type :data
    :text (if *fg* (spec-to-hex (term-color-spec *fg*)) "")
    :text-change-callback (op (if (> (length _1) 0)
                                (awhen (hex-to-spec _1)
                                  (set-fg (coerce-color-to 24 it)))
                                (set-fg nil))))
   (24-bit-bg-input
    capi:text-input-pane
    :title "Bg:" :title-position :left
    :change-callback-type :data
    :text (if *bg* (spec-to-hex (term-color-spec *bg*)) "")
    :text-change-callback (op (if (> (length _1) 0)
                                (awhen (hex-to-spec _1)
                                  (set-bg (coerce-color-to 24 it)))
                                (set-bg nil))))
   (color-picker-prompt-1
    capi:title-pane :text "Left click to set foreground")
   (color-picker-prompt-2
    capi:title-pane :text "Right click to set Background")
   (restore-default-colors
    capi:push-button
    :text "Restore default colors"
    :callback-type :none
    :callback (op (set-fg nil) (set-bg nil)))
   (x-offset-input
    capi:text-input-pane
    :title "Offset:" :title-position :left
    :text "0"
    :visible-min-width '(character 4)
    :visible-max-width t
    :change-callback-type '(:data :element :interface)
    :text-change-callback (lambda (data self itf)
                            (let* ((str (ppcre:regex-replace-all "[^0-9\-]" data ""))
                                   (num (parse-integer str :junk-allowed t)))
                              (when num
                                (setf (capi:text-input-pane-text self) (princ-to-string num)
                                      @itf.board.x-offset num)
                                (refresh-board @itf.board)))))
   (y-offset-input
    capi:text-input-pane
    :title "x" :title-position :left
    :text "0"
    :visible-min-width '(character 4)
    :visible-max-width t
    :change-callback-type '(:data :element :interface)
    :text-change-callback (lambda (data self itf)
                            (let* ((str (ppcre:regex-replace-all "[^0-9\-]" data ""))
                                   (num (parse-integer str :junk-allowed t)))
                              (when num
                                (setf (capi:text-input-pane-text self) (princ-to-string num)
                                      @itf.board.y-offset num)
                                (refresh-board @itf.board))))))
  (:layouts
   (main-layout
    capi:row-layout
    '(left-column board right-column))
   (left-column
    capi:column-layout
    '(offset-row char-board)
    :adjust :right)
   (offset-row
    capi:row-layout
    '(x-offset-input y-offset-input))
   (right-column
    capi:column-layout
    '(colors-tab restore-default-colors tool-settings-container)
    ;:visible-min-width (* 3 (capi:screen-logical-resolution (capi:convert-to-screen)))
    :adjust :center)
   (colors-tab
    capi:tab-layout
    ()
    :items '(4-bit 8-bit 24-bit)
    :print-function #'string-downcase
    :visible-child-function #'identity)
   (4-bit
    capi:column-layout
    '(color-picker-prompt-1 color-picker-prompt-2 4-bit-grid)
    :adjust :center)
   (8-bit
    capi:column-layout
    '(8-bit-spectrum 8-bit-grayscale))
   (24-bit
    capi:column-layout
    '(24-bit-spectrum 24-bit-fg-input 24-bit-bg-input)
    :adjust :center)      
   (tool-settings-container capi:simple-layout '()))
  (:menus
   (file-menu
    "File"
    ((:component
      (("New" :name 'file-new :callback #'file-new)
       ("Open..." :name 'file-open :callback #'file-open)
       ("Save" :name 'file-save :callback #'file-save)
       ("Save As..." :name 'file-save-as :callback #'file-save-as)
       ("Export" :name 'file-export :callback #'file-export))
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
      ((:menu
        (("Right" :callback (op (change-cursor-movement @_.board :right))
          :accelerator #\-)
         ("Down" :callback (op (change-cursor-movement @_.board :down))
          :accelerator #\\))
        :title "Cursor Movement"
        :callback-type :interface))))
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
  (:menu-bar file-menu edit-menu tools-menu window-menu help-menu)
  (:default-initargs
   :title "Charapainter"
   :confirm-destroy-callback #'check-saved
   :destroy-callback (op (awhen @_.app-interface
                           (when (null (capi:collect-interfaces 'main-interface))
                             (capi:destroy it))))))

(defmethod capi:interface-keys-style ((self main-interface)) :emacs)

(defmethod initialize-instance :after ((self main-interface) &key)
  (setf @self.tools (mapcar (op (make _ :board @self.board)) *all-tools-names*))
  (setf (capi:interface-toolbar-items self)
        (list @self.current
              (make 'capi:toolbar-component
                    :items (mapcar (lambda (name)
                                     (make 'capi:toolbar-button
                                           :name name :data name :image name :remapped name
                                           :print-function #'string-capitalize))
                                   *all-tools-names*))
              @self.cursor-movement-option
              @self.copy-option
              @self.settings-button)
        (capi:interface-toolbar-state self :items)
        (append '(:flexible-space current :space)
                *all-tools-names*
                '(cursor-movement-option copy-option :space settings :flexible-space)))
  (set-tool self @self.board 'brush))

(defmethod set-tool-internal :after ((itf main-interface) board tool)
  (when (eq @itf.board.current-tool tool)
    (unless @tool.settings-layout
      (setf @tool.settings-layout (make-settings-layout itf tool)))
    (setf (capi:layout-description @itf.tool-settings-container) (list @tool.settings-layout))))

(defun refresh-font ()
  (dolist (itf (capi:collect-interfaces 'main-interface))
    (with-slots (board current char-board) itf
      (setf (capi:simple-pane-font board) *fdesc*
            (capi:simple-pane-font current) *fdesc*
            (capi:simple-pane-font char-board) *fdesc*)
      (capi:set-hint-table current (list :visible-min-width (* *char-width* 3)
                                         :visible-max-width t))
      (capi:set-hint-table char-board (list :visible-min-width *char-board-width*
                                            :visible-min-height *char-board-height*
                                            :visible-max-width t
                                            :visible-max-height t))
      (gp:invalidate-rectangle char-board))))

(capi:define-interface settings-interface ()
  ()
  (:panes
   (font-option
    capi:push-button
    :title (make-font-describe-string) :title-position :left
    :text "Choose Font"
    :callback-type :element
    :callback (lambda (self)
                (when-let (font (capi:prompt-for-font "Select a Font" :font *fdesc*))
                  (let ((desc (gp:font-description font)))
                    (setf *font-family* (gp:font-description-attribute-value desc :family)
                          *font-size* (gp:font-description-attribute-value desc :size)
                          (capi:titled-pane-title self) (make-font-describe-string))
                    (save-settings)
                    (set-variables)
                    (refresh-font)))))
   (cursor-option
    capi:option-pane
    :title "Cursor shape:" :title-position :left
    :items '(:inverse :underline :caret :left-bar :outline)
    :selected-item capi:*editor-cursor-active-style*
    :print-function #'string-capitalize
    :callback-type :data
    :selection-callback (op (setq capi:*editor-cursor-active-style* _)
                          (save-settings)))
   (selection-border-pane
    capi:output-pane
    :title "Selection border:"
    :title-position :left
    :visible-min-width (* *char-width* 3)
    :visible-max-width t
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y))
                        (capi:with-geometry pane
                          (let* ((font (gp:find-best-font pane *fdesc*))
                                 (ascent (gp:get-char-ascent pane *selection-border-char* font)))
                            (gp:draw-rectangle pane 0 0 (or capi:%width% w) (or capi:%height% h)
                                               :foreground *selection-border-background*
                                               :filled t)
                            (gp:draw-character pane *selection-border-char*
                                               *char-width* (+ (/ (- (or capi:%height% h) *char-height*) 2) ascent)
                                               :foreground *selection-border-foreground*
                                               :font font))))
    :input-model `(((:key :press) ,(lambda (pane x y key)
                                     (declare (ignore x y))
                                     (awhen (gesture-spec-char key)
                                       (setq *selection-border-char* it)
                                       (save-settings)
                                       (gp:invalidate-rectangle pane))))))
   (set-selection-fg
    capi:push-button
    :text "Set foreground"
    :callback-type :interface
    :callback (lambda (itf)
                (multiple-value-bind (r okp)
                    (capi:prompt-for-color
                     "Choose a color"
                     :color *selection-border-foreground*)
                  (when okp
                    (setq *selection-border-foreground* r)
                    (save-settings)
                    (gp:invalidate-rectangle @itf.selection-border-pane)))))
   (set-selection-bg
    capi:push-button
    :text "Set background"
    :callback-type :interface
    :callback (lambda (itf)
                (multiple-value-bind (r okp)
                    (capi:prompt-for-color
                     "Choose a color"
                     :color *selection-border-background*)
                  (when okp
                    (setq *selection-border-background* r)
                    (save-settings)
                    (gp:invalidate-rectangle @itf.selection-border-pane)))))
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
    :title "Note for default colors" :title-position :frame
    :visible-min-width '(+ :text-width 1)
    :text "These colors are only used inside the Charapainter editor and when you
export/copy to image.

When exporting to HTML or ANSI escaped sequences, the default
foreground & background color of your browser/terminal will be used.")
   (reset-default
    capi:push-button
    :text "Reset all settings to default"
    :callback-type :interface
    :callback (lambda (itf)
                (set-variables-default)
                (set-variables)
                (save-settings)
                (refresh-font)
                (gp:invalidate-rectangle @itf.selection-border-pane))))
  (:layouts
   (main-layout capi:column-layout '(font-option cursor-option selection-row default-fg default-bg note reset-default))
   (selection-row capi:row-layout '(selection-border-pane set-selection-fg set-selection-bg))))

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
    (set-variables))
  #-darwin
  (progn
    (set-variables)
    (capi:display (make 'main-interface))))

(export 'main)

;(set-variables)
;(capi:contain (make 'main-interface))

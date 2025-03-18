;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

;; IO functions

(defun write-pixels (pixels out)
  (write-char #\( out)
  (prin1 (list :left (pixels-left pixels) :right (pixels-right pixels)
               :top (pixels-top pixels) :bottom (pixels-bottom pixels))
         out)
  (loop-pixels pixels
    (prin1 (nconc
            (list %x %y (aif (pixel-char %pixel) (char-code it) 32) ;(char-code #\Space)
                  (when-let (fg (pixel-fg %pixel))
                    (list (term-color-bit fg)
                          (term-color-code fg)
                          (term-color-alpha fg)))
                  (when-let (bg (pixel-bg %pixel))
                    (list (term-color-bit bg)
                          (term-color-code bg)
                          (term-color-alpha bg))))
            (when (pixel-bold-p %pixel) (list :bold-p t))
            (when (pixel-italic-p %pixel) (list :italic-p t)) 
            (when (pixel-underline-p %pixel) (list :underline-p t)))
           out))
  (write-char #\) out))

(defun write-layer (layer out)
  (write-char #\( out)
  (prin1 (list :name (layer-name layer)
               :alpha (layer-alpha layer)
               :visible (layer-visible layer))
         out)
  (write-pixels (layer-pixels layer) out)
  (write-char #\) out))

(defun write-project (project out)
  (write-char #\( out)
  (prin1 (list :name (project-name project))
         out)
  (dolist (layer (project-layers project))
    (write-layer layer out))
  (write-char #\) out))

(defun save-project (project file)
  (with-open-file (out file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type 'base-char)
    (write-project project out))
  file)

(defun read-project (in)
  (let* ((*read-eval* nil)
         (data (read in))
         (project (apply #'make-project :layers nil (car data))))
    (dolist (layer-list (cdr data))
      (let* ((pixels-list (second layer-list))
             (pixels (apply #'make-pixels (car pixels-list))))
        (dolist (pixel-list (cdr pixels-list))
          (destructuring-bind (x y char fg bg &rest args) pixel-list
            (setf (gethash (cons x y) (pixels-table pixels))
                  (apply #'make-pixel
                         :char (code-char char)
                         :fg (when fg (apply #'code-color fg))
                         :bg (when bg (apply #'code-color bg))
                         args))))
        (push-end (apply #'make-layer :pixels pixels (first layer-list))
                  (project-layers project))))
    project))

(defun load-project (file)
  (with-open-file (in file)
    (handler-case
        (read-project in)
      (error (e)
        (capi:prompt-with-message "Error while loading project: Unsupported data format")))))

(defun check-saved (itf)
  (if @itf.board.saved t
    (multiple-value-bind (r okp)
        (capi:prompt-for-confirmation
         "You have unsaved changes. Save it?"
         :default-button :ok
         :cancel-button t)
      (when okp
        (if r (file-save itf) t)))))

;; File menu operations

(defun file-new (itf)
  (when (check-saved itf)
    (setf @itf.file nil)
    (set-project (make-project) itf)))

(defun file-open-internal (itf file)
  (setf @itf.file file)
  (set-project (load-project file) itf))

(defun file-open (itf)
  (when (check-saved itf)
    (multiple-value-bind (file okp)
        (capi:prompt-for-file
         "Select file to open"
         :operation :open
         :filter "*.charap"
         :filters '("Charapainter Project" "*.charap")
         :pathname (aif @itf.file
                        (pathname-location it)
                        (sys:get-folder-path :documents)))
      (when okp (file-open-internal itf file)))))

(defun file-save (itf)
  (if @itf.file
    (progn
      (save-project @itf.board.project @itf.file)
      (setf @itf.board.saved t))
    (file-save-as itf)))

(defun file-save-as (itf)
  (multiple-value-bind (new-file okp)
      (capi:prompt-for-file
       "Save as"
       :operation :save
       :filter "*.charap"
       :filters '("Charapainter Project" "*.charap")
       :pathname (merge-pathnames (string-append (if @itf.file (pathname-name @itf.file) "Unnamed") ".charap")
                                  (sys:get-folder-path :documents)))
    (when okp
      (save-project @itf.board.project new-file)
      (setf @itf.board.saved t
            @itf.file new-file))))

;; Export interface

(capi:define-interface export-prompt ()
  ((parent :initarg :parent)
   (format :initform :html)
   left top right bottom
   (color-depth :initform 24)
   image-width image-height
   (image-background :initform :transparent))
  (:panes
   (info capi:message-pane)
   (left-value
    capi:text-input-range
    :title "Left:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.left data)
                (let ((iw (ceiling (* @parent.board.char-width (- @itf.right @itf.left)))))
                  (setf @itf.image-width iw
                        (capi:text-input-range-value @itf.image-width-value) iw))))
   (right-value
    capi:text-input-range
    :title "Right:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.right data)
                (let ((iw (ceiling (* @parent.board.char-width (- @itf.right @itf.left)))))
                  (setf @itf.image-width iw
                        (capi:text-input-range-value @itf.image-width-value) iw))))
   (top-value
    capi:text-input-range
    :title "Top:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.top data)
                (let ((ih (ceiling (* @parent.board.char-width (- @itf.bottom @itf.top)))))
                  (setf @itf.image-height ih
                        (capi:text-input-range-value @itf.image-height-value) ih))))
   (bottom-value
    capi:text-input-range
    :title "Bottom:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.bottom data)
                (let ((ih (ceiling (* @parent.board.char-width (- @itf.bottom @itf.top)))))
                  (setf @itf.image-height ih
                        (capi:text-input-range-value @itf.image-height-value) ih))))
   (color-depth-choice
    capi:radio-button-panel
    :title "Color Depth:" :title-position :left
    :items '(4 8 24)
    :selected-item 24
    :print-function (op (format nil "~A-bit" _))
    :selection-callback (lambda (data itf)
                          (setf @itf.color-depth data)))
   (image-width-value
    capi:text-input-range
    :title "Image Width:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.image-width data)))
   (image-height-value
    capi:text-input-range
    :title "Image Height:" :title-position :left
    :start -99999 :end 99999
    :callback-type :data-interface
    :callback (lambda (data itf)
                (setf @itf.image-height data)))
   (image-background-choice
    capi:radio-button-panel
    :title "Default Background" :title-position :top :title-adjust :center
    :items '(:transparent :black :white :custom)
    :print-function #'string-capitalize
    :selection-callback (lambda (data itf)
                          (case data
                            (:custom (awhen (capi:prompt-for-color
                                             "Choose a color"
                                             :color (color:get-color-translation @itf.image-background))
                                       (setf @itf.image-background it)))
                            (t (setf @itf.image-background data))))))
  (:layouts
   (main-layout
    capi:column-layout
    '(info size-grid color-depth-choice :separator export-format)
    :gap 5
    :adjust :center)
   (size-grid
    capi:grid-layout
    '(nil top-value nil
      left-value nil right-value
      nil bottom-value nil)
    :columns 3
    :x-adjust :center :y-adjust :center
    :x-uniform-size-p t)
   (image-size-row
    capi:row-layout
    '(image-width-value image-height-value)
    :gap 5
    :uniform-size-p t)
   (tab-column
    capi:column-layout
    (list (make 'capi:message-pane
                :text "Export a HTML file with image encoded in a <pre> element"))
    :adjust :center
    :internal-border 5)
   (export-format
    capi:tab-layout
    '(tab-column)
    :title "Export Format" :title-position :top :title-adjust :center
    :items '("HTML" "ANSI" "Plain Text" "Image")
    :selection-callback
    (lambda (data itf)
      (string-case data
        ("HTML"
         (setf @itf.format :html
               (capi:layout-description @itf.tab-column)
               (list (make 'capi:message-pane
                           :text "Export a HTML file with image encoded in a <pre> element"))))
        ("ANSI"
         (setf @itf.format :ansi
               (capi:layout-description @itf.tab-column)
               (list (make 'capi:message-pane
                           :text "Encoding image with ANSI escape sequences which can be shown in terminal"))))
        ("Plain Text"
         (setf @itf.format :text
               (capi:layout-description @itf.tab-column)
               (list (make 'capi:message-pane
                           :text "Export a TXT file. Colors and styles are not contained."))))
        ("Image"
         (setf @itf.format :image
               (capi:layout-description @itf.tab-column)
               (list @itf.image-size-row :separator @itf.image-background-choice))))))))

(defmethod initialize-instance :after ((self export-prompt) &key)
  (let* ((itf @self.parent)
         (board @itf.board)
         (project @board.project)
         (layers (project-layers project)))
    (with-layers-boundary layers
      (setf (capi:title-pane-text @self.info)
            (format nil "Project boundary: Left: ~A; Right: ~A; Top: ~A; Bottom: ~A" %left %right %top %bottom)
            
            @self.left %left
            (capi:text-input-range-value @self.left-value) %left
            @self.right %right
            (capi:text-input-range-value @self.right-value) %right
            @self.top %top
            (capi:text-input-range-value @self.top-value) %top
            @self.bottom %bottom
            (capi:text-input-range-value @self.bottom-value) %bottom)
      
      (let ((iw (ceiling (* @itf.board.char-width (- %right %left))))
            (ih (ceiling (* @itf.board.char-height (- %bottom %top)))))
        (setf @self.image-width iw
              (capi:text-input-range-value @self.image-width-value) iw
              @self.image-height ih
              (capi:text-input-range-value @self.image-height-value) ih)))))

(defun file-export (itf)
  (multiple-value-bind (result okp)
      (capi:popup-confirmer
       (make 'export-prompt :parent itf)
       "Export")
    (when okp
      (let ((ext (case @result.format
                   (:html "html") (:ansi "ans") (:text "txt") (:image "png")))
            (filter (case @result.format
                      (:html "*.html")
                      (:ansi "*.ans")
                      (:text "*.txt")
                      (:image "*.png;*.jpg;*.jpeg;*.bmp;*.tiff"))))
        (multiple-value-bind (file okp)
            (capi:prompt-for-file
             "Export to"
             :operation :save
             :pathname (make-pathname :name (project-name @itf.board.project) :type ext
                                      :defaults (sys:get-folder-path :documents))
             :filter filter)
          (when okp
            (handler-case
                (let* ((layers (project-layers @itf.board.project))
                       (pixels (composed-pixels
                                layers
                                @result.left @result.top @result.right @result.bottom
                                @result.color-depth)))
                  (if (member @result.format '(:html :ansi :text))
                    (with-open-file (out file
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (case @result.format
                        (:html (export-to-html pixels out @result.left @result.top @result.right @result.bottom))
                        (:ansi (export-to-ansi pixels out @result.left @result.top @result.right @result.bottom))
                        (:text (export-to-plain-text pixels out @result.left @result.top @result.right @result.bottom))))
                    (gp:externalize-and-write-image
                     @itf.board
                     (export-to-image @itf.board pixels
                                      @result.left @result.top @result.right @result.bottom
                                      @result.image-background @result.image-width @result.image-height)
                     file)))
              (error (e) (dbg:output-backtrace :verbose)
                (capi:prompt-with-message (format nil "Export failed: ~A" e))))))))))

;; Export functions

(defun merge-properties (properties)
  (let ((prop-start 0)
        (prop-end 0)
        prop)
    (with-collector (c)
      (dolist (props properties)
        (destructuring-bind (ps pe p) props
          (cond ((null prop)
                 (setq prop-start ps
                       prop-end pe
                       prop p))
                ((and (= prop-end ps)
                      (equalp prop p))
                 (setq prop-end pe))
                (t (c (list prop-start prop-end prop))
                   (setq prop-start ps
                         prop-end pe
                         prop p)))))
      (c (list prop-start prop-end prop)))))

(defun pixels-string-and-properties (pixels left top right bottom)
  (let* ((w (+ (- right left 1) 2))
         (h (1+ (- bottom top 1)))
         (str (make-string (* w h) :initial-element #\Space))
         (props (loop for (x0 . y0) being each hash-key of (pixels-table pixels)
                        using (hash-value pixel)
                      for x = (- x0 left)
                      for y = (- y0 top)
                      for subs = (+ (* y w) x)
                      do (setf (char str subs) (pixel-char pixel))
                      collect (list subs (1+ subs) pixel))))
    (dorange (i w (* w h) w)
      (setf (char str (1- i)) #\Newline))
    (values str (merge-properties
                 (sort props (op (< (car _) (car _))))))))

(defun term-print-fg (out fg)
  (declare (inline term-print-fg))
  (case (term-color-bit fg)
    (4 (format out "~@{~A~}" #\Escape #\[ (term-color-code fg) #\m))
    (8 (format out "~@{~A~}" #\Escape #\[ 38 #\; 5 #\; (term-color-code fg) #\m))
    (24 (destructuring-bind (r g b) (term-color-code fg)
          (format out "~@{~A~}" #\Escape #\[ 38 #\; 2 #\; r #\; g #\; b #\m)))))

(defun term-print-bg (out bg)
  (declare (inline term-print-bg))
  (case (term-color-bit bg)
    (4 (format out "~@{~A~}" #\Escape #\[ (+ (term-color-code bg) 10) #\m))
    (8 (format out "~@{~A~}" #\Escape #\[ 48 #\; 5 #\; (term-color-code bg) #\m))
    (24 (destructuring-bind (r g b) (term-color-code bg)
          (format out "~@{~A~}" #\Escape #\[ 48 #\; 2 #\; r #\; g #\; b #\m)))))

(defun term-print-reset (out)
  (format out "~@{~A~}" #\Escape #\[ 0 #\m))

(defun export-to-plain-text (pixels &optional (out *standard-output*)
                                    (left (pixels-left pixels)) (top (pixels-top pixels))
                                    (right (pixels-right pixels)) (bottom (pixels-bottom pixels)))
  (write-string (pixels-string-and-properties pixels left top right bottom) out))

(defun export-to-ansi (pixels &optional (out *standard-output*)
                              (left (pixels-left pixels)) (top (pixels-top pixels))
                              (right (pixels-right pixels)) (bottom (pixels-bottom pixels)))
  (multiple-value-bind (str props)
      (pixels-string-and-properties pixels left top right bottom)
    (loop for last-end = 0 then end
          for (start end pixel) in props
          do (when (< last-end start)
               (term-print-reset out)
               (write-string str out :start last-end :end start))
             (awhen (pixel-fg pixel) (term-print-fg out it))
             (awhen (pixel-bg pixel) (term-print-bg out it))
             (when (pixel-bold-p pixel)
               (format out "~@{~A~}" #\Escape #\[ 1 #\m))
             (when (pixel-italic-p pixel)
               (format out "~@{~A~}" #\Escape #\[ 3 #\m))
             (when (pixel-underline-p pixel)
               (format out "~@{~A~}" #\Escape #\[ 4 #\m))
             (write-string str out :start start :end end))
    (term-print-reset out)
    (terpri out)))

(defun export-to-html (pixels &optional (out *standard-output*)
                              (left (pixels-left pixels)) (top (pixels-top pixels))
                              (right (pixels-right pixels)) (bottom (pixels-bottom pixels)))
  (multiple-value-bind (str props)
      (pixels-string-and-properties pixels left top right bottom)
    (write-string "<pre>" out)
    (flet ((escape (str)
             (if (plusp (length str))
               (string-append*
                (loop for c across str
                      collect (if (alphanumericp c) (string c)
                                (format nil "&#~A" (char-code c)))))
               "")))
      (loop for last-end = 0 then end
            for (start end pixel) in props
            do (write-string (escape (subseq str last-end start)) out)
               (format out "<span style='")
               (awhen (pixel-fg pixel)
                 (format out "color:#~A;" (spec-to-hex (term-color-spec it))))
               (awhen (pixel-bg pixel)
                 (format out "background-color:#~A;" (spec-to-hex (term-color-spec it))))
               (when (pixel-bold-p pixel)
                 (format out "font-weight:bold;"))
               (when (pixel-italic-p pixel)
                 (format out "font-style:italic;"))
               (when (pixel-underline-p pixel)
                 (format out "text-decoration:underline;"))
               (format out "'>~A</span>" (escape (subseq str start end)))))
    (write-string "</pre>" out)))

(defun export-to-image (board pixels &optional
                              (left (pixels-left pixels)) (top (pixels-top pixels))
                              (right (pixels-right pixels)) (bottom (pixels-bottom pixels))
                              (background :transparent) width height)
  (with-pixels-boundary pixels
    (let ((w (* @board.char-width (- %right %left)))
          (h (* @board.char-height (- %bottom %top))))
      (unless width (setq width w height h))
      (setq width (ceiling width) height (ceiling height))
      (gp:with-pixmap-graphics-port (port board width height
                                          :background background
                                          :clear t)
        (gp:with-graphics-scale (port (/ width w) (/ height h))
          (loop-pixels pixels
            (when (and (<= left %x right)
                       (<= top %y bottom))
              (let* ((x (* @board.char-width (- %x %left)))
                     (y (* @board.char-height (- %y %top)))
                     (font (gp:find-best-font
                            board
                            (gp:make-font-description :family @board.project.font-family
                                                      :size @board.project.font-size
                                                      :weight (if (pixel-bold-p %pixel) :bold :normal)
                                                      :slant (if (pixel-italic-p %pixel) :italic :roman))))
                     (ascent (gp:get-font-ascent port font))
                     (width (gp:get-char-width port (pixel-char %pixel) font))
                     (height (gp:get-font-height port font)))
                (gp:draw-character port (pixel-char %pixel) x (+ y ascent)
                                   :font font
                                   :foreground (aif (pixel-fg %pixel) (term-color-spec it) *default-foreground*)
                                   :background (aif (pixel-bg %pixel) (term-color-spec it) *default-background*)
                                   :block t)
                (when (pixel-underline-p %pixel)
                  (gp:draw-line port x (+ y height) (+ x width) (+ y height)
                                :foreground (aif (pixel-fg %pixel) (term-color-spec it) *default-foreground*)
                                :thickness 1)))))
          (gp:make-image-from-port port))))))

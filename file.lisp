;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defun write-pixels (pixels out)
  (prin1 (list (pixels-left pixels) (pixels-right pixels)
               (pixels-top pixels) (pixels-bottom pixels))
         out)
  (loop-pixels pixels
    (prin1 (list %x %y (char-code (pixel-char %pixel))
                 (when-let (fg (pixel-fg %pixel))
                   (list (term-color-bit fg)
                         (term-color-code fg)))
                 (when-let (bg (pixel-bg %pixel))
                   (list (term-color-bit bg)
                         (term-color-code bg))))
           out)))

(defun read-pixels (in)
  (let ((*read-eval* nil)
        (pixels (make-pixels)))
    (destructuring-bind (l r top b) (read in)
      (setf (pixels-left pixels) l
            (pixels-right pixels) r
            (pixels-top pixels) top
            (pixels-bottom pixels) b))
    (iter (for lst :next (read in nil))
          (until (null lst))
          (destructuring-bind (x y char fg bg) lst
            (setf (gethash (cons x y) (pixels-table pixels))
                  (make-pixel :char (code-char char)
                              :fg (when fg (apply #'code-color fg))
                              :bg (when bg (apply #'code-color bg))))))
    pixels))

(defun save-project (project file)
  (with-open-file (out file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type 'base-char)
    (write-pixels (project-pixels project) out))
  file)

(defun load-project (file)
  (with-open-file (in file)
    (make-project :pixels (read-pixels in))))

(defun check-saved (itf)
  (if @itf.board.saved t
    (multiple-value-bind (r okp)
        (capi:prompt-for-confirmation
         "You have unsaved changes. Save it?"
         :default-button :ok
         :cancel-button t)
      (when okp
        (if r (file-save itf) t)))))

(defun file-new (itf)
  (when (check-saved itf)
    (setf @itf.file nil
          @itf.board.saved t
          @itf.board.project (make-project))
    (refresh-board @itf.board)))

(defun file-open-internal (itf file)
  (setf @itf.file file
        (slot-value @itf.board 'saved) t
        (board-project @itf.board) (load-project file))
  (refresh-board @itf.board))

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
  (with-slots (board file) itf
    (with-slots (project) board
      (if file
        (progn
          (save-project project file)
          (setf (slot-value board 'saved) t))
        (file-save-as itf)))))

(defun file-save-as (itf)
  (with-slots (board file) itf
    (with-slots (project) board
      (multiple-value-bind (new-file okp)
          (capi:prompt-for-file
           "Save as"
           :operation :save
           :filter "*.charap"
           :filters '("Charapainter Project" "*.charap")
           :pathname (merge-pathnames (string-append (if file (pathname-name file) "Unnamed") ".charap")
                                      (sys:get-folder-path :documents)))
        (when okp
          (save-project project new-file)
          (setf (slot-value board 'saved) t
                file new-file))))))

;; Export

(capi:define-interface export-image-prompt ()
  ((bg :initform :transparent)
   (default-width :initarg :default-width)
   (default-height :initarg :default-height))
  (:panes
   (width
    capi:text-input-pane
    :title "Width:" :title-position :left
    :change-callback-type '(:data :element)
    :text-change-callback (lambda (data self)
                            (setf (capi:text-input-pane-text self)
                                  (ppcre:regex-replace-all "[^0-9]" data ""))))
   (height
    capi:text-input-pane
    :title "Height:" :title-position :left
    :change-callback-type '(:data :element)
    :text-change-callback (lambda (data self)
                            (setf (capi:text-input-pane-text self)
                                  (ppcre:regex-replace-all "[^0-9]" data ""))))
   (background
    capi:option-pane
    :items '(:transparent :black :white :custom)
    :selection-callback (lambda (data itf)
                          (case data
                            (:custom (awhen (capi:prompt-for-color
                                             "Choose a color"
                                             :color (color:get-color-translation @itf.bg))
                                       (setf @itf.bg it)))
                            (t (setf @itf.bg data)))))))

(defmethod initialize-instance :after ((itf export-image-prompt) &key)
  (setf (capi:text-input-pane-text @itf.width) (princ-to-string @itf.default-width)
        (capi:text-input-pane-text @itf.height) (princ-to-string @itf.default-height)))

(defun file-export (itf)  
  (let ((pixels @itf.board.project.pixels))
    (multiple-value-bind (file okp)
        (capi:prompt-for-file
         "Export to"
         :operation :save
         :pathname (merge-pathnames (string-append @itf.board.project.name ".html")
                                    (sys:get-folder-path :documents))
         :filter "*.html"
         :filters '("HTML" "*.html"
                    "ANSI" "*.ans"
                    "Plain Text" "*.txt"
                    "Image" "*.png;*.jpg;*.jpeg;*.bmp;*.tiff"))
      (when okp
        (handler-case
            (if (member (pathname-type file) '("html" "ans" "txt"))
              (with-open-file (out file
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                (string-case (pathname-type file)
                  ("html" (export-to-html pixels out))
                  ("ans"  (export-to-ansi pixels out))
                  ("txt"  (export-to-plain-text pixels out))))
              (with-pixels-boundary pixels
                (multiple-value-bind (r okp)
                    (capi:popup-confirmer
                     (make 'export-image-prompt)
                     "Image settings"
                     :value-function (lambda (r)
                                       (let ((width-str (capi:text-input-pane-text @r.width))
                                             (height-str (capi:text-input-pane-text @r.height)))
                                         (list @r.bg
                                               (when (> (length width-str) 0) (parse-integer width-str))
                                               (when (> (length height-str) 0) (parse-integer height-str)))))
                     :ok-check (op (apply #'and _))
                     :default-width (* *char-width* (- %right %left))
                     :default-height (* *char-height* (- %bottom %top)))
                  (when okp
                    (gp:externalize-and-write-image
                     @itf.board
                     (apply #'export-to-image @itf.board pixels r)
                     file)))))
          (error (e) (capi:prompt-with-message (format nil "Export failed: ~A" e))))))))

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

(defun pixels-string-and-properties (pixels)
  (with-pixels-boundary pixels
    (let* ((w (+ (- %right %left 1) 2))
           (h (1+ (- %bottom %top 1)))
           (str (make-string (* w h) :initial-element #\Space)))
      (iter (for ((x0 . y0) pixel) :in-hashtable (pixels-table pixels))
            (let* ((x (- x0 %left))
                   (y (- y0 %top))
                   (subs (+ (* y w) x)))
              (setf (char str subs) (pixel-char pixel))
              (collect (list subs (1+ subs) pixel) into props))
            (finally
             (dorange (i w (* w h) w)
               (setf (char str (1- i)) #\Newline))
             (return-from pixels-string-and-properties
               (values str (merge-properties
                            (sort props (op (< (car _) (car _))))))))))))

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

(defun export-to-plain-text (pixels &optional (out *standard-output*))
  (write-string (pixels-string-and-properties pixels) out))

(defun export-to-ansi (pixels &optional (out *standard-output*))
  (multiple-value-bind (str props)
      (pixels-string-and-properties pixels)
    (iter (for last-end :previous end :initially 0)
          (for (start end pixel) :in props)
          (when (< last-end start)
            (term-print-reset out)
            (write-string str out :start last-end :end start))
          (awhen (pixel-fg pixel) (term-print-fg out it))
          (awhen (pixel-bg pixel) (term-print-bg out it))
          (write-string str out :start start :end end))
    (term-print-reset out)
    (terpri out)))

(defun export-to-html (pixels &optional (out *standard-output*))
  (multiple-value-bind (str props)
      (pixels-string-and-properties pixels)
    (write-string "<pre>" out)
    (flet ((escape (str)
             (if (plusp (length str))
               (apply #'string-append
                      (iter (for c :in-string str)
                            (collect (if (alphanumericp c) (string c)
                                       (format nil "&#~A" (char-code c))))))
               "")))
      (iter (for last-end :previous end :initially 0)
            (for (start end pixel) :in props)
            (write-string (escape (subseq str last-end start)) out)
            (format out "<span style='")
            (awhen (pixel-fg pixel)
              (format out "color:#~A;" (spec-to-hex (term-color-spec it))))
            (awhen (pixel-bg pixel)
              (format out "background-color:#~A;" (spec-to-hex (term-color-spec it))))
            (format out "'>~A</span>" (escape (subseq str start end)))))
    (write-string "</pre>" out)))

(defun export-to-image (board pixels &optional (background :transparent) width height)
  (with-pixels-boundary pixels
    (let ((w (* *char-width* (- %right %left)))
          (h (* *char-height* (- %bottom %top)))
          (font (gp:find-best-font board *fdesc*)))
      (unless width (setq width w height h))
      (setq width (ceiling width) height (ceiling height))
      (gp:with-pixmap-graphics-port (port board width height
                                          :background background
                                          :clear t)
        (gp:with-graphics-scale (port (/ width w) (/ height h))
          (loop-pixels pixels
            (let ((x (* *char-width* (- %x %left)))
                  (y (* *char-height* (- %y %top)))
                  (ascent (gp:get-char-ascent port (pixel-char %pixel) font)))
              (gp:draw-character port (pixel-char %pixel) x (+ y ascent)
                                 :font font
                                 :foreground (aif (pixel-fg %pixel) (term-color-spec it) *default-foreground*)
                                 :background (aif (pixel-bg %pixel) (term-color-spec it) *default-background*)
                                 :block t)))
          (gp:make-image-from-port port))))))

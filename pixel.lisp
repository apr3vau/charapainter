(in-package charapainter)

(proclaim *optimization*)

(defstruct pixel char fg bg bold-p italic-p underline-p)

#|(defun copy-pixel (pixel)
  (let ((new (%copy-pixel pixel)))
    (when (pixel-fg pixel)
      (setf (pixel-fg new) (copy-term-color (pixel-fg pixel))))
    (when (pixel-bg pixel)
      (setf (pixel-bg new) (copy-term-color (pixel-bg pixel))))
    new))|#

(defun pixel-not-empty-p (pixel)
  (and pixel
       (or (pixel-fg pixel)
           (pixel-bg pixel)
           (pixel-bold-p pixel)
           (pixel-italic-p pixel)
           (pixel-underline-p pixel)
           (not (member (pixel-char pixel) '(nil #\Space))))))

(defun pixel-face (pixel)
  (make-face nil
             :foreground (awhen (pixel-fg pixel) (term-color-spec it))
             :background (awhen (pixel-bg pixel) (term-color-spec it))
             :bold-p (pixel-bold-p pixel)
             :italic-p (pixel-italic-p pixel)
             :underline-p (pixel-underline-p pixel)))

(defstruct (pixels (:copier %copy-pixels))
  (table (make-hash-table :test #'equal)) ;; a hash table of (x . y) => pixel
  (left 0 :type fixnum)
  (right 0 :type fixnum)
  (top 0 :type fixnum)
  (bottom 0 :type fixnum))

(defun copy-pixels (pixels)
  (declare (inline copy-pixels))
  (let* ((old-table (pixels-table pixels))
         (new (%copy-pixels pixels))
         (new-table (make-hash-table :test #'equal :size (hash-table-size old-table))))
    (maphash (lambda (key pixel)
               (setf (gethash key new-table) (when (pixel-p pixel) (copy-pixel pixel))))
             old-table)
    (setf (pixels-table new) new-table)
    new))

(defun add-pixel (x y pixel pixels)
  (declare (type fixnum x y) (optimize (speed 3) (safety 0)))
  (if (= (hash-table-count (pixels-table pixels)) 0)
      (setf (pixels-left pixels) x
            (pixels-right pixels) (1+ x)
            (pixels-top pixels) y
            (pixels-bottom pixels) (1+ y))
    (progn
      (cond ((< x (pixels-left pixels))
             (setf (pixels-left pixels) x))
            ((>= x (pixels-right pixels))
             (setf (pixels-right pixels) (1+ x))))
      (cond ((< y (pixels-top pixels))
             (setf (pixels-top pixels) y))
            ((>= y (pixels-bottom pixels))
             (setf (pixels-bottom pixels) (1+ y))))))
  (setf (gethash (cons x y) (pixels-table pixels)) pixel))

(defun delete-pixel (x y pixels)
  (declare (inline delete-pixel))
  (remhash (cons x y) (pixels-table pixels)))

(defun delete-pixel-if (func pixels)
  (declare (inline delete-pixel-if))
  (let ((table (pixels-table pixels)))
    (maphash (lambda (key val)
               (when (funcall func (car key) (cdr key) val)
                 (remhash key table)))
             table)))

(defun find-pixel (x y pixels)
  (declare (inline find-pixel))
  (multiple-value-bind (pixel found)
      (gethash (cons x y) (pixels-table pixels))
    (values (awhen pixel (copy-pixel it)) found)))

(defun nfind-pixel (x y pixels)
  (declare (inline find-pixel))
  (multiple-value-bind (pixel found)
      (gethash (cons x y) (pixels-table pixels))
    (values (awhen pixel it) found)))

(defmacro loop-pixels (pixels &body body)
  (let ((cons (gensym)))
    `(do-hash-table (,cons %pixel (pixels-table ,pixels))
       (declare (ignorable %pixel))
       (destructuring-bind (%x . %y) ,cons
         (declare (type fixnum %x %y) (ignorable %x %y))
         ,@body))))

(defun nunion-pixels (&rest all-pixels)
  (dolist (pixels (cdr all-pixels))
    (loop-pixels pixels
      (add-pixel %x %y %pixel (car all-pixels))))
  (car all-pixels))

(defun union-pixels (&rest all-pixels)
  (let ((new-pixels (copy-pixels (car all-pixels)))
        (new-table (alexandria:copy-hash-table (pixels-table (car all-pixels)))))
    (setf (pixels-table new-pixels) new-table)
    (dolist (pixels (cdr all-pixels))
      (loop-pixels pixels
        (add-pixel %x %y %pixel new-pixels)))
    new-pixels))

(defun pixels-not-empty-p (pixels)
  (declare (inline pixels-not-empty-p))
  (> (hash-table-count (pixels-table pixels)) 0))

(defun pixels-line-p (pixels)
  (let (x y fixed)
    (null
     (loop-pixels pixels
       (if x
         (if fixed
           (if (eql fixed :x)
             (if (eql %x x)
               nil
               (return t))
             (if (eql %y y)
               nil
               (return t)))
           (if (and (/= %x x) (/= %y y))
             (return t)
             (setq fixed (if (= %x x) :x :y))))
         (setq x %x y %y))))))

(defmacro with-pixels-boundary (pixels &body body)
  `(let ((%left (pixels-left ,pixels))
         (%top (pixels-top ,pixels))
         (%right (pixels-right ,pixels))
         (%bottom (pixels-bottom ,pixels)))
     (declare (ignorable %left %right %top %bottom))
     ,@body))

(defun line-pixels (pixel x1 y1 x2 y2 &optional (thickness 1))
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum x1 y1 x2 y2))
  (let ((pixels (make-pixels)))
    (cond ((= x1 x2)
           (dorange$fixnum (y (min y1 y2) (1+ (max y1 y2)))
             (dorange$fixnum (x (- x1 (floor (/ thickness 2))) (+ x1 (floor (/ thickness 2)) 1))
               (add-pixel x y pixel pixels))))
          ((= y1 y2)
           (dorange$fixnum (x (min x1 x2) (1+ (max x1 x2)))
             (dorange$fixnum (y (- y1 (floor (/ thickness 2))) (+ y1 (floor (/ thickness 2)) 1))
               (add-pixel x y pixel pixels))))
          (t (let* ((k (/ (- y2 y1) (- x2 x1)))
                    (b (- y1 (* k x1))))
               (dorange$fixnum (x (min x1 x2) (1+ (max x1 x2)))
                 (let ((y (round (+ (* k x) b))))
                   (dorange$fixnum (y0 (- y (floor (/ thickness 2))) (+ y (floor (/ thickness 2)) 1))
                     (add-pixel x y0 pixel pixels))))
               (dorange$fixnum (y (min y1 y2) (1+ (max y1 y2)))
                 (let ((x (round (/ (- y b) k))))
                   (dorange$fixnum (x0 (- x (floor (/ thickness 2))) (+ x (floor (/ thickness 2)) 1))
                     (add-pixel x0 y pixel pixels)))))))
    pixels))

(defun join-pixels (charset pixels)
  (loop-pixels pixels
    (when %pixel
      (let (connections)
        (when (aand (nfind-pixel (1- %x) %y pixels)
                    (charset-member (pixel-char it) charset))
          (push :left connections))
        (when (aand (nfind-pixel (1+ %x) %y pixels)
                    (charset-member (pixel-char it) charset))
          (push :right connections))
        (when (aand (nfind-pixel %x (1- %y) pixels)
                    (charset-member (pixel-char it) charset))
          (push :top connections))
        (when (aand (nfind-pixel %x (1+ %y) pixels)
                    (charset-member (pixel-char it) charset))
          (push :bottom connections))
        (setf (pixel-char %pixel)
              (charset-get
               charset
               (cond ((null connections) :h)
                     ((= (length connections) 1)
                      (cond ((or (find :left connections) (find :right connections)) :h)
                            (t :v)))
                     ((= (length connections) 2)
                      (cond ((find :left connections)
                             (cond ((find :top connections) :lt)
                                   ((find :bottom connections) :lb)
                                   (t :h)))
                            ((find :right connections)
                             (cond ((find :top connections) :rt)
                                   (t :rb)))
                            (t :v)))
                     ((= (length connections) 3)
                      (cond ((not (find :left connections)) :tbr)
                            ((not (find :right connections)) :tbl)
                            ((not (find :top connections)) :lrb)
                            ((not (find :bottom connections)) :lrt)))
                     (t :cross)))))))
  pixels)

;; Interpolation

(defmacro with-image-transformed-boundary (image transform &body body)
  (with-unique-names (x y src-w src-h)
    `(let ((,src-w (gp:image-width ,image))
           (,src-h (gp:image-height ,image)))
       (loop for (,x ,y) on (gp:transform-points ,transform (list 0 0 0 ,src-h ,src-w 0 ,src-w ,src-h)) by #'cddr
             minimize (floor ,x) into %left
             maximize (ceiling ,x) into %right
             minimize (floor ,y) into %top
             maximize (ceiling ,y) into %bottom
             finally (return (locally (declare (type fixnum %left %right %top %bottom))
                               ,@body))))))

(defun 1-by-1-interpolation (board image w h theta &optional (bit 24))
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum w h))
  (let ((src-w (gp:image-width image))
        (src-h (gp:image-height image))
        (transform (gp:make-transform))
        (acc (gp:make-image-access board image))
        (pixels (make-pixels)))
    (declare (type fixnum src-w src-h))
    (gp:apply-scale transform (/ w src-w) (/ (* h 2) src-h))
    (gp:apply-rotation-around-point transform theta (/ src-w 2) (/ src-h 2))
    (with-image-transformed-boundary image transform
      (gp:apply-translation transform (- (min %left %right)) (- (min %top %bottom))))
    (with-image-transformed-boundary image transform
      (dorange$fixnum (y %top (ceiling %bottom 2))
        (dorange$fixnum (x %left %right)
          (add-pixel x y (make-pixel :char #\▄ :fg (aref *4-bit-colors* 0) :bg (aref *4-bit-colors* 0)) pixels)))
      (dorange$fixnum (y %top %bottom)
        (dorange$fixnum (x %left %right)
          (with ((x0 y0 (gp:untransform-point transform x y))
                 (x-base x-ratio (floor x0))
                 (y-base y-ratio (floor y0)))
            (when (and (<= 0 x-base (1- src-w))
                       (<= 0 y-base (1- src-h)))
              (with ((color-of (x y)
                       (if (or (>= x src-w) (>= y src-h))
                         (color:make-rgb 1 1 1 0)
                         (color:unconvert-color board (gp:image-access-pixel acc x y))))
                     (lt (color-of x-base y-base))
                     (rt (color-of (1+ x-base) y-base))
                     (lb (color-of x-base (1+ y-base)))
                     (rb (color-of (1+ x-base) (1+ y-base)))
                     (bilinear (func)
                       (lerp x-ratio
                             (lerp y-ratio (funcall func lt) (funcall func lb))
                             (lerp y-ratio (funcall func rt) (funcall func rb))))
                     (color (color:make-rgb
                             (bilinear #'color:color-red) (bilinear #'color:color-green) (bilinear #'color:color-blue)
                             (bilinear #'color:color-alpha)))
                     (pixel (nfind-pixel x (floor y 2) pixels)))
                (when (> (color:color-alpha color) 0)
                  (if (evenp y)
                    (setf (pixel-bg pixel) (coerce-color-to bit color))
                    (setf (pixel-fg pixel) (coerce-color-to bit color))))))))))
    (gp:free-image-access acc)
    pixels))

(defun 1-by-2-interpolation (board image w h theta &optional (bit 24))
  (declare (type fixnum w h))
  (let ((src-w (gp:image-width image))
        (src-h (gp:image-height image))
        (transform (gp:make-transform))
        (acc (gp:make-image-access board image))
        (pixels (make-pixels)))
    (declare (type fixnum src-w src-h))
    (gp:apply-scale transform (/ w src-w) (/ h src-h))
    (gp:apply-rotation-around-point transform theta (/ src-w 2) (/ src-h 2))
    (with-image-transformed-boundary image transform
      (gp:apply-translation transform (- (min %left %right)) (- (min %top %bottom))))
    (with-image-transformed-boundary image transform
      (dorange$fixnum (y %top %bottom)
        (dorange$fixnum (x %left %right)
          (with ((x0 y0 (gp:untransform-point transform x y))
                 (x-base x-ratio (floor x0))
                 (y-base y-ratio (floor y0)))
            (when (and (<= 0 x-base (1- src-w))
                       (<= 0 y-base (1- src-h)))
              (with ((color-of (x y)
                       (if (or (>= x src-w) (>= y src-h))
                         (color:make-rgb 1 1 1 0)
                         (color:unconvert-color board (gp:image-access-pixel acc x y))))
                     (lt (color-of x-base y-base))
                     (rt (color-of (1+ x-base) y-base))
                     (lb (color-of x-base (1+ y-base)))
                     (rb (color-of (1+ x-base) (1+ y-base)))
                     (bilinear (func)
                       (lerp x-ratio
                             (lerp y-ratio (funcall func lt) (funcall func lb))
                             (lerp y-ratio (funcall func rt) (funcall func rb))))
                     (color (color:make-rgb
                             (bilinear #'color:color-red) (bilinear #'color:color-green) (bilinear #'color:color-blue)
                             (bilinear #'color:color-alpha))))
                (add-pixel x y (make-pixel :char #\Space :bg (coerce-color-to bit color)) pixels)))))))
    (gp:free-image-access acc)
    pixels))

(defun gray-scale-interpolation (board image w h theta &optional (charset " .:-=+*%#@"))
  (declare (type fixnum w h))
  (let ((src-w (gp:image-width image))
        (src-h (gp:image-height image))
        (transform (gp:make-transform))
        (acc (gp:make-image-access board image))
        (pixels (make-pixels))
        (step (/ 1 (1- (length charset)))))
    (declare (type fixnum src-w src-h))
    (gp:apply-scale transform (/ w src-w) (/ h src-h))
    (gp:apply-rotation-around-point transform theta (/ src-w 2) (/ src-h 2))
    (with-image-transformed-boundary image transform
      (gp:apply-translation transform (- (min %left %right)) (- (min %top %bottom))))
    (with-image-transformed-boundary image transform
      (dorange$fixnum (y %top %bottom)
        (dorange$fixnum (x %left %right)
          (with ((x0 y0 (gp:untransform-point transform x y))
                 (y-base y-ratio (floor y0))
                 (x-base x-ratio (floor x0)))
            (when (and (<= 0 x-base (1- src-w))
                       (<= 0 y-base (1- src-h)))
              (with ((color-of (x y)
                       (if (or (>= x src-w) (>= y src-h))
                         (color:make-rgb 1 1 1 0)
                         (color:ensure-gray (color:unconvert-color board (gp:image-access-pixel acc x y)))))
                     (lt (color-of x-base y-base))
                     (rt (color-of (1+ x-base) y-base))
                     (lb (color-of x-base (1+ y-base)))
                     (rb (color-of (1+ x-base) (1+ y-base)))
                     (bilinear (func)
                       (lerp x-ratio
                             (lerp y-ratio (funcall func lt) (funcall func lb))
                             (lerp y-ratio (funcall func rt) (funcall func rb))))
                     (level (bilinear #'color:color-level)))
                (when (> (bilinear #'color:color-alpha) 0)
                  (add-pixel x y (make-pixel :char (aref charset (round level step))) pixels))))))))
    (gp:free-image-access acc)
    pixels))
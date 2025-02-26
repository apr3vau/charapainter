;; Copyright (c) 2024, April & May

(in-package charapainter)

(proclaim *optimization*)

(defmacro dorange ((var arg1 &optional arg2 step result-form) &body body)
  (with-unique-names (end unit loop-start)
    (multiple-value-bind (remaining decls)
        (alexandria:parse-body body)
      `(block nil
         (let ,(nconc `((,var ,(if arg2 arg1 0))
                        (,end ,(or arg2 arg1)))
                      (if step `((,unit ,step))))
           ,@decls
           (tagbody ,loop-start
                    ,@remaining
                    (setq ,var ,(if step `(+ ,var ,unit) `(1+ ,var)))
                    (unless (>= ,var ,end)
                      (go ,loop-start))))
         ,result-form))))

(defmacro dorange$fixnum ((var arg1 &optional arg2 step result-form) &body body)
  (with-unique-names (end unit loop-start)
    (multiple-value-bind (remaining decls)
        (alexandria:parse-body body)
      `(block nil
         (let ,(nconc `((,var ,(if arg2 arg1 0))
                        (,end ,(or arg2 arg1)))
                      (if step `((,unit ,step))))
           (declare ,(nconc `(type fixnum ,var ,end)
                            (if step `(type fixnum ,unit))
                            (cdr decls)))
           ,@decls
           (tagbody ,loop-start
                    ,@remaining
                    (setq ,var ,(if step `(+ ,var ,unit) `(1+ ,var)))
                    (unless (>= ,var ,end)
                      (go ,loop-start))))
         ,result-form))))

(defmacro dolist* ((index var list-form &optional result-form) &body body)
  (with-unique-names (len lst loop-start loop-end)
    (multiple-value-bind (remaining decls)
        (alexandria:parse-body body)
      `(block nil
         (let* ((,lst ,list-form)
                (,len (length ,lst))
                ,var
                (,index -1))
           (declare ,(nconc `(type fixnum ,index ,len)
                            (cdr decls)))
           (tagbody ,loop-start
                    (if ,lst
                      (setq ,index (1+ ,index)
                            ,var (pop ,lst))
                      (go ,loop-end))
                    ,@remaining
                    (go ,loop-start)
                    ,loop-end))
         ,result-form))))

(defun |@-reader| (stream char)
  "A refined version of the @ reader in RUTILS

1. We use slot-value instead of smart-slot-value. That's for
efficiency consideration, as in LispWorks, the CLOS and MOP can be
tree-shaked over the deliver level 2. The smart-slot-value use MOP to
implement the function, so the delivered image has to preserve CLOS
when we include RUTILS. That's not what we want.

2. We add a character prefix at the head of element query to use
specialized accessor. Again, for efficiency."
  (declare (ignore char))
  (if (member (peek-char nil stream)
              '(#\Space #\Newline #\Tab #\Return #\Linefeed #\)))
    (intern "@")
    (let ((whole (symbol-name (read stream)))
          (sub (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
          sep acc)
      (map nil (lambda (c)
                 (if (member c '(#\. #\#))
                   (progn
                     (push (cons sep sub) acc)
                     (setq sub (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
                           sep c))
                   (vector-push-extend c sub)))
           whole)
      (push (cons sep sub) acc)
      (reduce (lambda (outer inner)
                (unless (listp (cdr inner)) (setq inner (intern (cdr inner))))
                (case (car outer)
                  (#\. `(slot-value ,inner ',(intern (cdr outer))))
                  (#\# (case (char-downcase (char (cdr outer) 0))
                         (#\l `(nth ,(parse-integer (subseq (cdr outer) 1)) ,inner))
                         (#\v `(aref ,inner ,(parse-integer (subseq (cdr outer) 1))))
                         (#\s `(char ,inner ,(parse-integer (subseq (cdr outer) 1))))
                         (t `(elt ,inner ,(parse-integer (cdr outer))))))))
              acc :from-end t))))

(defun |#h-reader| (stream char arg)
  "Literal hash table reader macro function, specially tuned for
reading large data.

It does not support omitting the test function.  Test function should
always present as the first value of the list."
  (declare (ignore char arg))
  (read-char stream)
  (let* ((test (read stream t nil t))
         (table (gensym "TABLE"))
         (forms (list table))
         key)
    (tagbody
     loop-start
     (let ((c (read-char stream t nil t)))
       (if (whitespace-char-p c)
         (go loop-start)
         (if (not (eql c #\)))
           (progn
             (unread-char c stream)
             (let ((form (read stream t nil t)))
               (if key
                 (progn
                   (setq forms (cons `(setf (gethash ,key ,table) ,form) forms)
                         key nil))
                 (setq key form)))
             (go loop-start))))))
    (if key (error "Unmatched hash-table keys and values"))
    `(let ((,table (make-hash-table :test ,test)))
       ,@forms)))

(set-macro-character #\@ #'|@-reader|)
(set-dispatch-macro-character #\# #\h #'|#h-reader|)

(let ((*redefinition-action* nil)
      (*handle-warn-on-redefinition* nil))
  (defmethod print-object ((obj hash-table) out)
    (format out "#h(~S" (hash-table-test obj))
    (maphash (lambda (key val)
               (write-char #\Space out)
               (prin1 key out)
               (write-char #\Space out)
               (prin1 val out))
             obj)
    (write-char #\) out)))

(defmacro with (clauses &body body)
  (flet ((expand-clause (clause)
           (cond ((symbolp clause)
                  `(let (,clause)))
                 ((and (= (length clause) 2)
                       (symbolp (car clause)))
                  `(let (,clause)))
                 ((and (= (length clause) 2)
                       (listp (car clause)))
                  `(destructuring-bind ,(car clause) ,(cadr clause)
                     (declare (ignore ,(intern "_")))))
                 ((and (> (length clause) 2)
                       (symbolp (second clause)))
                  `(multiple-value-bind ,(butlast clause) ,(car (last clause))
                     (declare (ignore ,(intern "_")))))
                 ((and (= (length clause) 2)
                       (vectorp (car clause)))
                  (with-unique-names (vec)
                    `(let ((,vec ,(second clause)))
                       (let (loop for i from 0
                                  for sym across (car clause)
                                  collect `(,sym (aref ,vec ,i)))
                         (declare (ignore ,(intern "_")))))))
                 ((and (> (length clause) 2)
                       (listp (second clause)))
                  `(labels (,clause))))))
    (reduce (lambda (outer inner) `(,@outer ,inner))
            (nconc (mapcar #'expand-clause clauses)
                   (list `(locally ,@body)))
            :from-end t)))

(setup-indent "with" 1 nil nil 'flet)

(defun make-font-describe-string ()
  (string-append "Font: " *font-family* ", " (princ-to-string *font-size*) "pt"))

(defun gesture-spec-char (spec)
  (case (sys:gesture-spec-modifiers spec)
    (0 (sys:gesture-spec-to-character spec :errorp nil))
    (1 (let ((char (code-char (sys:gesture-spec-data spec))))
         (if (lower-case-p char)
           (char-upcase char)
           char)))))

(defun hex-to-spec (hex)
  (setq hex (string-trim '(#\# #\Space) hex))
  (let ((hex-list (case (length hex)
                    (3 (map 'list #'string hex))
                    (6 (loop for i from 0 to 4 by 2 collect (subseq hex i (+ 2 i))))))
        (deno (if (< (length hex) 6) 15 255)))
    (when hex-list
      (apply #'color:make-rgb
             (mapcar (op (/ (parse-integer _ :radix 16) deno))
                     hex-list)))))

(defun spec-to-hex (spec)
  (setf spec (color:ensure-rgb (color:get-color-spec spec)))
  (format nil "~{~16,2,'0R~}"
          (mapcar #'(lambda (i) (round (* 255 i)))
                  (delete nil
                          (list (color:color-red spec)
                                (color:color-green spec)
                                (color:color-blue spec))))))

(defun point-linenum (point)
  (count-lines (buffers-start (point-buffer point)) point))

(declaim (type double-float rad-to-deg-conversion-factor))
(defconstant rad-to-deg-conversion-factor (/ 180.0d0 pi)
  "Factor used to convert radiants to degrees by multiplication.")

(defun rad-to-deg (radians) (* radians rad-to-deg-conversion-factor))

(defun deg-to-rad (degree) (/ degree rad-to-deg-conversion-factor))

;; Pixels

(defstruct pixel char fg bg)

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
        (add-pixel %x %y %pixel (car all-pixels))))
    new-pixels))

(defun pixels-not-empty-p (pixels)
  (declare (inline pixels-not-empty-p))
  (> (hash-table-count (pixels-table pixels)) 0))

(defmacro with-pixels-boundary (pixels &body body)
  `(let ((%left (pixels-left ,pixels))
         (%top (pixels-top ,pixels))
         (%right (pixels-right ,pixels))
         (%bottom (pixels-bottom ,pixels)))
     (declare (ignorable %left %right %top %bottom))
     ,@body))

(defmacro with-pixels-transformed-boundary (pixels transform &body body)
  (let ((x (gensym))
        (y (gensym)))
    `(loop for (,x ,y) on (gp:transform-points
                           ,transform
                           (list (pixels-left ,pixels) (pixels-top ,pixels)
                                 (pixels-left ,pixels) (pixels-bottom ,pixels)
                                 (pixels-right ,pixels) (pixels-top ,pixels)
                                 (pixels-right ,pixels) (pixels-bottom ,pixels)))
             by #'cddr
           minimize (floor ,x) into %left
           maximize (ceiling ,x) into %right
           minimize (floor ,y) into %top
           maximize (ceiling ,y) into %bottom
           finally (return (progn ,@body)))))

(defun line-pixels (pixel x1 y1 x2 y2 &optional (thickness 1))
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum x1 y1 x2 y2))
  (let ((pixels (make-pixels)))
    (cond ((= x1 x2)
           (dorange$fixnum (y (min y1 y2) (max y1 y2))
             (dorange$fixnum (x (- x1 (floor (/ thickness 2))) (+ x1 (floor (/ thickness 2))))
               (add-pixel x y pixel pixels))))
          ((= y1 y2)
           (dorange$fixnum (x (floor (min x1 x2)) (floor (max x1 x2)))
             (dorange$fixnum (y (- y1 (floor (/ thickness 2))) (+ y1 (floor (/ thickness 2))))
               (add-pixel x y pixel pixels))))
          (t (let* ((k (/ (- y2 y1) (- x2 x1)))
                    (b (- y1 (* k x1))))
               (dorange$fixnum (x (min x1 x2) (max x1 x2))
                 (let ((y (round (+ (* k x) b))))
                   (dorange$fixnum (y0 (- y (floor (/ thickness 2))) (+ y (floor (/ thickness 2))))
                     (add-pixel x y0 pixel pixels))))
               (dorange$fixnum (y (min y1 y2) (max y1 y2))
                 (let ((x (round (/ (- y b) k))))
                   (dorange$fixnum (x0 (- x (floor (/ thickness 2))) (+ x (floor (/ thickness 2))))
                     (add-pixel x0 y pixel pixels)))))))
    pixels))

(defun join-pixels (charset pixels)
  (loop-pixels pixels
    (when %pixel
      (let (connections)
        (when (aand (find-pixel (1- %x) %y pixels)
                    (charset-member (pixel-char it) charset))
          (push :left connections))
        (when (aand (find-pixel (1+ %x) %y pixels)
                    (charset-member (pixel-char it) charset))
          (push :right connections))
        (when (aand (find-pixel %x (1- %y) pixels)
                    (charset-member (pixel-char it) charset))
          (push :top connections))
        (when (aand (find-pixel %x (1+ %y) pixels)
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

(defun rectangle-pixels (x1 y1 x2 y2 &optional border filling)
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum x1 y1 x2 y2))
  (let ((border-func
         (case border
           (:none #'false)
           (:clear (constantly (make-pixel :char #\Space)))
           (:color (constantly (make-pixel :char #\Space :bg *fg*)))
           (:character (constantly (make-pixel :char *char*)))
           (:color-and-character (constantly (make-pixel :char *char* :fg *fg* :bg *bg*)))
           (t (op (make-pixel :char (charset-get border _) :fg *fg* :bg *bg*)))))
        (filling-pixel
         (case filling
           (:clear (make-pixel :char #\Space))
           (:color (make-pixel :char #\Space :bg *fg*))
           (:character (make-pixel :char *char*))
           (:color-and-character (make-pixel :char *char* :fg *fg* :bg *bg*)))))
    (cond ((= x1 x2)
           (line-pixels (funcall border-func :v) x1 y1 x2 y2))
          ((= y1 y2)
           (line-pixels (funcall border-func :h) x1 y1 x2 y2))
          (t (let* ((h-pixel (funcall border-func :h))
                    (v-pixel (funcall border-func :v))
                    (pixels (nunion-pixels
                             (line-pixels v-pixel x1 y1 x1 y2)
                             (line-pixels v-pixel x2 y1 x2 y2)
                             (line-pixels h-pixel x1 y1 x2 y1)
                             (line-pixels h-pixel x1 y2 x2 y2))))
               (when filling-pixel
                 (dorange$fixnum (y (1+ y1) y2)
                   (dorange$fixnum (x (1+ x1) x2)
                     (add-pixel x y filling-pixel pixels))))
               (add-pixel x1 y1 (funcall border-func :rb) pixels)
               (add-pixel x2 y1 (funcall border-func :lb) pixels)
               (add-pixel x1 y2 (funcall border-func :rt) pixels)
               (add-pixel x2 y2 (funcall border-func :lt) pixels)
               pixels)))))

(defmacro with-image-transformed-boundary (image transform &body body)
  (with-unique-names (x y src-w src-h)
    `(let ((,src-w (gp:image-width ,image))
           (,src-h (gp:image-height ,image)))
       (iter (for (,x ,y) :on (gp:transform-points ,transform (list 0 0 0 ,src-h ,src-w 0 ,src-w ,src-h)) :by #'cddr)
             (minimize (floor ,x) into %left)
             (maximize (ceiling ,x) into %right)
             (minimize (floor ,y) into %top)
             (maximize (ceiling ,y) into %bottom)
             (finally (return (locally (declare (type fixnum %left %right %top %bottom))
                                ,@body)))))))

;; Interpolation

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
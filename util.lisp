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
  (with-unique-names (lst loop-start loop-end)
    (multiple-value-bind (remaining decls)
        (alexandria:parse-body body)
      `(block nil
         (let* ((,lst ,list-form)
                ,var
                (,index -1))
           (declare ,(nconc `(type fixnum ,index)
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
  "A refined version of the @ reader in RUTILS"
  (declare (ignore char))
  (if (member (peek-char nil stream)
              '(#\Space #\Newline #\Tab #\Return #\Linefeed #\)))
    (intern "@")
    (let (result
          (str (make-array 0 :element-type 'character :fill-pointer t :adjustable t))
          (level 0)
          sep)
      (loop for c = (read-char stream nil nil t)
            while c
            do (case c
                 (#\( (incf level)
                      (vector-push-extend c str))
                 (#\) (decf level)
                      (when (minusp level)
                        (loop-finish))
                      (vector-push-extend c str))
                 ((or #\Space #\Newline #\Tab #\Return #\Linefeed #\")
                  (if (plusp level)
                    (vector-push-extend c str)
                    (loop-finish)))
                 (#\.
                  (if (plusp level)
                    (vector-push-extend c str)
                    (progn
                      (push (vector sep (read-from-string str)) result)
                      (setq sep c
                            str (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))))
                 (t (vector-push-extend c str)))
            finally (progn
                      (push (vector sep (read-from-string str)) result)
                      (when c (unread-char c stream))))
      (reduce (lambda (outer inner)
                (when (arrayp inner)
                  (setq inner (aref inner 1)))
                (case (aref outer 0)
                  (#\. (list 'slot-value inner (if (listp (aref outer 1)) (aref outer 1)
                                                 (list 'quote (aref outer 1)))))))
              result :from-end t))))

(defun |#@-reader| (stream char arg)
  "Shorthand of chaining slot-value"
  (declare (ignore char arg))
  (read-char stream t nil t)
  (let ((forms (make-array 5 :fill-pointer 0 :adjustable t)))
    (tagbody
     loop-start
     (let ((c (read-char stream t nil t)))
       (if (whitespace-char-p c)
         (go loop-start)
         (if (not (eql c #\)))
           (progn
             (unread-char c stream)
             (vector-push-extend (read stream t nil t) forms)
             (go loop-start))))))
    (reduce (lambda (exp slot)
              `(slot-value ,exp ,(if (listp slot) slot (list 'quote slot))))
            forms)))

(set-macro-character #\@ #'|@-reader|)
(set-dispatch-macro-character #\# #\@ #'|#@-reader|)

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

(defun make-font-describe-string (family size)
  (string-append "Font: " family ", " (princ-to-string size) "pt"))

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
                  (list (color:color-red spec)
                        (color:color-green spec)
                        (color:color-blue spec)
                        (color:color-alpha spec)))))

(defun point-linenum (point)
  (count-lines (buffers-start (point-buffer point)) point))

(declaim (type double-float rad-to-deg-conversion-factor))
(defconstant rad-to-deg-conversion-factor (/ 180.0d0 pi)
  "Factor used to convert radiants to degrees by multiplication.")

(defun rad-to-deg (radians) (* radians rad-to-deg-conversion-factor))

(defun deg-to-rad (degree) (/ degree rad-to-deg-conversion-factor))

(defun ensure-element-interface (element)
  (if (capi:top-level-interface-p element) element
    (capi:element-interface element)))

(defun compose-two-colors (c-old c-new)
  "Can only be used on Premultiplied colors. Colors directly from
IMAGE-ACCESS are already premultiplied."
  (declare (inline compose-two-colors))
  (let* ((alpha-old (color:color-alpha c-old))
         (alpha-new (color:color-alpha c-new))
         (1-alpha-new (- 1 alpha-new)))
    (if (and (< alpha-new 1) (> alpha-old 0))
      (color:make-rgb (+ (* 1-alpha-new (color:color-red c-old))
                         (color:color-red c-new))
                      (+ (* 1-alpha-new (color:color-green c-old))
                         (color:color-green c-new))
                      (+ (* 1-alpha-new (color:color-blue c-old))
                         (color:color-blue c-new))
                      (+ alpha-new (* alpha-old 1-alpha-new)))
      c-new)))

(defmacro editor-pane-point (pane)
  `(buffer-point (capi:editor-pane-buffer ,pane)))

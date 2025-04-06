;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; This is the Visual Line Mode for LispWorks Editor.

;; Sometimes we use the Editor with serif font, and sometimes we want
;; to work with truncated lines, then such a mode will be necessary.

;; It uses the existing "cursorpos" system of LW Editor, which can
;; positioning lines pixelwise horizontally, and character-wise
;; vertically. The commands and behaviours are just like the
;; visual-line-mode of Emacs.

;; Usage: Load this file, enable "Visual Line" minor mode (with
;; command "Visual Line Mode")

(in-package editor)

;; `call-with-window-and-buffer-locked` is the only window locking
;; method that not shaken in LW 8.0.1
(defcommand "Beginning of Visual Line" (p)
     "Same with \"Beginning of Line\", but works with visual line."
     "Same with \"Beginning of Line\", but works with visual line."
  (let ((window (current-window)))
    (call-with-window-and-buffer-locked
     window 2
     (lambda (&rest args)
       (declare (ignore args))
       (let* ((point (current-point))
              (y     (nth-value 1 (point-to-cursorpos point window))))
         (unless (cursorpos-to-point 0 (if p (+ y p) y) window point)
           (editor-error "No such line.")))))))

(defcommand "End of Visual Line" (p)
     "Same with \"End of Line\", but works with visual line."
     "Same with \"End of Line\", but works with visual line."
  (let ((window (current-window)))
    (call-with-window-and-buffer-locked
     window 2
     (lambda (&rest args)
       (declare (ignore args))
       (let* ((point (current-point))
              (px    (window-width window))
              (y     (nth-value 1 (point-to-cursorpos point window))))
         (unless (cursorpos-to-point px (if p (+ y p) y) window point)
           (editor-error "No such line.")))))))

(defcommand "Kill Visual Line" (p)
     "Same with \"Kill Line\", but works with visual line."
     "Same with \"Kill Line\", but works with visual line."
  (let ((window (current-window)))
    (call-with-window-and-buffer-locked
     window 2
     (lambda (&rest args)
       (declare (ignore args))
       (let* ((buffer (current-buffer))
              (point  (buffer-point buffer))
              (px-end (window-width window))
              (y      (nth-value 1 (point-to-cursorpos point window))))
         (with-point ((tpoint point))
           (cond
            (p
             (when (and (not (start-line-p point)) (minusp p))
               (incf p))
             (unless (cursorpos-to-point px-end (+ y p) window tpoint)
               (if (plusp p)
                 (%kill-region  point (buffer-%end buffer)
                                :kill-forward)
                 (%kill-region  (buffer-%start buffer) point 
                                :kill-backward))
               (editor-error))
             (if (plusp p)
               (%kill-region  point tpoint :kill-forward)
               (%kill-region  tpoint point :kill-backward)))
            (t
             (unless (and (blank-after-p tpoint)
                          (line-offset tpoint 1 0))
               (cursorpos-to-point px-end (1+ y) window tpoint))
             (if (point= point tpoint)
               (editor-error "Kill Line attempted at end of buffer")
               (%kill-region point tpoint :kill-forward))))))))))

(define-editor-variable visual-line-target-x-position 0
   "Buffer-local variable for memorizing horizontal cursor position
for line movement in Visual Line mode.")

(defun set-target-x-position (point)
  (symbol-macrolet ((var (buffer-value (point-buffer point)
                                       'visual-line-target-x-position)))
    (if (eq (last-command-type) :line-motion)
      var
      (setf var (point-to-xy-pixels (current-window) point)))))

(defcommand "Previous Visual Line" (p)
     "Same with \"Previous Line\", but works with visual line."
     "Same with \"Previous Line\", but works with visual line."
  (let* ((point  (current-point))
         (window (current-window))
         (height (get-window-height window))
         (y      (nth-value 1 (point-to-cursorpos point window)))
         (offset (or p -1)))
    (when (or (< (+ y offset) 0)
              (> (+ y offset) height))
      (refresh-screen-command nil)
      (setq y (nth-value 1 (point-to-cursorpos point window))))
    (call-with-window-and-buffer-locked
     window 2
     (lambda (&rest args)
       (declare (ignore args))
       (let ((px (set-target-x-position point))
             (y  (nth-value 1 (point-to-cursorpos point window))))
         (unless (cursorpos-to-point px (if p (- y p) (1- y)) window point)
           (editor-error "No previous line.")))))
    (setf (last-command-type) :line-motion)))

(defcommand "Next Visual Line" (p)
     "Same with \"Next Line\", but works with visual line."
     "Same with \"Next Line\", but works with visual line."
  (let* ((point  (current-point))
         (window (current-window))
         (height (get-window-height window))
         (y      (nth-value 1 (point-to-cursorpos point window)))
         (offset (or p -1)))
    (when (or (< (+ y offset) 0)
              (> (+ y offset) height))
      (refresh-screen-command nil)
      (setq y (nth-value 1 (point-to-cursorpos point window))))
    (call-with-window-and-buffer-locked
     window 2
     (lambda (&rest args)
       (declare (ignore args))
       (let ((px (set-target-x-position point))
             (y  (nth-value 1 (point-to-cursorpos point window))))
         (unless (cursorpos-to-point px (if p (+ y p) (1+ y)) window point)
           (editor-error "No previous line.")))))
    (setf (last-command-type) :line-motion)))

(defmode "Visual Line"
  :key-bindings '(("Beginning of Visual Line" "Control-a")
                  ("End of Visual Line"       "Control-e")
                  ("Kill Visual Line"         "Control-k")
                  ("Previous Visual Line"     "Control-p")
                  ("Next Visual Line"         "Control-n")))

(defcommand "Visual Line Mode" (p)
     "Toggle the Visual Line Mode. If P is supplied, turn on the mode
if P is positive, turn off if not."
     "Toggle the Visual Line Mode. If P is supplied, turn on the mode
if P is positive, turn off if not."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "Visual Line")
          (if p (plusp p)
            (not (buffer-minor-mode buffer "Visual Line"))))))

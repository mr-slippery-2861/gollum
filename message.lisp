(in-package :gollum)

(defvar *message-window-gravity* :bottom-center)

(defun hide-screen-message (screen)
  (xlib:unmap-window (message-window screen))
  (flush-display (display screen)))

(defun filt-color-controling (string)
  (let ((raw-string string)
	(start 0)
	(done nil)
	(last-token nil))
    (loop until done
	 (let ((caret (position #\^ raw-string :start start))
	   (cond
	     ((null caret) (setf done t))
	     ((char= (elt raw-string (1+ caret)) #\^) (setf raw-string (remove #\^ raw-string :count 1)
							    start (1+ caret)))
	     ((digit-char-p (elt raw-string (1+ caret))) )
	     (progn
	       ))
(defmethod screen-message ((screen screen) message &optional (time-out-p t))
  (let* ((font (message-font screen))
	 (message-window (message-window screen))
	 (message-gc (message-gc screen)))
    (setup-window-for-drawing-glyphs screen message-window *message-window-gravity* message-gc font message)
    (xlib:draw-image-glyphs message-window message-gc *internal-window-horizontal-padding* (+ *internal-window-vertical-padding* (xlib:font-ascent font)) message)
    (when time-out-p
      (schedule-timer (message-timer screen)))
    (flush-display (display screen))))

(defun message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments)))

(defun permanent-message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments) nil))
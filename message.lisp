(in-package :gollum)

(defvar *message-window-gravity* :bottom-center)

(defun hide-screen-message (screen)
  (xlib:unmap-window (message-window screen))
  (flush-display (display screen)))

(defun filt-color-controling (string)
  (let ((raw-string nil)
	(caret -3)
	(current 0))
    (loop for x across string
	 do (progn
	      (cond
		((char= x #\^)
		 (if (= (- current caret) 1)
		     (setf raw-string (concat raw-string (string x)))
		     (setf caret current)))
		((> (- current caret) 2)
		 (setf raw-string (concat raw-string (string x))))
		((= (- current caret) 2)
		 (unless (or (digit-char-p x) (char= x #\*))
		   (setf raw-string (concat raw-string (string x)))))
		((= (- current caret) 1)
		 (unless (digit-char-p x)
		   (setf caret -3))))
	      (setf current (1+ current))))
    raw-string))


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
(in-package :gollum)

(defvar *message-window-gravity* :bottom-center)

(defun hide-screen-message (screen)
  (xlib:unmap-window (message-window screen))
  (flush-display (display screen)))

(defmethod screen-message ((screen screen) message &optional (time-out-p t))
  (let* ((font (message-font screen))
	 (height (+ (xlib:font-descent font) (xlib:font-ascent font)))
	 (width (xlib:text-width font message))
	 (message-window (message-window screen))
	 (message-gc (message-gc screen)))
    (xlib:with-state (message-window)
      (if (eql (xlib:window-map-state message-window) :unmapped)
	  (xlib:map-window message-window))
      (setf (xlib:drawable-height message-window) (+ (* 2 *internal-window-vertical-padding*) height)
	    (xlib:drawable-width message-window) (+ (* 2 *internal-window-horizontal-padding*) width)
	    (xlib:window-priority message-window) :above)
      (calculate-geometry screen message-window *message-window-gravity*))
    (xlib:with-gcontext (message-gc :foreground (xlib:gcontext-background message-gc))
      (xlib:draw-rectangle message-window message-gc 0 0 (xlib:drawable-width message-window) (xlib:drawable-height message-window) t))
    (xlib:draw-image-glyphs message-window message-gc *internal-window-horizontal-padding* (+ *internal-window-vertical-padding* (xlib:font-ascent font)) message)
    (when time-out-p
      (schedule-timer (message-timer screen)))))

(defun message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments)))

(defun permanent-message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments) nil))
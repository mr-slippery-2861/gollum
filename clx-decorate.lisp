(in-package :gollum)

(defvar *decorate-background-color* "blue")

(defvar *decorate-foreground-color* "black")

(defvar *decorate-vertical-padding* 1)

(defclass clx-decorate (decorate)
  ((xwindow :initarg :xwindow
	    :accessor xwindow
	    :initform nil)))

(defun make-decorate (window)
  (let* ((screen (screen window))
	 (font (output-font screen))
	 (width (xlib:drawable-width (xwindow window)))
	 (font-height (+ (xlib:font-ascent font) (xlib:font-descent font)))
	 (button-size (floor (* 0.618 font-height)))
	 (height (+ *decorate-vertical-padding* font-height))
	 (title (xlib:create-window :parent (xmaster window)
				    :x 0 :y 0 :width width :height height
				    :background (alloc-color *decorate-background-color* screen)
				    :event-mask '(:button-press :button-release :owner-grab-button :exposure)
				    :override-redirect :on
				    :backing-store :always
				    :save-under :on))
	 (x-button (xlib:create-window :parent title
				       :x (- width font-height 5)
				       :y (floor (/ (- height button-size 2) 2))
				       :width button-size :height button-size
				       :background (alloc-color "red" screen)
				       :border (alloc-color "gray" screen)
				       :border-width 1
				       :event-mask '(:button-press)
				       :override-redirect :on
				       :save-under :on)))
    (xlib:map-window x-button)
    (xlib:map-window title)
    (make-instance 'clx-decorate
		   :window window
		   :height height
		   :xwindow title)))

(defmethod update-title ((decorate clx-decorate))
  (let ((screen (screen (window decorate))))
    (output-to-window screen (xwindow decorate) (output-gc screen) :keep (wm-name (window decorate)))))

(defmethod destroy-decorate ((decorate clx-decorate))
  (xlib:destroy-subwindows (xwindow decorate))
  (xlib:destroy-window (xwindow decorate)))

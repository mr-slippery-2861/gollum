(in-package :gollum)

(defun set-internal-window-type (xwindow type)
  (xlib:change-property xwindow :__GOLLUM_INTERNAL (list (case type
							   (:master 1)
							   (:title 2)
							   (:border-nw 3)
							   (:border-top 4)
							   (:border-ne 5)
							   (:border-right 6)
							   (:border-se 7)
							   (:border-bottom 8)
							   (:border-sw 9)
							   (:border-left 10)
							   (:root 11))) :__GOLLUM_INTERNAL 32))

(defun get-internal-window-type (xwindow)
  (let ((type (car (xlib:get-property xwindow :__GOLLUM_INTERNAL))))
    (case type
      (1 :master)
      (2 :title)
      (3 :border-nw)
      (4 :border-top)
      (5 :border-ne)
      (6 :border-right)
      (7 :border-se)
      (8 :border-bottom)
      (9 :border-sw)
      (10 :border-left)
      (11 :root)
      (t nil))))

(defun set-xwindow-geometry (xwindow &key x y width height)
  (xlib:with-state (xwindow)
    (if x (setf (xlib:drawable-x xwindow) x))
    (if y (setf (xlib:drawable-y xwindow) y))
    (if width (setf (xlib:drawable-width xwindow) width))
    (if height (setf (xlib:drawable-height xwindow) height))))

(defun create-gcontext (xwindow bg fg font)
  (xlib:create-gcontext :drawable xwindow
			:background bg
			:foreground fg
			:font font))


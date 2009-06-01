(in-package :gollum)

(defun alloc-color (color screen)
  (xlib:alloc-color (xlib:screen-default-colormap (xscreen screen)) color))
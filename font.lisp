(in-package :gollum)

(defun open-font (display name)
  (xlib:open-font (xdisplay display) name))
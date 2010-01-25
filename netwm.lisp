(in-package :gollum)

(defun get-property-utf8 (xwindow property)
  (let ((vector (xlib:get-property xwindow property :result-type '(vector (unsigned-byte 8)))))
    (if vector
	(babel:octets-to-string vector :encoding :utf-8))))

(defmethod get-net-wm-name ((window toplevel-window))
  (get-property-utf8 (xwindow window) :_NET_WM_NAME))


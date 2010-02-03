(in-package :gollum)

(defparameter +net-supported+
  '(:_NET_CLIENT_LIST
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_DESKTOP_NAMES
    :_NET_ACTIVE_WINDOW
    :_NET_SUPPORTING_WM_CHECK))
    
;; we read a property, copy it, then update it
(defmacro property-rcu (xwindow property type format operator)
  `(let ((old-value (xlib:get-property ,xwindow ,property))
	 new-value)
     (setf new-value (funcall ,operator old-value))
     (xlib:change-property ,xwindow ,property new-value ,type ,format)))

(defun add-to-net-client-list (xroot xwindow-id)
  (property-rcu xroot :_NET_CLIENT_LIST :WINDOW 32 (lambda (client-list) (append client-list (list xwindow-id)))))

(defun remove-from-net-client-list (xroot xwindow-id)
  (property-rcu xroot :_NET_CLIENT_LIST :WINDOW 32 (lambda (client-list) (remove xwindow-id client-list :test #'=))))

(defun add-to-net-client-list-stacking (xroot xwindow-id)
  (property-rcu xroot :_NET_CLIENT_LIST_STACKING :WINDOW 32 (lambda (client-list) (append client-list (list xwindow-id)))))

(defun remove-from-net-client-list-stacking (xroot xwindow-id)
  (property-rcu xroot :_NET_CLIENT_LIST :WINDOW 32 (lambda (client-list) (remove xwindow-id client-list :test #'=))))

(defun set-number-of-desktops (xroot number)
  (xlib:change-property xroot :_NET_NUMBER_OF_DESKTOPS (list number) :CARDINAL 32))

(defun set-desktop-geometry (xroot width height)
  (xlib:change-property xroot :_NET_DESKTOP_GEOMETRY (list width height) :CARDINAL 32))

(defun set-desktop-viewport (xroot x y)
  (xlib:change-property xroot :_NET_DESKTOP_VIEWPORT (list x y) :CARDINAL 32))

(defun set-current-desktop (xroot id)	;id ranges from 0 to _NET_NUMBER_OF_DESKTOPS - 1
  (xlib:change-property xroot :_NET_CURRENT_DESKTOP (list id) :CARDINAL 32))

(defun encode-string (string)
  (if string
      (concatenate 'sequence (babel:string-to-octets string :encoding :utf-8) '(0))
      '(0)))

(defun set-desktop-names (xroot names)
  (xlib:change-property xroot :_NET_DESKTOP_NAMES (apply #'concatenate 'sequence (mapcar #'encode-string names)) :UTF8_STRING 8))

(defun set-active-window (xroot id)
  (xlib:change-property xroot :_NET_ACTIVE_WINDOW (list id) :WINDOW 32))

(defun set-wm-check (xroot)
  (let ((child (xlib:create-window :parent xroot
				   :x 0 :y 0 :width 1 :height 1
				   :override-redirect :on)))
    (xlib:change-property xroot :_NET_SUPPORTING_WM_CHECK (list (xlib:window-id child)) :WINDOW 32)
    (xlib:change-property child :_NET_SUPPORTING_WM_CHECK (list (xlib:window-id child)) :WINDOW 32)
    (xlib:change-property child :_NET_WM_NAME (babel:string-to-octets +myname+) :UTF8_STRING 8)
    child))

(defmacro get-property-utf8 (xwindow property)
  `(let ((vector (xlib:get-property ,xwindow ,property :result-type '(vector (unsigned-byte 8)))))
     (if vector (babel:octets-to-string vector :encoding :utf-8))))

(defun get-net-wm-name (xwindow)
  (get-property-utf8 xwindow :_NET_WM_NAME))

(defun get-net-wm-icon-name (xwindow)
  (get-property-utf8 xwindow :_NET_WM_ICON_NAME))

(defun get-net-wm-desktop (xwindow)
  (car (xlib:get-property xwindow :_NET_WM_DESKTOP)))

(defun set-net-wm-desktop (xwindow id)
  (if id
      (xlib:change-property xwindow :_NET_WM_DESKTOP (list id) :CARDINAL 32)
      (xlib:delete-property xwindow :_NET_WM_DESKTOP)))

(defun get-net-wm-window-type (xwindow)
  (xlib:get-property xwindow :_NET_WM_WINDOW_TYPE))
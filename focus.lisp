(in-package :gollum)

;; Input Model 	        Input Field 	WM_TAKE_FOCUS
;; No Input 	        False 	        Absent             we are not needed to set
;; Passive 	        True 	        Absent             we are needed to set
;; Locally Active 	True 	        Present            we are needed to set
;; Globally Active 	False 	        Present            we are not needed to set
(defun input-field (window)
  (let ((hints (xlib:wm-hints (xwindow window))))
    (if hints
	(eql (xlib:wm-hints-input hints) :on)
	nil)))

(defun should-grab-input (window)
  (if (input-field window)
      t
      nil))

(defmethod set-input-focus ((focus toplevel-window) &optional (revert-to :pointer-root))
  (xlib:set-input-focus (xdisplay (display focus)) (xwindow focus) revert-to))

(defun set-focus (window)
  (if window
      (let ((root (root (screen window))))
	(set-input-focus window :pointer-root)
	(set-active-window (xwindow root) (id window))))) ;netwm

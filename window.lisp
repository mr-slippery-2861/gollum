(in-package :gollum)

(defclass window ()
  ((id :initarg :id
       :accessor id
       :initform nil)
   (xwindow :initarg :xwindow		;a xlib:window instance
	    :accessor xwindow
	    :initform nil)
   (workspace :initarg :workspace	;a workspace instance
	      :accessor workspace
	      :initform nil)
   (screen :initarg :screen
	   :accessor screen
	   :initform nil)
   (display :initarg :display
	    :accessor display
	    :initform nil)
   (orig-x :initarg :orig-x
	   :accessor orig-x
	   :initform 0)
   (orig-y :initarg :orig-y
	   :accessor orig-y
	   :initform 0)
   (orig-width :initarg :orig-width
	       :accessor orig-width
	       :initform nil)
   (orig-height :initarg :orig-height
		:accessor orig-height
		:initform nil)
   (size-state :initarg :size-state
	       :accessor size-state
	       :initform :normal)
   (title :initarg :title		;it's the title bar,as my plan,it can be a gtk window
	  :accessor title
	  :initform nil)
   (wm-name :initarg :wm-name
	     :accessor wm-name
	     :initform nil)
   (wm-class :initarg :wm-class
	      :accessor wm-class
	      :initform nil)
   (group :initarg :group		;this is not the same as group in stumpwm at all!more like in starcraft
	  :accessor group
	  :initform nil)
   (parent :initarg :parent
	   :accessor parent
	   :initform nil)
   (map-state :initarg :map-state
	      :accessor map-state
	      :initform :unmapped)
   (ws-map-state :initarg :ws-map-state
		 :accessor ws-map-state
		 :initform :unmapped)))

(defgeneric map-workspace-window (win)
  (:documentation "make a window viewable in current workspace,if it is not explicitly unmapped by user"))

(defgeneric map-window (win)
  (:documentation "make a window viewable"))

(defgeneric unmap-workspace-window (win)
  (:documentation "make a window unviewable in current workspace"))

(defgeneric unmap-window (win)
  (:documentation "make a window unviewable"))

(defgeneric window-equal (w1 w2)
  (:documentation "return T if w1 and w2 refer to the same window"))

(defgeneric raise-window (win)
  (:documentation "put the window on top of other windows"))

(defgeneric set-input-focus (window))

(defgeneric match-window (win &key class name)
  (:documentation "return T if a window satisfies the description against match-type"))

(defgeneric apply-place-rule (window place-rule))

(defgeneric place-window-according-to-rule (window match-rule place-rule))

(defgeneric place-window (window))

(defgeneric maximize-window (window))

(defgeneric restore-window (window))

(defgeneric minimize-window (window))

(defgeneric grab-key (win keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-key (win keycode &key modifiers))

(defgeneric grab-keyboard (win &key owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-keyboard (display &key time))

(defun mapped (window)
  (eql (map-state window) :viewable))

(defun not-mapped (window)
  (eql (map-state window) :unmapped))

(defun should-be-mapped (window)
  (eql (ws-map-state window) :viewable))

(defmethod map-workspace-window ((win window))
  (when (and (plusp (get-wm-state win)) (should-be-mapped win) (not-mapped win))
    (xlib:map-window (xwindow win))
    (setf (map-state win) :viewable)))

(defmethod map-window ((win window))
  (when (eql (map-state win) :unmapped)
    (xlib:map-window (xwindow win))
    (setf (map-state win) :viewable))
  (setf (ws-map-state win) :viewable))

(defmethod unmap-workspace-window ((win window))
  (when (mapped win)
    (xlib:unmap-window (xwindow win))
    (setf (map-state win) :unmapped)))

(defmethod unmap-window ((win window))
  (when (eql (map-state win) :viewable)
    (xlib:unmap-window (xwindow win))
    (setf (map-state win) :unmapped))
  (setf (ws-map-state win) :unmapped))

(defmethod window-equal ((w1 window) (w2 window))
  (= (id w1) (id w2)))

(defmethod raise-window ((win window))
  (xlib:with-state ((xwindow win))
    (setf (xlib:window-priority (xwindow win)) :top-if)))

(defmethod set-input-focus ((window window))
  (xlib:set-input-focus (xdisplay (display window)) (xwindow window) :parent))

(defun kill-window (window)
  (let ((display (display window)))
    (if (find :WM_DELETE_WINDOW (xlib:wm-protocols (xwindow window)))
	(xlib:send-event (xwindow window) :client-message nil :window (xwindow window)
			                                      :type :WM_PROTOCOLS
							      :format 32
							      :data (list (xlib:intern-atom (xdisplay display) :WM_DELETE_WINDOW)))
	(xlib:kill-client (xdisplay display) (id window)))))

(defun kill ()
  (kill-window (current-window nil)))

(defmethod match-window ((win window) &key class name)
  (and
   (if (null class) t (string= (wm-class win) class))
   (if (null name) t (string= (wm-name win) name))))

(defun find-matching-window (all-windows &key class name)
  (loop for k being the hash-keys in all-windows using (hash-value v)
     when (match-window v :class class :name name)
     return v))

(defmethod maximize-window ((window window))
  (let* ((screen (screen window))
	 (max-width (width screen))
	 (max-height (height screen))
	 (xwindow (xwindow window)))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-x xwindow) (x screen)
	    (xlib:drawable-y xwindow) (y screen)
	    (xlib:drawable-width xwindow) max-width
	    (xlib:drawable-height xwindow) max-height))
    (setf (size-state window) :maximized)
    (flush-display (display window))))

(defun maximized (window)
  (eql (size-state window) :maximized))

(defun maximize ()
  (maximize-window (current-window nil)))

(defmethod restore-window ((window window))
  (let* ((x (orig-x window))
	 (y (orig-y window))
	 (width (orig-width window))
	 (height (orig-height window))
	 (xwindow (xwindow window)))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-x xwindow) x
	    (xlib:drawable-y xwindow) y
	    (xlib:drawable-width xwindow) width
	    (xlib:drawable-height xwindow) height))
    (setf (size-state window) :normal)
    (flush-display (display window))))

(defun restore ()
  (restore-window (current-window nil)))

(defmethod grab-key ((win window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xwindow win) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod ungrab-key ((window window) keycode &key (modifiers 0))
  (xlib:ungrab-key (xwindow window) keycode :modifiers modifiers))

(defmethod grab-keyboard ((win window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xwindow win) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defun get-wm-state (window)
  (car (xlib:get-property (xwindow window) :WM_STATE)))

(defun set-wm-state (xwindow state)
  (xlib:change-property xwindow :WM_STATE (list state) :WM_STATE 32))
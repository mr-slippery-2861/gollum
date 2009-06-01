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
   (title :initarg :title		;it's the title bar,as my plan,it can be a gtk window
	  :accessor title
	  :initform nil)
   (win-name :initarg :win-name
	     :accessor win-name
	     :initform nil)
   (win-class :initarg :win-class
	      :accessor win-class
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

(defgeneric delete-window (win all-windows)
  (:documentation "make a window out of gollum's control"))

(defgeneric window-equal (w1 w2)
  (:documentation "return T if w1 and w2 refer to the same window"))

(defgeneric raise-window (win)
  (:documentation "put the window on top of other windows"))

(defgeneric match-window (win &key class name)
  (:documentation "return T if a window satisfies the description against match-type"))

(defgeneric grab-key (win keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p))

(defgeneric grab-keyboard (win &key owner-p sync-pointer-p sync-keyboard-p))

(defmethod map-workspace-window ((win window))
  (unless (eql (ws-map-state win) :unmapped)
    (when (eql (map-state win) :unmapped)
      (xlib:map-window (xwindow win))
      (setf (map-state win) :viewable))))

(defmethod map-window ((win window))
  (when (eql (map-state win) :unmapped)
    (xlib:map-window (xwindow win))
    (setf (map-state win) :viewable))
  (setf (ws-map-state win) :viewable))

(defmethod map-window-maybe ((win window))
  (unless (eql (ws-map-state win) :unmapped)
    (map-window win)))

(defmethod unmap-workspace-window ((win window))
  (when (eql (map-state win) :viewable)
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

(defun kill-window (window xdisplay)
  (xlib:kill-client xdisplay (xlib:window-id window)))

(defmethod match-window ((win window) &key class name)
  (and
   (if (null class) t (string= (win-class win) class))
   (if (null name) t (string= (win-name win) name))))

(defun find-matching-window (all-windows &key class name)
  (loop for k being the hash-keys in all-windows using (hash-value v)
     when (match-window v :class class :name name)
     return v))

(defmethod grab-key ((win window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xwindow win) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod grab-keyboard ((win window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xwindow win) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))
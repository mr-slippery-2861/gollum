(in-package :gollum)

(defclass window ()
  ((id :initarg :id
       :accessor id
       :initform nil)
   (xmaster :initarg :xmaster		;prepare for reparenting
	    :accessor xmaster
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
   (wm-instance :initarg :wm-instance
		:accessor wm-instance
		:initform nil)
   (wm-class :initarg :wm-class
	      :accessor wm-class
	      :initform nil)
   (protocols :initarg :protocols
	      :accessor protocols
	      :initform nil)
   (group :initarg :group ;this is not the same as group in stumpwm at all!more like in starcraft
	  :accessor group
	  :initform nil)
   (toplevel-p :initarg :toplevel-p
	       :accessor toplevel-p
	       :initform nil)
   (parent :initarg :parent		;this slot seems no much use
	   :accessor parent
	   :initform nil)
   (map-state :initarg :map-state
	      :accessor map-state
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

(defgeneric circulate-window-down (window))

(defgeneric circulate-window-up (window))

(defgeneric set-input-focus (focus &optional revert-to))

(defgeneric match-window (win &key instance class name)
  (:documentation "return T if a window satisfies the description against match-type"))

(defgeneric apply-place-rule (window place-rule))

(defgeneric place-window-according-to-rule (window match-rule place-rule))

(defgeneric place-window (window))

(defgeneric maximize-window (window))

(defgeneric restore-window (window))

(defgeneric minimize-window (window))

(defgeneric reconfigure-window (window x y width height &key prompt))

(defgeneric resize-window (window width height &key prompt))

(defgeneric move-window (window x y))

(defgeneric grab-key (win keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-key (win keycode &key modifiers))

(defgeneric grab-keyboard (win &key owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-keyboard (display &key time))

(defgeneric print-obj (obj))

(defun mapped (window)
  (eql (map-state window) :viewable))

(defun not-mapped (window)
  (eql (map-state window) :unmapped))

(defun should-be-mapped (window)
  (plusp (get-wm-state window)))

(defmethod map-workspace-window ((window window))
  (when (and (should-be-mapped window) (not-mapped window))
    (xlib:map-window (xmaster window))
    (setf (map-state window) :viewable)))

(defmethod map-window ((window window))
  (when (not-mapped window)
    (xlib:map-window (xmaster window))
    (setf (map-state window) :viewable)))

(defmethod unmap-workspace-window ((window window))
  (when (mapped window)
    (xlib:unmap-window (xmaster window))
    (setf (map-state window) :unmapped)))

(defmethod unmap-window ((window window))
  (when (mapped window)
    (xlib:unmap-window (xmaster window))
    (setf (map-state window) :unmapped)))

(defmethod window-equal ((w1 window) (w2 window))
  (= (id w1) (id w2)))

(defmethod raise-window ((window window))
    (setf (xlib:window-priority (xmaster window)) :top-if))

(defmethod circulate-window-down ((window window))
  (xlib:circulate-window-down (xmaster window)))

(defmethod circulate-window-up ((window window))
  (xlib:circulate-window-up (xmaster window)))

(defmethod set-input-focus ((focus window) &optional (revert-to :pointer-root))
  (xlib:set-input-focus (xdisplay (display focus)) (xwindow focus) revert-to))

(defun send-client-message (window type &rest data)
  (xlib:send-event (xwindow window) :client-message nil
		   :window (xwindow window)
		   :type type
		   :format 32
		   :data (mapcar (lambda (x)
				   (if (keywordp x)
				       (xlib:intern-atom (xdisplay (display window)) x)
				       x)) data)))

(defun kill-window (window)
  (let ((display (display window)))
    (if (find :wm_delete_window (protocols window))
	(send-client-message window :wm_protocols :wm_delete_window)
	(xlib:kill-client (xdisplay display) (xlib:window-id (xwindow window))))))

(defun kill ()
  (if (current-window nil)
      (kill-window (current-window nil))))

(defmethod match-window ((window window) &key instance class name)
  (and
   (if (null class) t (string= (wm-class window) class))
   (if (null name) t (string= (wm-name window) name))
   (if (null instance) t (string= (wm-instance window) instance))))

(defun find-matching-window (all-windows &key instance class name)
  (loop for k being the hash-keys in all-windows using (hash-value v)
     when (match-window v :instance instance :class class :name name)
     return v))

(defun group-window (window group)
  (setf (group window) group))

(defmethod maximize-window ((window window))
  (let* ((screen (screen window))
	 (max-width (- (width screen) (* 2 *default-window-border-width*)))
	 (max-height (- (height screen) (* 2 *default-window-border-width*)))
	 (xmaster (xmaster window))
	 (xwindow (xwindow window)))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) (x screen)
	    (xlib:drawable-y xmaster) (y screen)
	    (xlib:drawable-width xmaster) max-width
	    (xlib:drawable-height xmaster) max-height))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) max-width
	    (xlib:drawable-height xwindow) max-height))
    (setf (size-state window) :maximized)
    (flush-display (display window))))

(defun maximized (window)
  (eql (size-state window) :maximized))

(defun maximize ()
  (if (current-window nil)
      (maximize-window (current-window nil))))

(defmethod restore-window ((window window))
  (let* ((x (orig-x window))
	 (y (orig-y window))
	 (width (orig-width window))
	 (height (orig-height window))
	 (xmaster (xmaster window))
	 (xwindow (xwindow window)))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) x
	    (xlib:drawable-y xmaster) y
	    (xlib:drawable-width xmaster) width
	    (xlib:drawable-height xmaster) height))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) width
	    (xlib:drawable-height xwindow) height))
    (setf (size-state window) :normal)
    (flush-display (display window))))

(defun restore ()
  (if (current-window nil)
      (restore-window (current-window nil))))

(defmethod move-window ((window window) x y)
  (let ((xmaster (xmaster window)))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) x
	    (xlib:drawable-y xmaster) y))
    (setf (orig-x window) x
	  (orig-y window) y)))

(let ((target-window nil)
      (offset-x nil)
      (offset-y nil))
  (defun set-drag-move-window (window)
    (setf target-window window)
    (if window
	(multiple-value-setq (offset-x offset-y) (xlib:query-pointer (xmaster window)))
	(setf offset-x nil
	      offset-y nil)))
  (defun drag-move-window (x y)
    (when target-window
      (let* ((screen (screen target-window))
	     (min-x (x screen))
	     (min-y (y screen))
	     (max-x (+ min-x (width screen)))
	     (max-y (+ min-y (height screen)))
	     (new-x (second (sort (list min-x (- x offset-x) max-x) #'<)))
	     (new-y (second (sort (list min-y (- y offset-y) max-y) #'<))))
	(move-window target-window new-x new-y)
	(flush-display (display screen))))))

(defmethod grab-key ((win window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xmaster win) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod ungrab-key ((window window) keycode &key (modifiers 0))
  (xlib:ungrab-key (xmaster window) keycode :modifiers modifiers))

(defmethod grab-keyboard ((win window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xmaster win) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defun get-wm-state (window)
  (car (xlib:get-property (xwindow window) :WM_STATE)))

(defun set-wm-state (xwindow state)
  (xlib:change-property xwindow :WM_STATE (list state) :WM_STATE 32))

(defun set-gollum-master (xwindow)
  (xlib:change-property xwindow :__GOLLUM_MASTER '(1) :__GOLLUM_MASTER 32))

(defun gollum-master-p (xwindow)
  (let ((gollum-master (car (xlib:get-property xwindow :__GOLLUM_MASTER))))
    (if gollum-master
	(plusp gollum-master)
	nil)))

(defun translate-coordinates (src src-x src-y dst)
  (xlib:translate-coordinates (xmaster src) src-x src-y (xmaster dst)))

(defvar *default-window-border* "blue")

(defvar *default-window-border-width* 2)
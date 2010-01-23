(in-package :gollum)

(defclass window ()
  ((id :initarg :id
       :accessor id
       :initform nil)
   (xmaster :initarg :xmaster		;prepare for reparenting
	    :accessor xmaster
	    :initform nil)
   (xframe :initarg :xframe
	   :accessor xframe
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
   (min-width :initarg :min-width
	      :accessor min-width
	      :initform 1)
   (min-height :initarg :min-height
	       :accessor min-height
	       :initform 1)
   (size-state :initarg :size-state
	       :accessor size-state
	       :initform :normal)
   (decorate :initarg :decorate
	     :accessor decorate
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

(defmethod x ((window window))
  (xlib:drawable-x (xmaster window)))

(defmethod (setf x) (new-x (window window))
  (setf (xlib:drawable-x (xmaster window)) new-x))

(defmethod y ((window window))
  (xlib:drawable-y (xmaster window)))

(defmethod (setf y) (new-y (window window))
  (setf (xlib:drawable-y (xmaster window)) new-y))

(defmethod width ((window window))
  (xlib:drawable-width (xmaster window)))

(defmethod (setf width) (new-width (window window))
  (setf (xlib:drawable-width (xmaster window)) new-width))

(defmethod height ((window window))
  (xlib:drawable-height (xmaster window)))

(defmethod (setf height) (new-height (window window))
  (setf (xlib:drawable-height (xmaster window)) new-height))

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
	 (max-width (width screen))
	 (max-height (height screen))
	 (xmaster (xmaster window))
	 (xframe (xframe window))
	 (xwindow (xwindow window))
	 (double-border (* 2 *default-window-border-width*))
	 (title-height (title-height (decorate window))))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) (x screen)
	    (xlib:drawable-y xmaster) (y screen)
	    (xlib:drawable-width xmaster) max-width
	    (xlib:drawable-height xmaster) max-height))
    (xlib:with-state (xframe)
      (setf (xlib:drawable-width xframe) (- max-width double-border)
	    (xlib:drawable-height xframe) (- max-height double-border title-height)))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) (- max-width double-border)
	    (xlib:drawable-height xwindow) (- max-height double-border title-height)))
    (update-decorate (decorate window))
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
	 (xframe (xframe window))
	 (xwindow (xwindow window))
	 (double-border (* 2 *default-window-border-width*))
	 (title-height (title-height (decorate window))))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) x
	    (xlib:drawable-y xmaster) y
	    (xlib:drawable-width xmaster) width
	    (xlib:drawable-height xmaster) height))
    (xlib:with-state (xframe)
      (setf (xlib:drawable-width xframe) (- width double-border)
	    (xlib:drawable-height xframe) (- height double-border title-height)))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) (- width double-border)
	    (xlib:drawable-height xwindow) (- height double-border title-height)))
    (update-decorate (decorate window))
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

(defmethod moveresize-window ((window window) &key x y width height)
  (let ((xmaster (xmaster window)))
    (xlib:with-state (xmaster)
      (if x (setf (xlib:drawable-x xmaster) x))
      (if y (setf (xlib:drawable-y xmaster) y))
      (if width (setf (xlib:drawable-width xmaster) width))
      (if height (setf (xlib:drawable-height xmaster) height)))
    (if x (setf (orig-x window) x))
    (if y (setf (orig-y window) y))
    (if width (setf (orig-width window) width))
    (if height (setf (orig-height window) height))
    (when (or width height)
      (let ((xframe (xframe window))
	    (xwindow (xwindow window))
	    (double-border (* 2 *default-window-border-width*))
	    (title-height (title-height (decorate window))))
	(xlib:with-state (xframe)
	  (if width (setf (xlib:drawable-width xframe) (- width double-border)))
	  (if height (setf (xlib:drawable-height xframe) (- height double-border title-height))))
	(xlib:with-state (xwindow)
	  (if width (setf (xlib:drawable-width xwindow) (- width double-border)))
	  (if height (setf (xlib:drawable-height xwindow) (- height double-border title-height))))
	(update-decorate (decorate window))))))

(let ((target-window nil)
      (p-x nil)
      (p-y nil)
      (init-x nil)
      (init-y nil)
      (init-width nil)
      (init-height nil)
      (operation nil))			;:move :nw-resize ... :left-resize
  (defun init-drag-moveresize (window pointer-x pointer-y type)
    (setf target-window window
	  p-x pointer-x
	  p-y pointer-y
	  init-x (x window)
	  init-y (y window)
	  init-width (width window)
	  init-height (height window)
	  operation type))
  (defun reset-drag-moveresize ()
    (setf target-window nil
	  p-x nil
	  p-y nil
	  init-x nil
	  init-y nil
	  init-width nil
	  init-height nil
	  operation nil))
  (defun drag-moveresize-window (x y)
    (labels ((mid (a b c)
	       (second (sort (list a b c) #'<))))
    (when target-window
      (let* ((screen (screen target-window))
	     (min-x (x screen))
	     (min-y (y screen))
	     (max-x (+ min-x (width screen)))
	     (max-y (+ min-y (height screen)))
	     (diff-x (- x p-x))
	     (diff-y (- y p-y))
	     (min-width (min-width target-window))
	     (min-height (min-height target-window))
	     (new-x (mid min-x (+ init-x diff-x) max-x))
	     (new-y (mid min-y (+ init-y diff-y) max-y)))
	(dformat 2 "now pointer at (~a, ~a)" x y)
	(case operation
	  (:move (moveresize-window target-window :x new-x :y new-y))
	  (:nw-resize (moveresize-window target-window
					 :x new-x :y new-y
					 :width (mid min-width
						     (- init-width diff-x)
						     (- (+ init-x init-width) (x screen)))
					 :height (mid min-height
						      (- init-height diff-y)
						      (- (+ init-y init-height) (y screen)))))
	  (:top-resize (moveresize-window target-window
					  :y new-y
					  :height (mid min-height
						       (- init-height diff-y)
						       (- (+ init-y init-height) (y screen)))))
	  (:ne-resize (moveresize-window target-window
					 :y new-y
					 :width (mid min-width (+ init-width diff-x) (- max-x init-x))
					 :height (mid min-height
						       (- init-height diff-y)
						       (- (+ init-y init-height) (y screen)))))
	  (:right-resize (moveresize-window target-window
					    :width (mid min-width (+ init-width diff-x) (- max-x init-x))))
	  (:se-resize (moveresize-window target-window
					 :width (mid min-width (+ init-width diff-x) (- max-x init-x))
					 :height (mid min-height (+ init-height diff-y) (- max-y init-y))))
	  (:bottom-resize (moveresize-window target-window
					     :height (mid min-height (+ init-height diff-y) (- max-y init-y))))
	  (:sw-resize (moveresize-window target-window
					 :x new-x
					 :width (mid min-width
						     (- init-width diff-x)
						     (- (+ init-x init-width) (x screen)))
					 :height (mid min-height (+ init-height diff-y) (- max-y init-y))))
	  (:left-resize (moveresize-window target-window
					   :x new-x
					   :width (mid min-width
						     (- init-width diff-x)
						     (- (+ init-x init-width) (x screen))))))
	(flush-display (display screen)))))))

(defmethod grab-key ((window window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xmaster window) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod ungrab-key ((window window) keycode &key (modifiers 0))
  (xlib:ungrab-key (xmaster window) keycode :modifiers modifiers))

(defmethod grab-keyboard ((window window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xmaster window) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defun get-wm-state (window)
  (car (xlib:get-property (xwindow window) :WM_STATE)))

(defun set-wm-state (xwindow state)
  (xlib:change-property xwindow :WM_STATE (list state) :WM_STATE 32))

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
							   (:frame 11))) :__GOLLUM_INTERNAL 32))

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
      (11 :frame)
      (t nil))))

(defun translate-coordinates (src src-x src-y dst)
  (xlib:translate-coordinates (xmaster src) src-x src-y (xmaster dst)))

(defvar *default-window-border* "blue")

(defvar *default-window-border-width* 2)
(in-package :gollum)

(defclass window-base ()
  ((id :initarg :id			;cache of xwindow's id, in case xwindow destroyed
       :accessor id
       :initform nil)
   (xwindow :initarg :xwindow		;a xlib:window instance
	    :accessor xwindow
	    :initform nil)
   (screen :initarg :screen
	   :accessor screen
	   :initform nil)
   (display :initarg :display
	    :accessor display
	    :initform nil)
   (parent :initarg :parent		;this slot seems no much use
	   :accessor parent
	   :initform nil)
   (map-state :initarg :map-state
	      :accessor map-state
	      :initform :unmapped)))

(defclass root-window (window-base)
  ())

(defclass transient-window (window-base)
  ((xmaster :initarg :xmaster
	    :accessor xmaster
	    :initform nil)
   (protocols :initarg :protocols
	      :accessor protocols
	      :initform nil)
   (main-window :initarg :main-window
		:accessor main-window
		:initform nil)))

(defclass toplevel-window (window-base)
  ((xmaster :initarg :xmaster		;prepare for reparenting
	    :accessor xmaster
	    :initform nil)
   (workspace :initarg :workspace	;a workspace instance
	      :accessor workspace
	      :initform nil)
   (last-x :initarg :last-x
	   :accessor last-x
	   :initform 0)
   (last-y :initarg :last-y
	   :accessor last-y
	   :initform 0)
   (last-width :initarg :last-width
	       :accessor last-width
	       :initform 1)
   (last-height :initarg :last-height
		:accessor last-height
		:initform 1)
   (min-width :initarg :min-width
	      :accessor min-width
	      :initform 1)
   (min-height :initarg :min-height
	       :accessor min-height
	       :initform 1)
   (decorate :initarg :decorate
	     :accessor decorate
	     :initform nil)
   (wm-instance :initarg :wm-instance	;icccm
		:accessor wm-instance
		:initform nil)
   (wm-class :initarg :wm-class		;icccm
	     :accessor wm-class
	     :initform nil)
   (protocols :initarg :protocols	;icccm
	      :accessor protocols
	      :initform nil)
   (transient :initarg :transient	;icccm
	      :accessor transient
	      :initform nil)
   (group :initarg :group
	  :accessor group
	  :initform nil)))

(defgeneric map-workspace-window (window)
  (:documentation "make a window viewable in current workspace,if it is not explicitly unmapped by user"))

(defgeneric map-window (window)
  (:documentation "make a window viewable"))

(defgeneric unmap-workspace-window (window)
  (:documentation "make a window unviewable in current workspace"))

(defgeneric unmap-window (window)
  (:documentation "make a window unviewable"))

(defgeneric window-equal (w1 w2)
  (:documentation "return T if w1 and w2 refer to the same window"))

(defgeneric raise-window (window)
  (:documentation "put the window on top of other windows"))

(defgeneric circulate-window-down (window))

(defgeneric circulate-window-up (window))

(defgeneric set-input-focus (focus &optional revert-to))

(defgeneric match-window (window &key instance class name)
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

(defgeneric grab-key (window keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-key (window keycode &key modifiers))

(defgeneric grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p))

(defgeneric ungrab-keyboard (display &key time))

(defmethod print-object ((obj toplevel-window) stream)
  (format stream "#S(~a ~s #x~x)" (type-of obj) (wm-name obj) (id obj)))

(defun master-id (window)
  (declare (type (or toplevel-window transient-window) window))
  (xlib:window-id (xmaster window)))

(defmethod x ((window toplevel-window))
  (xlib:drawable-x (xmaster window)))

(defmethod (setf x) (new-x (window toplevel-window))
  (setf (xlib:drawable-x (xmaster window)) new-x))

(defmethod x ((window transient-window))
  (xlib:drawable-x (xmaster window)))

(defmethod (setf x) (new-x (window transient-window))
  (setf (xlib:drawable-x (xmaster window)) new-x))

(defmethod y ((window toplevel-window))
  (xlib:drawable-y (xmaster window)))

(defmethod (setf y) (new-y (window toplevel-window))
  (setf (xlib:drawable-y (xmaster window)) new-y))

(defmethod y ((window transient-window))
  (xlib:drawable-y (xmaster window)))

(defmethod (setf y) (new-y (window transient-window))
  (setf (xlib:drawable-y (xmaster window)) new-y))

(defmethod width ((window toplevel-window))
  (xlib:drawable-width (xmaster window)))

(defmethod (setf width) (new-width (window toplevel-window))
  (setf (xlib:drawable-width (xmaster window)) new-width))

(defmethod width ((window transient-window))
  (xlib:drawable-width (xmaster window)))

(defmethod (setf width) (new-width (window transient-window))
  (setf (xlib:drawable-width (xmaster window)) new-width))

(defmethod height ((window toplevel-window))
  (xlib:drawable-height (xmaster window)))

(defmethod (setf height) (new-height (window toplevel-window))
  (setf (xlib:drawable-height (xmaster window)) new-height))

(defmethod height ((window transient-window))
  (xlib:drawable-height (xmaster window)))

(defmethod (setf height) (new-height (window transient-window))
  (setf (xlib:drawable-height (xmaster window)) new-height))

(defmethod workspace ((window transient-window))
  (workspace (main-window window)))

(defmethod mapped ((window toplevel-window))
  (eql (xlib:window-map-state (xmaster window)) :viewable))

(defmethod not-mapped ((window toplevel-window))
  (eql (xlib:window-map-state (xmaster window)) :unmapped))

(defmethod should-be-mapped ((window toplevel-window))
  (member (get-wm-state window) '(:normal :iconic)))

(defmethod get-window-name ((window toplevel-window))
  (let ((netwm-name (net-wm-name window)))
    (if netwm-name
	netwm-name
	(wm-name window))))

(defmethod map-window ((window toplevel-window))
  (when (not-mapped window)
    (xlib:map-window (xmaster window))))

(defmethod map-workspace-window ((window toplevel-window))
  (when (should-be-mapped window)
    (map-window window)))

(defmethod unmap-window ((window toplevel-window))
  (when (mapped window)
    (xlib:unmap-window (xmaster window))))

(defmethod unmap-workspace-window ((window toplevel-window))
  (unmap-window window))

(defmethod window-equal ((w1 window-base) (w2 window-base))
  (= (id w1) (id w2)))

(defmethod raise-window ((window toplevel-window))
  (setf (xlib:window-priority (xmaster window)) :top-if))

(defmethod circulate-window-down ((window toplevel-window))
  (xlib:circulate-window-down (xmaster window)))

(defmethod circulate-window-down ((window root-window))
  (xlib:circulate-window-down (xwindow window)))

(defmethod circulate-window-up ((window toplevel-window))
  (xlib:circulate-window-up (xmaster window)))

(defmethod circulate-window-up ((window root-window))
  (xlib:circulate-window-up (xwindow window)))

(defun kill-window (window)
  (let ((display (display window)))
    (if (find :wm_delete_window (protocols window))
	(send-client-message window :wm_protocols :wm_delete_window)
	(xlib:kill-client (xdisplay display) (id window)))))

(defun kill ()
  (if (current-window)
      (kill-window (current-window nil))))

(defmethod match-window ((window toplevel-window) &key instance class name)
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

(defmethod maximize-window ((window toplevel-window))
  (let* ((screen (screen window))
	 (max-width (width screen))
	 (max-height (height screen))
	 (xmaster (xmaster window))
	 (xwindow (xwindow window))
	 (double-border (* 2 *default-window-border-width*))
	 (title-height (title-height (decorate window))))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) (x screen)
	    (xlib:drawable-y xmaster) (y screen)
	    (xlib:drawable-width xmaster) max-width
	    (xlib:drawable-height xmaster) max-height))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) (- max-width double-border)
	    (xlib:drawable-height xwindow) (- max-height double-border title-height)))
    (update-decorate (decorate window))
;    (setf (size-state window) :maximized)
    (flush-display (display window))))

;; (defun maximized (window)
;;   (eql (size-state window) :maximized))

(defun maximize ()
  (if (current-window)
      (maximize-window (current-window))))

(defmethod restore-window ((window toplevel-window))
  (let* ((x (last-x window))
	 (y (last-y window))
	 (width (last-width window))
	 (height (last-height window))
	 (xmaster (xmaster window))
	 (xwindow (xwindow window))
	 (double-border (* 2 *default-window-border-width*))
	 (title-height (title-height (decorate window))))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) x
	    (xlib:drawable-y xmaster) y
	    (xlib:drawable-width xmaster) width
	    (xlib:drawable-height xmaster) height))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) (- width double-border)
	    (xlib:drawable-height xwindow) (- height double-border title-height)))
    (update-decorate (decorate window))
    (flush-display (display window))))

(defun restore ()
  (if (current-window)
      (restore-window (current-window))))

(defmethod moveresize-window ((window toplevel-window) &key x y width height)
  (let ((xmaster (xmaster window)))
    (xlib:with-state (xmaster)
      (if x (setf (xlib:drawable-x xmaster) x))
      (if y (setf (xlib:drawable-y xmaster) y))
      (if width (setf (xlib:drawable-width xmaster) width))
      (if height (setf (xlib:drawable-height xmaster) height)))
    (if x (setf (last-x window) x))
    (if y (setf (last-y window) y))
    (if width (setf (last-width window) width))
    (if height (setf (last-height window) height))
    (when (or width height)
      (let ((xwindow (xwindow window))
	    (double-border (* 2 *default-window-border-width*))
	    (title-height (title-height (decorate window))))
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

(defun send-client-message (window type &rest data)
  (xlib:send-event (xwindow window) :client-message nil
		   :window (xwindow window)
		   :type type
		   :format 32
		   :data (mapcar (lambda (x)
				   (if (keywordp x)
				       (xlib:intern-atom (xdisplay (display window)) x)
				       x)) data)))


(defmethod grab-key ((window toplevel-window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xmaster window) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod grab-key ((window root-window) keycode &key modifiers owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-key (xwindow window) keycode :modifiers modifiers :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod ungrab-key ((window toplevel-window) keycode &key (modifiers 0))
  (xlib:ungrab-key (xmaster window) keycode :modifiers modifiers))

(defmethod ungrab-key ((window root-window) keycode &key (modifiers 0))
  (xlib:ungrab-key (xwindow window) keycode :modifiers modifiers))

(defmethod grab-keyboard ((window toplevel-window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xmaster window) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defmethod grab-keyboard ((window root-window) &key owner-p sync-pointer-p sync-keyboard-p)
  (xlib:grab-keyboard (xwindow window) :owner-p owner-p :sync-pointer-p sync-pointer-p :sync-keyboard-p sync-keyboard-p))

(defun translate-coordinates (src src-x src-y dst)
  (xlib:translate-coordinates (xmaster src) src-x src-y (xmaster dst)))

(defvar *default-window-border* "blue")

(defvar *default-window-border-width* 2)

;; icccm

(defun wm-name (window)
  (declare (type (or toplevel-window transient-window) window))
  (xlib:wm-name (xwindow window)))

;; netwm

(defun net-wm-name (window)
  (declare (type (or toplevel-window transient-window) window))
  (get-net-wm-name (xwindow window)))

(defun net-wm-icon-name (window)
  (declare (type (or toplevel-window transient-window) window))
  (get-net-wm-icon-name (xwindow window)))

(defun net-wm-desktop (window)
  (declare (type (or toplevel-window transient-window) window))
  (get-net-wm-desktop (xwindow window)))

(defun (setf net-wm-desktop) (id window)
  (declare (type (or toplevel-window transient-window) window))
  (set-net-wm-desktop (xwindow window) id))

;; Window managers may examine the property(WM_CLASS) only when they start up and when the window leaves the Withdrawn state, but there should be no need for a client to change its state dynamically.

(defmethod wm-normal-hints ((window toplevel-window))
  (xlib:wm-normal-hints (xwindow window)))

(defmethod wm-hints ((window toplevel-window))
  (xlib:wm-hints (xwindow window)))

(defun get-wm-state-1 (xwindow)
  (let ((state-value (xlib:get-property xwindow :WM_STATE)))
    (values (case (car state-value)
	      (0 :withdrawn)
	      (1 :normal)
	      (3 :iconic)
	      (t nil)) (cadr state-value))))

(defmethod get-wm-state ((window toplevel-window))
  (get-wm-state-1 (xwindow window)))

(defun set-wm-state-1 (xwindow state &optional id)
  (let ((state-value (case state
		       (:withdrawn 0)
		       (:normal 1)
		       (:iconic 3))))
    (xlib:change-property xwindow :WM_STATE (if id (list state-value id) (list state-value)) :WM_STATE 32)))

(defmethod set-wm-state ((window toplevel-window) state &optional id)
  (set-wm-state-1 (xwindow window) state id))

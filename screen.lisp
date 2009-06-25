(in-package :gollum)

(defclass screen ()
  ((id :initarg :id
       :accessor id
       :initform nil)
   (xscreen :initarg :xscreen
	    :accessor xscreen
	    :initform nil)
   (display :initarg :display
	    :accessor display
	    :initform nil)
   (height :initarg :height
	   :accessor height
	   :initform nil)
   (width :initarg :width
	  :accessor width
	  :initform nil)
   (x :initarg :x
      :accessor x
      :initform 0)
   (y :initarg :y
      :accessor y
      :initform 0)
   (mode-line :initarg :mode-line
	      :accessor mode-line
	      :initform nil)
   (output-gc :initarg :output-gc
	       :accessor output-gc
	       :initform nil)
   (output-font :initarg :output-font
		 :accessor output-font
		 :initform nil)
   (message-window :initarg :message-window
		   :accessor message-window
		   :initform nil)
   (message-timer :initarg :message-timer
		  :accessor message-timer
		  :initform nil)
   (key-prompt-window :initarg :key-prompt-window
		      :accessor key-prompt-window
		      :initform nil)
   (key-prompt-timer :initarg :key-prompt-timer
		     :accessor key-prompt-timer
		     :initform nil)
   (input-gc :initarg :input-gc
	     :accessor input-gc
	     :initform nil)
   (input-font :initarg :input-font
	       :accessor input-font
	       :initform nil)
   (input-window :initarg :input-window
		 :accessor input-window
		 :initform nil)
   (input-buffer :initarg :input-buffer
		 :accessor input-buffer
		 :initform nil)
   (input-buffer-lock :initarg :input-buffer-lock
		      :accessor input-buffer-lock
		      :initform (bordeaux-threads:make-recursive-lock "input-buffer-lock"))
   (input-cv :initarg :input-cv
	     :accessor input-cv
	     :initform (bordeaux-threads:make-condition-variable))
   (configure-gc :initarg :configure-gc
		 :accessor configure-gc
		 :initform nil)
   (root :initarg :root
	 :accessor root
	 :initform nil)
   (windows :initarg :windows
	    :accessor windows
	    :initform (make-hash-table))
   (workspaces :initarg :workspaces
	       :accessor workspaces
	       :initform (make-hash-table))
   (current-workspace :initarg :current-workspace
		      :accessor current-workspace
		      :initform nil)
   (rules :initarg :rules
	  :accessor rules
	  :initform nil)))

(defvar *background-color* "black")
(defvar *foreground-color* "white")
(defvar *output-font* "-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-0-75-75-p-0-gbk-0")
(defvar *input-font* "-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-0-75-75-p-0-gbk-0")
(defvar *internal-window-border-width* 1)
(defvar *internal-window-border* "white")
(defvar *internal-window-vertical-padding* 1)
(defvar *internal-window-horizontal-padding* 2)

(defgeneric make-internal-window (s))

(defgeneric manage-screen-root (screen))

(defgeneric manage-existing-windows (s))

(defgeneric set-current-workspace (ws s)
  (:documentation "set a workspace as current workspace"))

(defgeneric switch-to-workspace (ws s)
  (:documentation "as is"))

(defgeneric raise-workspace-window (win s)
  (:documentation "raise the window,switch to the window's workspace if needed"))

(defgeneric add-workspace-to-screen (name s))

(defgeneric delete-workspace-from-screen (ws s))

(defgeneric xwindow-window (xwin obj))

(defgeneric screen-message (screen message &optional time-out-p))

(defgeneric screen-input (screen prompt))

(defgeneric add-workspaces-according-to-layout (screen))

(defgeneric list-workspaces (screen))

(defgeneric init-screen (screen))

(defmethod make-internal-window ((s screen))
  (xlib:create-window :parent (xwindow (root s))
		      :x 0 :y 0 :width 1 :height 1
		      :override-redirect :on
		      :save-under :on))

(defmethod set-current-workspace ((ws workspace) (s screen))
  (setf (current-workspace s) ws))

(defmethod switch-to-workspace ((workspace workspace) (screen screen))
  (unless (workspace-equal (current-workspace screen) workspace)
    (unmap-workspace (current-workspace screen))
    (map-workspace workspace)
    (set-current-workspace workspace screen)
    (flush-display (display screen))))

(defmethod raise-workspace-window ((win window) (s screen))
  (let ((ws (workspace win)))
    (unless (workspace-equal ws (current-workspace s))
      (switch-to-workspace ws s))
    (raise-window win)))

(defmethod add-window ((win window) (obj screen))
  (setf (gethash (id win) (windows obj)) win
	(screen win) obj)
  (place-window win))

(defmethod delete-window ((win window) (obj screen))
  (let ((id (id win))
	(ws (workspace win)))
    (when ws
      (delete-window win ws))
    (multiple-value-bind (w exist-p) (gethash id (windows obj))
      (declare (ignore w))
      (when exist-p
	(remhash id (windows obj))))))

(defmethod add-workspace-to-screen (name (screen screen))
  (or (find-workspace-by-name name (workspaces screen))
      (let* ((new-id (next-workspace-id (workspaces screen)))
	     (workspace (make-instance 'workspace :name name :id new-id)))
	(setf (gethash new-id (workspaces screen)) workspace
	      (screen workspace) screen))))

;; need to process all the windows in workspace
(defmethod delete-workspace-from-screen ((ws workspace) (s screen))
  (let ((nworkspace (get-nworkspace (workspaces s))))
    (when (> nworkspace 1)
      (let* ((id (id ws))
	     (id-1 (if (= id 1) 2 (1- id))))
	(dolist (win (windows ws))
	  (move-window-to-workspace win (find-workspace-by-id id-1))))
      (remhash (id ws) (workspaces s)))))

(defmethod xwindow-window (xwin (obj screen))
  "XWIN is an xlib window,we find the corresponding window."
  (when xwin
    (multiple-value-bind (win exist-p) (gethash (xlib:window-id xwin) (windows obj))
      (and exist-p (xlib:window-equal xwin (xwindow win)) win))))

(defun max-or (num1 num2)
  (if (and num1 num2)
      (max num1 num2)
      (or num1 num2)))

(defun min-or (num1 num2)
  (if (and num1 num2)
      (min num1 num2)
      (or num1 num2)))

(defun update-screen-window-geometry (window screen)
  (let* ((xwindow (xwindow window))
	 (old-x (or (orig-x window) (xlib:drawable-x xwindow)))
	 (old-y (or (orig-y window) (xlib:drawable-y xwindow)))
	 (old-width (or (orig-width window) (xlib:drawable-width xwindow)))
	 (old-height (or (orig-height window) (xlib:drawable-height xwindow)))
	 (new-x (max old-x (x screen)))
	 (new-y (max old-y (y screen)))
	 (new-width (min old-width (width screen)))
	 (new-height (min old-height (height screen))))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-x xwindow) new-x
	    (xlib:drawable-y xwindow) new-y
	    (xlib:drawable-width xwindow) new-width
	    (xlib:drawable-height xwindow) new-height))
    (setf (orig-x window) new-x
	  (orig-y window) new-y
	  (orig-width window) new-width
	  (orig-height window) new-height)))

;; this is lowerlevel function
(defun manage-new-window (xwindow xparent screen)
  (multiple-value-bind (wm-instance wm-class) (xlib:get-wm-class xwindow)
    (let* ((map-state (xlib:window-map-state xwindow))
	   (id (xlib:window-id xwindow))
	   (win (make-instance 'window :id id :xwindow xwindow :map-state map-state :ws-map-state map-state
			       :orig-width (width screen) :orig-height (height screen)))
	   (pwin (xwindow-window xparent screen)) ;FIXME:what if we can not find the parent?
	   (normal-hints (xlib:wm-normal-hints xwindow)))
      (when (window-equal pwin (root screen))
	(unless (eql (xlib:window-class xwindow) :input-only)
	  (setf (xlib:window-border xwindow) (alloc-color *default-window-border* screen)
		(xlib:drawable-border-width xwindow) *default-window-border-width*))
	(setf (toplevel-p win) t
	      (xlib:window-event-mask xwindow) '(:focus-change
						 :button-motion
						 :enter-window
						 :structure-notify))
	(setf (parent win) pwin
	      (wm-name win) (xlib:wm-name xwindow)
	      (wm-instance win) wm-instance
	      (wm-class win) wm-class
	      (protocols win) (xlib:wm-protocols xwindow)
	      (orig-x win) (xlib:wm-size-hints-x normal-hints)
	      (orig-y win) (xlib:wm-size-hints-y normal-hints)
	      (orig-width win) (xlib:wm-size-hints-width normal-hints)
	      (orig-height win) (xlib:wm-size-hints-height normal-hints))
	(add-window win (display screen))
	(add-window win screen)
	(update-screen-window-geometry win screen)))))

(defmethod manage-screen-root ((screen screen))
  (let* ((xroot (xlib:screen-root (xscreen screen)))
	 (root-id (xlib:window-id xroot))
	 (root (make-instance 'window
			      :id root-id
			      :xwindow xroot
			      :screen screen
			      :display (display screen)
			      :map-state :viewable
			      :ws-map-state :viewable
			      :wm-name "ROOT"
			      :wm-class "ROOT")))
    (setf (gethash root-id (windows screen)) root
	  (gethash root-id (windows (display screen))) root
	  (root screen) root
	  (xlib:window-event-mask (xwindow (root screen))) '(:focus-change
							     :button-motion
							     :substructure-notify
							     :substructure-redirect))))

(defmethod manage-existing-windows ((screen screen))
  (let* ((xroot (xlib:screen-root (xscreen screen)))
	 (window-list (xlib:query-tree xroot)))
    (dolist (win window-list)
      (unless (eql :on (xlib:window-override-redirect win))
	(set-wm-state win (case (xlib:window-map-state win) (:unmapped 0) (:viewable 1)))
	(manage-new-window win xroot screen)))))

(defun calculate-geometry (screen xwindow gravity)
  (let* ((height (height screen))
	 (width (width screen))
	 (window-height (xlib:drawable-height xwindow))
	 (window-width (xlib:drawable-width xwindow))
	 (double-border (* 2 *internal-window-border-width*)))
    (case gravity
      (:center (setf (xlib:drawable-x xwindow) (floor (/ (- width window-width) 2))
		     (xlib:drawable-y xwindow) (floor (/ (- height window-height) 2))))
      (:bottom-left (setf (xlib:drawable-x xwindow) 0
			  (xlib:drawable-y xwindow) (- height window-height double-border)))
      (:bottom-center (setf (xlib:drawable-x xwindow) (floor (/ (- width window-width double-border) 2))
			    (xlib:drawable-y xwindow) (- height window-height double-border)))
      (:bottom-right (setf (xlib:drawable-x xwindow) (- width window-width double-border)
			   (xlib:drawable-y xwindow) (- height window-height double-border)))
      (:top-left (setf (xlib:drawable-x xwindow) 0
		       (xlib:drawable-y xwindow) 0))
      (:top-center (setf (xlib:drawable-x xwindow) (floor (/ (- width window-width double-border) 2))
			 (xlib:drawable-y xwindow) 0))
      (:top-right (setf (xlib:drawable-x xwindow) (- width window-width double-border)
			(xlib:drawable-y xwindow) 0))
      (:pointer (multiple-value-bind (x y) (xlib:query-pointer (xwindow (root screen)))
		  (setf (xlib:drawable-x xwindow) x
			(xlib:drawable-y xwindow) y))))))

(defun setup-window-for-drawing-glyphs (screen xwindow gravity gcontext font content)
  "CONTENT is a list of string indicating multi-line drawing"
  (let* ((descent (xlib:font-descent font))
	 (ascent (xlib:font-ascent font))
	 (height (* (length content) (+ ascent descent)))
	 (width (apply #'max (mapcar (lambda (line)
				       (xlib:text-width font line :translate #'translate-id)) content))))
    (xlib:with-state (xwindow)
      (if (eql (xlib:window-map-state xwindow) :unmapped)
	  (xlib:map-window xwindow))
      (setf (xlib:drawable-height xwindow) (+ (* 2 *internal-window-vertical-padding*) height)
	    (xlib:drawable-width xwindow) (+ (* 2 *internal-window-horizontal-padding*) width)
	    (xlib:window-priority xwindow) :top-if)
      (calculate-geometry screen xwindow gravity))
    (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
      (xlib:draw-rectangle xwindow gcontext 0 0 (xlib:drawable-width xwindow) (xlib:drawable-height xwindow) t))))

(defmethod add-workspaces-according-to-layout ((screen screen))
  (if *workspace-layout*
      (dolist (name *workspace-layout*)
	(add-workspace-to-screen name screen))
      (add-workspace-to-screen "default" screen)))

(defmethod list-windows ((obj screen))
  (let ((windows nil))
    (maphash (lambda (id window)
	       (declare (ignore id))
	       (setf windows (append windows (list window)))) (windows obj))
    windows))

(defmethod list-workspaces ((screen screen))
  (let ((workspaces nil))
    (maphash (lambda (id workspace)
	       (setf workspaces (append workspaces (list (list id workspace))))) (workspaces screen))
    workspaces))

(defun create-gcontext (xwin bg fg font)
  (xlib:create-gcontext :drawable xwin
			:background bg
			:foreground fg
			:font font))

;; we are in bottom-half,i.e. after the rc file loaded
(defmethod init-screen ((screen screen))
  (add-workspaces-according-to-layout screen)
  (set-current-workspace (find-workspace-by-id 1 (workspaces screen)) screen)
  (manage-existing-windows screen)
  (setf (mode-line screen) (make-internal-window screen)
	(output-font screen) (open-font (display screen) *output-font*)
	(message-window screen) (make-internal-window screen)
	(xlib:drawable-border-width (message-window screen)) *internal-window-border-width*
	(xlib:window-border (message-window screen)) (alloc-color *internal-window-border* screen)
	(output-gc screen) (create-gcontext (message-window screen)
					     (alloc-color *background-color* screen)
					     (alloc-color *foreground-color* screen)
					     (output-font screen))
	(message-timer screen) (make-instance 'message-timer
					      :action 'hide-screen-message
					      :screen screen)
	(key-prompt-timer screen) (make-instance 'message-timer
						 :action 'hide-key-prompt
						 :screen screen)
	(key-prompt-window screen) (make-internal-window screen)
	(xlib:drawable-border-width (key-prompt-window screen)) *internal-window-border-width*
	(xlib:window-border (key-prompt-window screen)) (alloc-color *internal-window-border* screen)
	(input-font screen) (open-font (display screen) *input-font*)
	(input-window screen) (make-internal-window screen)
	(xlib:drawable-border-width (input-window screen)) *internal-window-border-width*
	(xlib:window-border (input-window screen)) (alloc-color *internal-window-border* screen)
	(input-gc screen) (create-gcontext (input-window screen)
					   (alloc-color *background-color* screen)
					   (alloc-color *foreground-color* screen)
					   (input-font screen))
	(configure-gc screen) (xlib:create-gcontext :drawable (xwindow (root screen))
						    :font (output-font screen)
						    :foreground (alloc-color *foreground-color* screen)
						    :background (alloc-color *background-color* screen)
						    :line-style :dash
						    :line-width 1
						    :subwindow-mode :clip-by-children))
  (xlib:grab-button (xwindow (root screen)) 1 '(:button-motion :button-release) ;FIXME:should be customizable
		    :modifiers (list (key->mod :alt (key-mod-map (display screen))))
		    :owner-p nil
		    :sync-pointer-p nil
		    :sync-keyboard-p nil))
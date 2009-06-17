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
   (message-gc :initarg :message-gc
	       :accessor message-gc
	       :initform nil)
   (message-font :initarg :message-font
		 :accessor message-font
		 :initform nil)
   (message-window :initarg :message-window
		   :accessor message-window
		   :initform nil)
   (message-timer :initarg :message-timer
		  :accessor message-timer
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
		      :override-redirect :on))

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

(defun update-screen-window-geometry (window screen)
  (let* ((xwindow (xwindow window))
	 (old-x (xlib:drawable-x xwindow))
	 (old-y (xlib:drawable-y xwindow))
	 (old-width (xlib:drawable-width xwindow))
	 (old-height (xlib:drawable-height xwindow))
	 (new-x (max old-x (x screen)))
	 (new-y (max old-y (y screen)))
	 (new-width (min old-width (width screen)))
	 (new-height (min old-height (height screen))))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-x xwindow) new-x
	    (xlib:drawable-y xwindow) new-y
	    (xlib:drawable-width xwindow) new-width
	    (xlib:drawable-height xwindow) new-height))
    (setf (orig-x window) (max new-x (orig-x window))
	  (orig-y window) (max new-y (orig-y window))
	  (orig-width window) (min new-width (orig-width window))
	  (orig-height window) (min new-height (orig-height window)))))

;; this is lowerlevel function
(defun manage-new-window (xwindow xparent screen)
  (multiple-value-bind (ignore-name wm-class) (xlib:get-wm-class xwindow)
    (declare (ignore ignore-name))
    (let* ((map-state (xlib:window-map-state xwindow))
	   (id (xlib:window-id xwindow))
	   (win (make-instance 'window :id id :xwindow xwindow :map-state map-state :ws-map-state map-state
			       :orig-width (width screen) :orig-height (height screen)))
	   (pwin (xwindow-window xparent screen))) ;FIXME:what if we can not find the parent?
      (setf (xlib:window-event-mask xwindow) '(:focus-change :substructure-notify :substructure-redirect))
      (setf (parent win) pwin
	    (wm-name win) (xlib:wm-name xwindow)
	    (wm-class win) wm-class)
      (add-window win (display screen))
      (add-window win screen)
      (update-screen-window-geometry win screen))))

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
	 (window-width (xlib:drawable-width xwindow)))
    (case gravity
      (:bottom-center (setf (xlib:drawable-x xwindow) (floor (/ (- width window-width (* 2 *internal-window-border-width*)) 2))
			    (xlib:drawable-y xwindow) (- height window-height (* 2 *internal-window-border-width*))))
      (:center (setf (xlib:drawable-x xwindow) (floor (/ (- width window-width) 2))
		     (xlib:drawable-y xwindow) (floor (/ (- height window-height) 2)))))))

(defun setup-window-for-drawing-glyphs (screen xwindow gravity gcontext font content)
  (let* ((height (+ (xlib:font-descent font) (xlib:font-ascent font)))
	 (width (xlib:text-width font content)))
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
	(message-font screen) (open-font (display screenm) *output-font*)
	(message-window screen) (make-internal-window screen)
	(xlib:drawable-border-width (message-window screen)) *internal-window-border-width*
	(xlib:window-border (message-window screen)) (alloc-color *internal-window-border* screen)
	(message-gc screen) (create-gcontext (message-window screen)
					     (alloc-color *background-color* screen)
					     (alloc-color *foreground-color* screen)
					     (message-font screen))
	(message-timer screen) (make-instance 'message-timer
					      :action #'hide-screen-message
					      :screen screen)
	(input-font screen) (open-font (display screen) *input-font*)
	(input-window screen) (make-internal-window screen)
	(xlib:drawable-border-width (input-window screen)) *internal-window-border-width*
	(xlib:window-border (input-window screen)) (alloc-color *internal-window-border* screen)
	(input-gc screen) (create-gcontext (input-window screen)
					   (alloc-color *background-color* screen)
					   (alloc-color *foreground-color* screen)
					   (input-font screen))))
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
   (message-gc :initarg :message-gc
	       :accessor message-gc
	       :initform nil)
   (message-font :initarg :message-font
		 :accessor message-font
		 :initform nil)
   (message-window :initarg :message-window
		   :accessor message-window
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
(defvar *internal-window-border "white")

(defgeneric make-internel-window (s))

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

(defgeneric screen-message (screen message))

(defgeneric add-workspaces-according-to-layout (screen))

(defgeneric init-screen (screen))

(defmethod make-internel-window ((s screen))
  (xlib:create-window :parent (xwindow (root s))
		      :x 0 :y 0 :width 1 :height 1
		      :override-redirect :on))

(defmethod set-current-workspace ((ws workspace) (s screen))
  (setf (current-workspace s) ws))

(defmethod switch-to-workspace ((ws workspace) (s screen))
  (unmap-workspace (current-workspace s))
  (map-workspace ws)
  (set-current-workspace ws s))

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

(defmethod add-workspace-to-screen (name (s screen))
  (or (find-workspace-by-name name (workspaces s))
      (let* ((new-id (next-workspace-id (workspaces s)))
	     (w (make-instance 'workspace :name name :id new-id)))
	(setf (gethash new-id (workspaces s)) w
	      (screen w) s)
	(unless (current-workspace s)
	  (setf (current-workspace s) w)))))

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

;; this is lowerlevel function
(defun manage-new-window (xwindow xparent screen)
  (multiple-value-bind (ignore-name wm-class) (xlib:get-wm-class xwindow)
    (declare (ignore ignore-name))
    (let* ((map-state (xlib:window-map-state xwindow))
	   (id (xlib:window-id xwindow))
	   (win (make-instance 'window :id id :xwindow xwindow :map-state map-state :ws-map-state map-state))
	   (pwin (xwindow-window xparent screen))) ;FIXME:what if we can not find the parent?
      (setf (parent win) pwin
	    (win-name win) (xlib:wm-name xwindow)
	    (win-class win) wm-class)
      (add-window win (display screen))
      (add-window win screen))))

(defmethod manage-existing-windows ((s screen))
  (let* ((xroot (xlib:screen-root (xscreen s)))
	 (window-list (xlib:query-tree xroot))
	 (root-id (xlib:window-id xroot))
	 (root (make-instance 'window
			      :id root-id
			      :xwindow xroot
			      :screen s
			      :display (display s)
			      :map-state :viewable
			      :ws-map-state :viewable)))
    (setf (gethash root-id (windows s)) root	;manage the root window seperately
	  (gethash root-id (windows (display s))) root
	  (root s) root)
    (dolist (win window-list)
      (unless (eql :on (xlib:window-override-redirect win))
	(manage-new-window win xroot s)))))

(defmethod screen-message ((screen screen) message)
  (let* ((font (message-font screen))
	 (height (+ (xlib:font-descent font) (xlib:font-ascent font)))
	 (width (xlib:text-width font message))
	 (message-window (message-window screen))
	 (message-gc (message-gc screen)))
    (xlib:with-state (message-window)
      (if (eql (xlib:window-map-state message-window) :unmapped)
	  (xlib:map-window message-window))
      (setf (xlib:drawable-height message-window) (+ 2 height)
	    (xlib:drawable-width message-window) (+ 2 width)
	    (xlib:window-priority message-window) :above)
      (xlib:clear-area message-window)
      (xlib:draw-image-glyphs message-window message-gc 1 (xlib:font-ascent font) message :width width))))

(defmethod add-workspaces-according-to-layout ((screen screen))
  (if *workspace-layout*
      (dolist (name *workspace-layout*)
	(add-workspace-to-screen name screen))
      (add-workspace-to-screen "default" screen)))

(defun create-gcontext (xwin bg fg font)
  (xlib:create-gcontext :drawable xwin
			:background bg
			:foreground fg
			:font font))

(defmethod init-screen ((screen screen))
  (add-workspaces-according-to-layout screen)
  (manage-existing-windows screen)
  (setf (xlib:window-event-mask (xwindow (root screen))) '(:key-press
							   :key-release
							   :substructure-notify
							   :substructure-redirect)
	(message-font screen) (open-font (display screen) *output-font*)
	(message-window screen) (make-internel-window screen)
	(message-gc screen) (create-gcontext (message-window screen)
				     (alloc-color *background-color* screen)
				     (alloc-color *foreground-color* screen)
				     (message-font screen))))

  


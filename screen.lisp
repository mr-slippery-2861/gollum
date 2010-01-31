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
   (mode-line-children :initarg :mode-line-children
		       :accessor mode-line-children
		       :initform (make-hash-table))
   (mode-line-gc :initarg :mode-line-gc
		 :accessor mode-line-gc
		 :initform nil)
   (mode-line-timer :initarg :mode-line-timer
		    :accessor mode-line-timer
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
   (input-history :initarg :input-history
		  :accessor input-history
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
   (mapped-windows :initarg :mapped-windows
		   :accessor mapped-windows
		   :initform (make-hash-table))
   (withdrawn-windows :initarg :withdrawn-windows
		      :accessor withdrawn-windows
		      :initform (make-hash-table))
   (stacking-orderd :initarg :stacking-orderd
		    :accessor stacking-orderd
		    :initform nil)	;this is a list of window-id in stacking-order
   (workspaces :initarg :workspaces
	       :accessor workspaces
	       :initform (make-hash-table))
   (current-workspace :initarg :current-workspace
		      :accessor screen-current-workspace
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

(defgeneric make-internal-window (screen))

(defgeneric manage-screen-root (screen))

(defgeneric manage-existing-windows (screen))

(defgeneric set-current-workspace (workspace screen)
  (:documentation "set a workspace as current workspace"))

(defgeneric switch-to-workspace (workspace screen)
  (:documentation "as is"))

(defgeneric raise-workspace-window (window screen)
  (:documentation "raise the window,switch to the window's workspace if needed"))

(defgeneric add-workspace-to-screen (name screen))

(defgeneric delete-workspace-from-screen (workspace screen))

(defgeneric xwindow-window (xwindow obj))

(defgeneric screen-message (screen message &optional time-out-p))

(defgeneric screen-input (screen prompt))

(defgeneric add-workspaces-according-to-layout (screen))

(defgeneric list-workspaces (screen))

(defgeneric output-to-window (screen xwindow gcontext gravity content))

(defgeneric init-screen (screen))

(defgeneric deinit-screen (screen))

(defun current-workspace (&optional screen)
  (screen-current-workspace (or screen (current-screen))))

(defun (setf current-workspace) (workspace screen)
  (setf (screen-current-workspace screen) workspace))

(defmethod make-internal-window ((screen screen))
  (alloc-xwindow *display*
		 :parent (xwindow (root screen))
		 :x 0 :y 0 :width 1 :height 1
		 :override-redirect :on
		 :save-under :on))

(defmethod set-current-workspace ((workspace workspace) (screen screen))
  (setf (current-workspace screen) workspace))

(defmethod switch-to-workspace ((workspace workspace) (screen screen))
  (unless (workspace-equal (current-workspace screen) workspace)
    (unmap-workspace (current-workspace screen))
    (map-workspace workspace)
    (set-current-workspace workspace screen)
    (workspace-set-focus workspace (current-focus workspace))
    (flush-display (display screen))))

(defmethod raise-workspace-window ((window toplevel-window) (screen screen))
  (let ((workspace (workspace window)))
    (unless (workspace-equal workspace (current-workspace screen))
      (switch-to-workspace workspace screen))
    (raise-window window)))

(defun sort-by-stacking-order (window-list screen)
  (sort window-list #'< :key (lambda (window)
			       (position (id window) (stacking-orderd screen) :test #'=))))

(defmethod add-window ((window toplevel-window) (obj screen))
  (setf (gethash (id window) (mapped-windows obj)) window)
  (setf (screen window) obj)
  (setf (stacking-orderd obj) (append (stacking-orderd obj) (list (id window))))
  (add-window window (window-workspace-according-to-rule window)))

(defmethod add-window ((window transient-window) (obj screen))
  (setf (gethash (id window) (mapped-windows obj)) window)
  (setf (screen window) obj)
  (setf (stacking-orderd obj) (append (stacking-orderd obj) (list (id window)))))

(defmethod add-window ((window xlib:window) (obj screen))
  (setf (gethash (xlib:window-id window) (withdrawn-windows obj)) window))

(defmethod remove-window ((window xlib:window) (obj screen))
  (remhash (xlib:window-id window) (withdrawn-windows obj)))

(defmethod delete-window ((window toplevel-window) (obj screen))
  (let ((id (id window))
	(workspace (workspace window)))
    (when workspace
      (delete-window window workspace))
    (remhash id (mapped-windows obj))
    (remhash id (withdrawn-windows obj))
    (setf (stacking-orderd obj) (remove id (stacking-orderd obj) :test #'=))))

(defmethod find-matching-windows ((obj screen) &key instance class name)
  (remove-if #'null (mapcar (lambda (id)
			      (let ((window (gethash id (mapped-windows obj))))
				(if (match-window window :instance instance :class class :name name)
				    window
				    nil))) (stacking-orderd obj))))
    
(defmethod add-workspace-to-screen (name (screen screen))
  (or (find-workspace-by-name name (workspaces screen))
      (let* ((new-id (next-workspace-id (workspaces screen)))
	     (workspace (make-instance 'workspace :name name :id new-id)))
	(setf (gethash new-id (workspaces screen)) workspace
	      (screen workspace) screen))))

;; need to process all the windows in workspace
(defmethod delete-workspace-from-screen ((workspace workspace) (screen screen))
  (let ((nworkspace (get-nworkspace (workspaces screen))))
    (when (> nworkspace 1)
      (let* ((id (id workspace))
	     (id-1 (if (= id 1) 2 (1- id))))
	(dolist (window (mapped-windows workspace))
	  (move-window-to-workspace window (find-workspace-by-id id-1 (workspaces screen)))))
      (remhash (id workspace) (workspaces screen)))))

(defmethod xwindow-window (xwindow (obj screen))
  "xwin is an xlib window,we find the corresponding window."
  (when xwindow
    (multiple-value-bind (window exist-p) (gethash (xlib:window-id xwindow) (withdrawn-windows obj))
      (and exist-p (xlib:window-equal xwindow (xwindow window)) window))))

(defun max-or (num1 num2)
  (if (and num1 num2)
      (max num1 num2)
      (or num1 num2)))

(defun min-or (num1 num2)
  (if (and num1 num2)
      (min num1 num2)
      (or num1 num2)))

(defun update-screen-window-geometry (window)
  (let* ((xmaster (xmaster window))
	 (xwindow (xwindow window))
	 (screen (screen window))
	 (old-x (x window))
	 (old-y (y window))
	 (old-width (width window))
	 (old-height (height window))
	 (new-x (max old-x (x screen)))
	 (new-y (max old-y (y screen)))
	 (new-width (min old-width (width screen)))
	 (new-height (min old-height (height screen)))
	 (double-border (* 2 *default-window-border-width*))
	 (title-height (title-height (decorate window))))
    (xlib:with-state (xmaster)
      (setf (xlib:drawable-x xmaster) new-x
	    (xlib:drawable-y xmaster) new-y
	    (xlib:drawable-width xmaster) new-width
	    (xlib:drawable-height xmaster) new-height))
    (xlib:with-state (xwindow)
      (setf (xlib:drawable-width xwindow) (- new-width double-border)
	    (xlib:drawable-height xwindow) (- new-height double-border title-height)))
    (setf (last-x window) new-x
	  (last-y window) new-y
	  (last-width window) new-width
	  (last-height window) new-height)))

(defun update-screen-windows-geometry (screen)
  (maphash (lambda (id window)
	     (declare (ignore id))
	     (update-screen-window-geometry window)) (mapped-windows screen)))

(defvar *toplevel-window-event* '(:focus-change
				  :button-motion
				  :button-release
				  :enter-window
				  :substructure-notify
				  :substructure-redirect))

(defun prepare-for-new-window (xwindow)
  (let ((hints (xlib:wm-hints xwindow)))
    (set-wm-state-1 xwindow :withdrawn (xlib:wm-hints-icon-window hints))
    (set-internal-window-type xwindow :toplevel)
    (add-window xwindow *display*)))

(defun normalize-toplevel-window (xwindow)
  (multiple-value-bind (wm-instance wm-class) (xlib:get-wm-class xwindow)
    (let* ((xroot (xlib:drawable-root xwindow))
	   (screen (find-screen xroot))
	   (normal-hints (xlib:wm-normal-hints xwindow)) ;FIXME: respect the hints
	   (min-width (or (xlib:wm-size-hints-min-width normal-hints) (xlib:wm-size-hints-base-width normal-hints) 1))
	   (min-height (or (xlib:wm-size-hints-min-height normal-hints) (xlib:wm-size-hints-base-height normal-hints) 1))
	   (x (xlib:drawable-x xwindow))
	   (y (xlib:drawable-y xwindow))
	   (width (xlib:drawable-width xwindow))
	   (height (xlib:drawable-height xwindow))
	   (double-border (* 2 *default-window-border-width*))
	   (xmaster (alloc-xwindow *display*
				   :parent xroot
				   :event-mask *toplevel-window-event*))
	   (window (make-instance 'toplevel-window
				  :id (xlib:window-id xmaster)
				  :xwindow xwindow
				  :xmaster xmaster
				  :protocols (xlib:wm-protocols xwindow)
				  :wm-instance wm-instance
				  :wm-class wm-class
				  :map-state :mapped))
	   (hints (xlib:wm-hints xwindow))
	   decorate title-height)
      (remove-window xwindow *display*)
      (add-window window *display*)
      (setf decorate (make-decorate window)
	    title-height (title-height decorate)
	    (decorate window) decorate
	    (min-width window) (+ min-width double-border)
	    (min-height window) (+ min-height double-border title-height))
      (set-internal-window-type xmaster :master)
      (xlib:with-state (xmaster)
	(setf (xlib:drawable-x xmaster) (- x *default-window-border-width*)
	      (xlib:drawable-y xmaster) (- y *default-window-border-width* title-height)
	      (xlib:drawable-width xmaster) (+ width double-border)
	      (xlib:drawable-height xmaster) (+ height double-border title-height)))
      (xlib:reparent-window xwindow xmaster *default-window-border-width* (+ title-height *default-window-border-width*))
      (xlib:add-to-save-set xwindow)
      (setf (last-x window) (- x *default-window-border-width*)
	    (last-y window) (- y *default-window-border-width* title-height)
	    (last-width window) (+ width double-border)
	    (last-height window) (+ height title-height double-border))
      (setf (xlib:drawable-border-width xwindow) 0) ;the managed window need no border
      (set-wm-state window :normal (xlib:wm-hints-icon-window hints)) ;FIXME:we need to supply an icon if the client didn't set one
      (update-screen-window-geometry window)
      (if (workspace-equal (workspace window) (current-workspace screen))
	  (map-workspace-window window))
      (if (eql (xlib:wm-hints-input hints) :on) ;icccm
	  (workspace-set-focus (workspace window) window)
	  (if (find :WM_TAKE_FOCUS (protocols window))
	      (send-client-message window :WM_PROTOCOLS :WM_TAKE_FOCUS (get-event-time)))))))

(defun normalize-transient-window (xwindow transient-for)
  (let* ((main-window (xmaster-window (find-parent transient-for) *display*))
	 (state (get-wm-state (xwindow main-window))))
    (if (eql state :normal)
	(let* ((xroot (xlib:drawable-root xwindow))
	       (screen (find-screen xroot))
	       (x (xlib:drawable-x xwindow))
	       (y (xlib:drawable-y xwindow))
	       (width (xlib:drawable-width xwindow))
	       (height (xlib:drawable-height xwindow))
	       (double-border (* 2 *default-window-border-width*))
	       (xmaster (alloc-xwindow *display*
				       :parent xroot
				       :x (- x *default-window-border-width*)
				       :y (- y *default-window-border-width*)
				       :width (+ width double-border)
				       :height (+ height double-border)
				       :event-mask *toplevel-window-event*))
	       (window (make-instance 'transient-window
				      :id (xlib:window-id xmaster)
				      :main-window main-window
				      :xwindow xwindow
				      :xmaster xmaster
				      :protocols (xlib:wm-protocols xwindow)
				      :map-state :mapped))
	       (hints (xlib:wm-hints xwindow)))
	  (xlib:with-state (xmaster)
	    (setf (xlib:drawable-border-width xmaster) *default-window-border-width*
		  (xlib:window-border xmaster) (alloc-color *default-window-border* (screen window))
		  (xlib:drawable-x xmaster) (- x *default-window-border-width*)
		  (xlib:drawable-y xmaster) (- y *default-window-border-width*)
		  (xlib:drawable-width xmaster) width
		  (xlib:drawable-height xmaster) height))
					;	    (setf (xlib:window-priority (xmaster window)) (xmaster main-window) :above))
	  (xlib:reparent-window xwindow xmaster 0 0)
	  (xlib:add-to-save-set xwindow)
	  (setf (xlib:drawable-border-width xwindow) 0)
	  (setf (transient main-window) (list* window (transient main-window)))
	  (remove-window xwindow *display*)
	  (add-window window *display*)
	  (set-wm-state window :normal)
	  (if (workspace-equal (workspace window) (current-workspace screen))
	      (map-workspace-window window))
	  (if (eql (xlib:wm-hints-input hints) :on) ;icccm
	      (workspace-set-focus (workspace window) window)
	      (if (find :WM_TAKE_FOCUS (protocols window))
		  (send-client-message window :WM_PROTOCOLS :WM_TAKE_FOCUS (get-event-time))))))))

(defun normalize (xwindow)
  (let ((transient-for (xlib:get-property xwindow :WM_TRANSIENT_FOR)))
    (if transient-for
	(normalize-transient-window (xwindow transient-for))
	(normalize-toplevel-window xwindow))))

(defun withdrawn->normal (xwindow)
  (normalize xwindow))

;; FIXME:complete this function
(defun withdrawn->iconic (xwindow)
  (let ((hints (xlib:wm-hints xwindow)))
    (set-wm-state-1 xwindow :iconic (xlib:wm-hints-icon-window hints))))

(defun unwithdraw (xwindow)
  (let ((hints (xlib:wm-hints xwindow)))
    (case (xlib:wm-hints-initial-state hints)
      (:normal (withdrawn->normal xwindow))
      (:iconic (withdrawn->iconic xwindow)))))

;; Reparenting window managers must unmap the client's window when it is in the Iconic state, even if an ancestor window being unmapped renders the client's window unviewable
;; FIXME:complete this function
(defun normal->iconic (xwindow)
  (let ((hints (xlib:wm-hints xwindow)))
    (set-wm-state-1 xwindow :iconic (xlib:wm-hints-icon-window hints))))
;; FIXME:complete this function
(defun normal->withdrawn (xwindow)
  (let* ((window (find-window xwindow))
	 (xmaster (xmaster window))
	 (screen (screen window))
	 (workspace (workspace window))
	 (hints (xlib:wm-hints xwindow)))
    (setf (id window) (xlib:window-id xwindow))
    (remhash (xlib:window-id xmaster) (mapped-windows *display*))
    (setf (gethash (xlib:window-id xwindow) (withdrawn-windows *display*)) window)
    (remhash (xlib:window-id xmaster) (mapped-windows screen))
    (setf (gethash (xlib:window-id xwindow) (withdrawn-windows screen)) window)
    (setf (withdrawn-windows workspace) (list* window (withdrawn-windows workspace)))
    (setf (mapped-windows workspace) (remove window (mapped-windows workspace) :test #'window-equal))
    (unmap-window window)
    (xlib:unmap-window xwindow)
    (xlib:reparent-window xwindow (xlib:drawable-root xwindow) 0 0)))

(defun iconic->normal (xwindow)
  (normalize xwindow))

;; FIXME:complete this function
(defun iconic->withdrawn (xwindow)
  )

(defun manage-existing-xwindow (xwindow)
  (prepare-for-new-window xwindow)
  (if (not (eql (xlib:window-map-state xwindow) :unmapped))
      (unwithdraw xwindow)))
  

(defvar *root-event* '(:focus-change :button-motion :substructure-redirect :substructure-notify))

(defmethod manage-screen-root ((screen screen))
  (let* ((xroot (xlib:screen-root (xscreen screen)))
	 (root-id (xlib:window-id xroot))
	 (root (make-instance 'root-window
			      :id root-id
			      :xwindow xroot
			      :screen screen
			      :display (display screen)
			      :map-state :viewable)))
    (set-internal-window-type xroot :root)
    (setf (root screen) root
	  (xlib:window-event-mask (xwindow (root screen))) *root-event*)))

(defmethod manage-existing-windows ((screen screen))
  (let* ((xroot (xlib:screen-root (xscreen screen)))
	 (window-list (xlib:query-tree xroot))) ;from bottom-most (first) to top-most (last)
    (setf (xlib:window-cursor xroot) (cursor-default (display screen)))
    (dolist (win window-list)
      (unless (eql :on (xlib:window-override-redirect win))
	(manage-existing-window win xroot screen)))))

(defun calculate-geometry (screen xwindow gravity)
  (let* ((height (height screen))
	 (width (width screen))
	 (x (x screen))
	 (y (y screen))
	 (window-height (xlib:drawable-height xwindow))
	 (window-width (xlib:drawable-width xwindow))
	 (double-border (* 2 *internal-window-border-width*)))
    (case gravity
      (:center (setf (xlib:drawable-x xwindow) (+ x (floor (/ (- width window-width) 2)))
		     (xlib:drawable-y xwindow) (+ y (floor (/ (- height window-height) 2)))))
      (:bottom-left (setf (xlib:drawable-x xwindow) x
			  (xlib:drawable-y xwindow) (+ y (- height window-height double-border))))
      (:bottom-center (setf (xlib:drawable-x xwindow) (+ x (floor (/ (- width window-width double-border) 2)))
			    (xlib:drawable-y xwindow) (+ y (- height window-height double-border))))
      (:bottom-right (setf (xlib:drawable-x xwindow) (+ x (- width window-width double-border))
			   (xlib:drawable-y xwindow) (+ y (- height window-height double-border))))
      (:top-left (setf (xlib:drawable-x xwindow) x
		       (xlib:drawable-y xwindow) y))
      (:top-center (setf (xlib:drawable-x xwindow) (+ x (floor (/ (- width window-width double-border) 2)))
			 (xlib:drawable-y xwindow) y))
      (:top-right (setf (xlib:drawable-x xwindow) (+ x (- width window-width double-border))
			(xlib:drawable-y xwindow) y))
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
    (unless (eql gravity :keep)
      (xlib:with-state (xwindow)
	(if (eql (xlib:window-map-state xwindow) :unmapped)
	    (xlib:map-window xwindow))
	(setf (xlib:drawable-height xwindow) (+ (* 2 *internal-window-vertical-padding*) height)
	      (xlib:drawable-width xwindow) (+ (* 2 *internal-window-horizontal-padding*) width)
	      (xlib:window-priority xwindow) :top-if)
	(calculate-geometry screen xwindow gravity))
      (set-focus (current-focus (current-workspace screen)))
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
	(xlib:draw-rectangle xwindow gcontext 0 0 (xlib:drawable-width xwindow) (xlib:drawable-height xwindow) t)))))

(defmethod add-workspaces-according-to-layout ((screen screen))
  (if *workspace-layout*
      (dolist (name *workspace-layout*)
	(add-workspace-to-screen name screen))
      (add-workspace-to-screen "default" screen)))

(defmethod list-windows ((obj screen))
  (let ((windows nil))
    (maphash (lambda (id window)
	       (declare (ignore id))
	       (setf windows (append windows (list window)))) (mapped-windows obj))
    windows))

(defmethod list-workspaces ((screen screen))
  (let ((workspaces nil))
    (maphash (lambda (id workspace)
	       (declare (ignore id))
	       (setf workspaces (append workspaces (list workspace)))) (workspaces screen))
    workspaces))

;; we are in bottom-half,i.e. after the rc file loaded
(defmethod init-screen ((screen screen))
  (add-workspaces-according-to-layout screen)
  (set-current-workspace (find-workspace-by-id 1 (workspaces screen)) screen)
  (setf (output-font screen) (open-font (display screen) *output-font*)
	(mode-line screen) (make-internal-window screen)
	(mode-line-gc screen) (create-gcontext (mode-line screen)
					       (alloc-color *mode-line-background* screen)
					       (alloc-color *mode-line-foreground* screen)
					       (output-font screen))
	(mode-line-timer screen) (make-timer (list 'update-mode-line screen)
					     0.5)
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
  (xlib:set-wm-properties (key-prompt-window screen) :input :off)
  (manage-existing-windows screen)
  (init-mode-line screen)
  (xlib:grab-button (xwindow (root screen)) 1 '(:button-motion :button-release) ;FIXME:should be customizable
		    :modifiers (list (key->mod :alt (key-mod-map (display screen))))
		    :owner-p nil
		    :sync-pointer-p nil
		    :sync-keyboard-p nil))

(defmethod deinit-screen ((screen screen))
  (xlib:destroy-window (mode-line screen))
  (xlib:destroy-window (message-window screen))
  (xlib:destroy-window (key-prompt-window screen))
  (xlib:destroy-window (input-window screen))
  (xlib:close-font (output-font screen))
  (xlib:close-font (input-font screen))
  (xlib:free-gcontext (mode-line-gc screen))
  (xlib:free-gcontext (input-gc screen))
  (xlib:free-gcontext (configure-gc screen))
  (ungrab-key (root screen) :any)
  (xlib:ungrab-button (xwindow (root screen)) :any)
  (setf (xlib:window-event-mask (xwindow (root screen))) 0)
  (maphash (lambda (id workspace)
	     (declare (ignore id))
	     (destroy-workspace workspace)) (workspaces screen)))
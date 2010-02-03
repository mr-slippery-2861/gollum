(in-package :gollum)

(defclass display ()
  ((xdisplay :initarg :xdisplay
	     :accessor xdisplay
	     :initform nil)
   (id :initarg :id
       :accessor id
       :initform nil)
   (mapped-windows :initarg :mapped-windows
		   :accessor mapped-windows
		   :initform (make-hash-table))
   (withdrawn-windows :initarg :withdrawn-windows
		      :accessor withdrawn-windows
		      :initform (make-hash-table))
   (screens :initarg :screens
	    :accessor screens
	    :initform (make-hash-table))
   (current-screen :initarg :current-screen
		   :accessor display-current-screen
		   :initform nil)
   (key-mod-map :initarg :key-mod-map
		:accessor key-mod-map
		:initform nil)
   (mod-key-map :initarg :mod-key-map
		:accessor mod-key-map
		:initform nil)
   (mod-keycodes :initarg :mod-keycodes
		 :accessor mod-keycodes
		 :initform nil)
   (lock-type :initarg :lock-type
	      :accessor lock-type
	      :initform nil)
   (current-keymap :initarg :current-keymap
		:accessor current-keymap
		:initform nil)
   (keymaps :initarg :keymaps
	    :accessor keymaps
	    :initform (make-hash-table))
   (event-handlers :initarg :event-handlers
		   :accessor event-handlers
		   :initform nil)
   (last-event-timestamp :initarg :last-event-timestamp
			 :accessor last-event-timestamp
			 :initform 0)
   (cursor-default :initarg :cursor-default
		   :accessor cursor-default
		   :initform nil)
   (cursor-drag-move :initarg :cursor-drag-move
		     :accessor cursor-drag-move
		     :initform nil)
   (cursor-drag-resize :initarg :cursor-drag-resize
		       :accessor cursor-drag-resize
		       :initform nil)
   (font :initarg :font
	 :accessor font 
	 :initform nil)
   (xwindow-pool :initarg :xwindow-pool
		 :accessor xwindow-pool
		 :initform nil)))

(defgeneric input-focus (display))

(defgeneric find-keymap (keymap d))

(defgeneric add-keymap (keymap display))

(defgeneric bind-key (keymap key-desc action display &optional keep-keymap))

(defgeneric describe-key (key-desc display &optional kmap))

(defgeneric do-bind (key window display))

(defgeneric add-screen-to-display (xscreen id d))

(defgeneric update-last-event-timestamp (display timestamp))

(defun current-screen (&optional (display *display*))
  (display-current-screen display))

(defun (setf current-screen) (screen display)
  (setf (display-current-screen display) screen))

(defmethod input-focus ((display display))
  (let ((input-focus (xlib:input-focus (xdisplay display))))
    (if (xlib:window-p input-focus)
	(find-window input-focus)
	input-focus)))

(defmethod find-keymap (keymap (display display))
  (gethash keymap (keymaps display)))

(defmethod add-keymap (keymap (display display))
  (setf (gethash keymap (keymaps display)) (make-hash-table)))

(defmethod bind-key (keymap key-desc action (display display) &optional (keep-keymap nil))
  "ACTION is either a command or a keymap while KEYMAP and KEY are what their names indicate.
example:(bind-key :top-map \"C-h\" :help-map *display*)"
  (let ((key (kbd-internal key-desc (key-mod-map display))))
    (when (plusp key)
      (when (eql keymap :top-map)
	(multiple-value-bind (state keysym) (hash->key key)
	  (let ((keycode (xlib:keysym->keycodes (xdisplay display) keysym)))
	    (loop for k being the hash-keys in (screens display) using (hash-value screen)
	       do (grab-key (root screen) keycode :modifiers state :owner-p t :sync-pointer-p nil :sync-keyboard-p nil)))))
      (setf (gethash key (gethash keymap (keymaps display))) (cons action keep-keymap)))))

(defmethod describe-key (key-desc (display display) &optional (kmap :top-map))
  (let ((key-seq (split-string key-desc " "))
	(keymap kmap))
    (dolist (kbd key-seq)
      (let ((key (kbd-internal kbd (key-mod-map display))))
	(multiple-value-bind (action exist-p) (gethash key (find-keymap keymap display))
	  (if exist-p
	      (let* ((action (car action))
		     (args (if (listp action) (cdr action) nil))
		     (action (if (listp action) (car action) action)))
		(cond ((commandp action) (return-from describe-key action))
		      ((symbol->function action) (return-from describe-key action))
		      ((keywordp action) (setf keymap action))
		      (t (return-from describe-key nil))))
	      (return-from describe-key nil)))))
    keymap))

(defun key->key-desc (display key)
  (labels ((mod->abbr (mod)
	     (case mod
	       (:control "C-")
	       (:alt "A-")
	       (:meta "M-")
	       (:super "S-")
	       (:hyper "H-")
	       (t nil))))
    (multiple-value-bind (state keysym) (hash->key key)
      (let ((mods (xlib:make-state-keys state))
	    (key-char (keysym->keysym-name keysym))
	    (result nil))
	(mapc (lambda (mod)
		(setf result (concat result (mod->abbr (if (find mod '(:mod-1 :mod-2 :mod-3 :mod-4 :mod-5))
							   (car (rassoc mod (key-mod-map display)))
							   mod))))) mods)
	(concat result (string key-char))))))

(defun run-action (action)
  (let ((args (if (listp action) (cdr action) nil))
	(action (if (listp action) (car action) action)))
    (cond
      ((commandp action) (run-command action))
      ((symbol->function action) (apply (symbol-function action) args)))))

(let ((key-desc ""))
  (defmethod do-bind (key xwindow (display display))
    (when (eql (current-keymap display) :top-map)
      (grab-keyboard (root (current-screen display)) :owner-p nil :sync-pointer-p nil :sync-keyboard-p nil))
    (multiple-value-bind (action exist-p) (gethash key (find-keymap (current-keymap display) display))
      (let* ((keep-keymap (cdr action))
	     (action (car action))
	     (current-key-desc (string-trim " " (concat key-desc " " (key->key-desc display key))))
	     (screen (current-screen display)))
	(if exist-p
	    (cond
	      ((eql action :abort)
	       (setf keep-keymap nil)
	       (screen-prompt-key screen "^1Aborted." t))
	      ((keywordp action) (setf (current-keymap display) action
				       key-desc current-key-desc
				       keep-keymap t)
	       (screen-prompt-key screen (concat key-desc "-")))
	      (t (hide-key-prompt screen)
		 (run-action action)))
	    (progn		   ;there is no action corresponding the key
	      (screen-prompt-key screen (concat current-key-desc " not binded") t)))
	(unless keep-keymap
	  (setf (current-keymap display) :top-map
		key-desc "")
	  (xlib:ungrab-keyboard (xdisplay display)))))))

(defun update-key-mod-map (display)
  (multiple-value-bind (map mods mod-keycodes) (make-key-mod-map (xdisplay display))
    (setf (key-mod-map display) map)
    (setf (mod-key-map display) mods)
    (setf (mod-keycodes display) mod-keycodes)))

(defun open-display (host &key display protocol)
  (let* ((xdisplay (xlib:open-display host :display display :protocol protocol))
	 (id (xlib:display-display xdisplay))
	 (display (make-instance 'display
				 :xdisplay xdisplay
				 :id id
				 :current-keymap :top-map)))
    (dolist (xscreen (xlib:display-roots xdisplay))
      (add-screen-to-display xscreen (hash-table-count (screens display)) display)
      (if (eql xscreen (xlib:display-default-screen xdisplay))
	  (setf (current-screen display) ;set current-screen to the default screen of the display
		(gethash (1- (hash-table-count (screens display))) (screens display)))))
    display))

(defun close-display (display)
  (maphash (lambda (id screen)
	     (declare (ignore id))
	     (deinit-screen screen)) (screens display))
  (xlib:close-display (xdisplay display)))

(defun setup-cursor-shapes (display)
  (let ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	(black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
	(cursor-font (xlib:open-font (xdisplay display) "cursor")))
    (labels ((create-cursor (char)
	       (xlib:create-glyph-cursor :source-font cursor-font
					 :source-char char
					 :mask-font cursor-font
					 :mask-char (1+ char)
					 :foreground black
					 :background white)))
    (setf (cursor-default display) (create-cursor 68)
	  (cursor-drag-move display) (create-cursor 30)
	  (getf (cursor-drag-resize display) :nw) (create-cursor 134)
	  (getf (cursor-drag-resize display) :top) (create-cursor 138)
	  (getf (cursor-drag-resize display) :ne) (create-cursor 136)
	  (getf (cursor-drag-resize display) :right) (create-cursor 96)
	  (getf (cursor-drag-resize display) :se) (create-cursor 14)
	  (getf (cursor-drag-resize display) :bottom) (create-cursor 16)
	  (getf (cursor-drag-resize display) :sw) (create-cursor 12)
	  (getf (cursor-drag-resize display) :left) (create-cursor 70)))))

(defun init-display-top-half (display)
  (update-key-mod-map display)
  (update-lock-type display)
  (setf (xlib:display-error-handler (xdisplay display)) *error-handlers*)
  (add-keymap :top-map display)
  (add-keymap :input-map display)
  (add-keymap :root-map display)
  (add-keymap :window-map display)
  (setup-input-map display)
  (setup-default-bindings display)
  (setup-cursor-shapes display))

(defun init-display-bottom-half (display)
  (maphash (lambda (id screen)
	     (declare (ignore id))
	     (init-screen screen)) (screens display)))

(defmethod add-screen-to-display (xscreen id (display display))
  (let ((screen (make-instance 'screen
			       :id id
			       :xscreen xscreen
			       :height (xlib:screen-height xscreen)
			       :width (xlib:screen-width xscreen)))
	(xroot (xlib:screen-root xscreen)))
    (setf (gethash id (screens display)) screen
	  (display screen) display)
    (manage-screen-root screen)
    (update-screen-workspace-geometry screen)))

(defmethod update-last-event-timestamp ((display display) timestamp)
  (let ((last (last-event-timestamp display)))
    (if (or (> (- last timestamp) (* 30 internal-time-units-per-second)) (> timestamp last))
	(setf (last-event-timestamp display) timestamp))))

(defmethod xmaster-window (xwindow (obj display))
  (multiple-value-bind (window exist-p) (gethash (xlib:window-id xwindow) (mapped-windows obj))
    (if (and exist-p (xmaster window) (xlib:window-equal xwindow (xmaster window)))
	window
	nil)))

(defmethod xwindow-window (xwindow (obj display))
  (if (eql (get-wm-state-1 xwindow) :withdrown)
      nil
      (multiple-value-bind (window exist-p) (gethash (xlib:window-id xwindow) (withdrawn-windows obj))
	(if (and exist-p (xwindow window) (xlib:window-equal xwindow (xwindow window)))
	    window
	    nil))))

(defun find-window (xwindow &optional (raise-error nil))
  (let* ((type (get-internal-window-type xwindow))
	 (window (case type
		   (:master (xmaster-window xwindow *display*))
		   (:toplevel (xwindow-window xwindow *display*))
		   ((:title :border-right :border-left :border-top :border-bottom :border-nw :border-ne :border-sw :border-se) (xmaster-window (find-parent xwindow) *display*))
		   (:root (root (find-screen xwindow)))
		   (t (xwindow-window xwindow *display*)))))
     (if (and raise-error (null window))
	 (error 'no-such-window :xwindow xwindow)
	 window)))

(defmethod add-window ((window toplevel-window) (obj display))
  (setf (gethash (master-id window) (mapped-windows obj)) window)
  (setf (display window) obj)
  (add-window window (find-screen (xlib:drawable-root (xwindow window)))))

(defmethod add-window ((window transient-window) (obj display))
  (setf (gethash (master-id window) (mapped-windows obj)) window)
  (setf (display window) obj)
  (add-window window (find-screen (xlib:drawable-root (xwindow window)))))

(defmethod add-window ((window xlib:window) (obj display))
  (setf (gethash (xlib:window-id window) (withdrawn-windows obj)) window)
  (add-window window (find-screen (xlib:drawable-root window))))

(defmethod remove-window ((window xlib:window) (obj display))
  (remove-window window (find-screen (xlib:drawable-root window)))
  (remhash (xlib:window-id window) (withdrawn-windows obj)))

(defmethod remove-window ((window toplevel-window) (obj display))
  (remove-window window (screen window))
  (remhash (master-id window) (mapped-windows obj))
  (setf (display window) nil))

(defmethod remove-window ((window transient-window) (obj display))
  (remove-window window (screen window))
  (remhash (master-id window) (mapped-windows obj))
  (setf (display window) nil))

(defmethod delete-window ((window toplevel-window) (obj display))
  (let ((screen (screen window))
	(id (master-id window)))
    (delete-window window screen)
    (remhash id (mapped-windows obj))
    (free-xwindow (xmaster window) *display*)
    (destroy-decorate (decorate window))))

(defmethod delete-window ((window transient-window) (obj display))
  (let ((screen (screen window))
	(id (master-id window)))
    (delete-window window screen)
    (remhash id (mapped-windows obj))
    (free-xwindow (xmaster window) *display*)))

(defun alloc-xwindow (display &key parent (x 0) (y 0) (width 1) (height 1) background backing-store border (border-width 0) (cursor :none) event-mask override-redirect save-under)
  (let ((xwindow (car (xwindow-pool display))))
    (if xwindow
	(progn
	  (xlib:reparent-window xwindow parent x y)
	  (xlib:with-state (xwindow)
	    (setf (xlib:drawable-width xwindow) width
		  (xlib:drawable-height xwindow) height)
	    (if background (setf (xlib:window-background xwindow) background))
	    (if backing-store (setf (xlib:window-backing-store xwindow) backing-store))
	    (if border (setf (xlib:window-border xwindow) border))
	    (if border-width (setf (xlib:drawable-border-width xwindow) border-width))
	    (if cursor (setf (xlib:window-cursor xwindow) cursor))
	    (if event-mask (setf (xlib:window-event-mask xwindow) event-mask))
	    (if override-redirect (setf (xlib:window-override-redirect xwindow) override-redirect))
	    (if save-under (setf (xlib:window-save-under xwindow) save-under)))
	  (setf (xwindow-pool display) (cdr (xwindow-pool display))))
	(setf xwindow (xlib:create-window :parent parent :x x :y y :width width :height height :background background :backing-store backing-store :border border :border-width border-width :cursor cursor :event-mask event-mask :override-redirect override-redirect :save-under save-under)))
    xwindow))

(defun free-xwindow (xwindow display)
  (if (> (length (xwindow-pool display)) 16) ;FIXME:need to be customizable?
      (xlib:destroy-window xwindow)
      (progn
	(setf (xwindow-pool display) (list* xwindow (xwindow-pool display)))
	(xlib:unmap-window xwindow)
	(xlib:reparent-window xwindow (xlib:drawable-root xwindow) 0 0))))

(defun flush-display (display)
  (xlib:display-finish-output (xdisplay display)))

(defmethod ungrab-keyboard ((display display) &key time)
  (xlib:ungrab-keyboard (xdisplay display) :time time))
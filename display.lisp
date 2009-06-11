(in-package :gollum)

(defclass display ()
  ((xdisplay :initarg :xdisplay
	     :accessor xdisplay
	     :initform nil)
   (id :initarg :id
       :accessor id
       :initform nil)
   (windows :initarg :windows
	    :accessor windows
	    :initform (make-hash-table))
   (screens :initarg :screens
	    :accessor screens
	    :initform (make-hash-table))
   (current-screen :initarg :current-screen
		   :accessor current-screen
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
   (font :initarg :font
	 :accessor font
	 :initform nil)))

(defgeneric find-keymap (keymap d))

(defgeneric add-keymap (keymap display))

(defgeneric bind-key (keymap key-desc action display))

(defgeneric do-bind (key window display))

(defgeneric add-screen-to-display (xscreen id d))

(defmethod find-keymap (keymap (d display))
  (gethash keymap (keymaps d)))

(defmethod add-keymap (keymap (d display))
  (setf (gethash keymap (keymaps d)) (make-hash-table)))

(defmethod bind-key (keymap key-desc action (display display))
  "ACTION is either a command or a keymap while KEYMAP and KEY are what their names indicate.
example:(bind-key :top-map \"C-h\" :help-map)"
  (let ((key (kbd-internal key-desc (key-mod-map display))))
    (when (eql keymap :top-map )
      (multiple-value-bind (state keysym) (hash->key key)
	(let ((keycode (xlib:keysym->keycodes (xdisplay display) keysym)))
	  (loop for k being the hash-keys in (screens display) using (hash-value screen)
	     do (grab-key (root screen) keycode :modifiers state :owner-p t :sync-pointer-p nil :sync-keyboard-p nil)))))
    (setf (gethash key (gethash keymap (keymaps display))) action)))

(defun symbol->function (symbol)
  (handler-case (symbol-function symbol)
    (undefined-function () nil)))

(defmethod do-bind (key xwindow (display display))
  (when (eql (current-keymap display) :top-map)
    (let ((grab-state nil))
      (setf grab-state (grab-keyboard (root (current-screen display)) :owner-p nil :sync-pointer-p nil :sync-keyboard-p nil))))
  (multiple-value-bind (action exist-p) (gethash key (find-keymap (current-keymap display) display))
    (if exist-p
	(let ((args (if (listp action) (cdr action) nil))
	      (action (if (listp action) (car action) action)))
	  (cond
	    ((command-p action) (progn	;if action is a command,we run it,and restore the keymap state
				  (run-command action)
				  (xlib:ungrab-keyboard (xdisplay display))
				  (setf (current-keymap display) :top-map)))
	    ((symbol->function action) (progn	;if it's a function,we call it
					 (apply (symbol->function action) args)
					 (unless (eql (current-keymap display) :input-map)
					   (xlib:ungrab-keyboard (xdisplay display))
					   (setf (current-keymap display) :top-map))))
	    ((keywordp action) (setf (current-keymap display) action)))) ;else,we asume it a keymap,so set the keymap state
	(progn				      ;there is no action corresponding the key
	  (setf (current-keymap display) :top-map)
	  (xlib:ungrab-keyboard (xdisplay display))
	  (screen-message (current-screen display) "no action binded to key")))))

(defun update-key-mod-map (display)
  (multiple-value-bind (map mods mod-keycodes) (make-key-mod-map (xdisplay display))
    (setf (key-mod-map display) map)
    (setf (mod-key-map display) mods)
    (setf (mod-keycodes display) mod-keycodes)))

(defvar *all-displays* (make-hash-table))

(defvar *current-display* nil)

(defun open-display (host &key display protocol)
  (let* ((xdisplay (xlib:open-display host :display display :protocol protocol))
	 (id (xlib:display-display xdisplay))
	 (display (make-instance 'display
				 :xdisplay xdisplay
				 :id id
				 :current-keymap :top-map)))
    (update-key-mod-map display)
    (update-lock-type display)
    (add-keymap :top-map display)
    (add-keymap :input-map display)
    (setup-input-map display)
    (setf (gethash id *all-displays*) display
	  *current-display* display)
    (dolist (xscreen (xlib:display-roots xdisplay))
      (add-screen-to-display xscreen (hash-table-count (screens display)) display)
      (if (eql xscreen (xlib:display-default-screen xdisplay))
	  (setf (current-screen display) ;set current-screen to the default screen of the display
		(gethash (1- (hash-table-count (screens display))) (screens display)))))))

(defun init-display (display)
  (maphash (lambda (id screen)
	     (declare (ignore id))
	     (init-screen screen)) (screens display)))

(defun current-display ()
  *current-display*)

(defun xdisplay-display (xdisplay)
  (let ((id (xlib:display-display xdisplay)))
    (gethash id *all-displays*)))

(defmethod add-screen-to-display (xscreen id (d display))
  (let ((s (make-instance 'screen
			  :id id
			  :xscreen xscreen
			  :height (xlib:screen-height xscreen)
			  :width (xlib:screen-width xscreen))))
    (setf (gethash id (screens d)) s
	  (display s) d)))

(defmethod xwindow-window (xwin (obj display))
  (when xwin
    (multiple-value-bind (win exist-p) (gethash (xlib:window-id xwin) (windows obj))
      (and exist-p (xlib:window-equal xwin (xwindow win)) win))))

(defmethod add-window ((win window) (obj display))
  (setf (gethash (id win) (windows obj)) win
	(display win) obj))

(defmethod delete-window ((win window) (obj display))
  (let ((screen (screen win))
	(id (id win)))
    (setf (xwindow win) nil)		;it's a hacking,cause clx will destroy the window resource immediately when I don't know when,at least it's unavailable when we receive the destroy notify,so set xwindow to nil here to avoid further referencing which causes a window-error
    (delete-window win screen)
    (multiple-value-bind (w exist-p) (gethash id (windows obj))
      (declare (ignore w))
      (when exist-p
	(remhash id (windows obj))))))

(defun flush-display (display)
  (xlib:display-finish-output (xdisplay display)))


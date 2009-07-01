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

(defgeneric input-focus (display))

(defgeneric find-keymap (keymap d))

(defgeneric add-keymap (keymap display))

(defgeneric bind-key (keymap key-desc action display &optional keep-keymap))

(defgeneric describe-key (key-desc display &optional kmap))

(defgeneric do-bind (key window display))

(defgeneric add-screen-to-display (xscreen id d))

(defmethod input-focus ((display display))
  (let ((input-focus (xlib:input-focus (xdisplay display))))
    (if (xlib:window-p input-focus)
	(xwindow-window input-focus display)
	input-focus)))

(defmethod find-keymap (keymap (d display))
  (gethash keymap (keymaps d)))

(defmethod add-keymap (keymap (d display))
  (setf (gethash keymap (keymaps d)) (make-hash-table)))

(defmethod bind-key (keymap key-desc action (display display) &optional (keep-keymap nil))
  "ACTION is either a command or a keymap while KEYMAP and KEY are what their names indicate.
example:(bind-key :top-map \"C-h\" :help-map (current-display))"
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
		(cond ((command-p action) (return-from describe-key action))
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
      ((command-p action) (run-command action))
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
	(dformat 1 "do-bind: action ~a keep-keymap ~a" action keep-keymap)
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

(defvar *all-displays* (make-hash-table))

(defvar *current-display* nil)

(defun open-display (host &key display protocol)
  (let* ((xdisplay (xlib:open-display host :display display :protocol protocol))
	 (id (xlib:display-display xdisplay))
	 (display (make-instance 'display
				 :xdisplay xdisplay
				 :id id
				 :current-keymap :top-map)))
    (setf (gethash id *all-displays*) display
	  *current-display* display)
    (dolist (xscreen (xlib:display-roots xdisplay))
      (add-screen-to-display xscreen (hash-table-count (screens display)) display)
      (if (eql xscreen (xlib:display-default-screen xdisplay))
	  (setf (current-screen display) ;set current-screen to the default screen of the display
		(gethash (1- (hash-table-count (screens display))) (screens display)))))
    display))

(defun close-display (display)
  (let* ((xdisplay (xdisplay display))
	 (id (xlib:display-display xdisplay)))
    (xlib:close-display xdisplay)
    (remhash id *all-displays*)
    (setf *current-display* nil)))

(defun init-display-top-half (display)
  (update-key-mod-map display)
  (update-lock-type display)
  (setf (xlib:display-error-handler (xdisplay display)) *error-handlers*)
  (add-keymap :top-map display)
  (add-keymap :input-map display)
  (add-keymap :root-map display)
  (add-keymap :window-map display)
  (setup-input-map display)
  (setup-default-bindings display))

(defun init-display-bottom-half (display)
  (maphash (lambda (id screen)
	     (declare (ignore id))
	     (init-screen screen)) (screens display)))

(defun current-display ()
  *current-display*)

(defun xdisplay-display (xdisplay)
  (let ((id (xlib:display-display xdisplay)))
    (gethash id *all-displays*)))

(defmethod add-screen-to-display (xscreen id (display display))
  (let ((screen (make-instance 'screen
			       :id id
			       :xscreen xscreen
			       :height (xlib:screen-height xscreen)
			       :width (xlib:screen-width xscreen))))
    (setf (gethash id (screens display)) screen
	  (display screen) display)
    (manage-screen-root screen)))

(defmethod xwindow-window (xwin (obj display))
  (multiple-value-bind (win exist-p) (gethash (xlib:window-id xwin) (windows obj))
    (if (and exist-p (xmaster win) (xlib:window-equal xwin (xmaster win)))
	win
	(error 'no-such-window :xwindow xwin))))

(defmethod add-window ((win window) (obj display))
  (setf (gethash (id win) (windows obj)) win
	(display win) obj))

(defmethod delete-window ((win window) (obj display))
  (let ((screen (screen win))
	(id (id win)))
    (setf (map-state win) :unmapped)
    (delete-window win screen)
    (xlib:destroy-window (xmaster win))
    (multiple-value-bind (w exist-p) (gethash id (windows obj))
      (declare (ignore w))
      (when exist-p
	(remhash id (windows obj))))))

(defun flush-display (display)
  (xlib:display-finish-output (xdisplay display)))

(defmethod ungrab-keyboard ((display display) &key time)
  (xlib:ungrab-keyboard (xdisplay display) :time time))
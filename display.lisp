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

(defgeneric bind-key (keymap key-desc action display))

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

(defmethod bind-key (keymap key-desc action (display display))
  "ACTION is either a command or a keymap while KEYMAP and KEY are what their names indicate.
example:(bind-key :top-map \"C-h\" :help-map)"
  (let ((key (kbd-internal key-desc (key-mod-map display))))
    (when (plusp key)
      (when (eql keymap :top-map)
	(multiple-value-bind (state keysym) (hash->key key)
	  (let ((keycode (xlib:keysym->keycodes (xdisplay display) keysym)))
	    (loop for k being the hash-keys in (screens display) using (hash-value screen)
	       do (grab-key (root screen) keycode :modifiers state :owner-p t :sync-pointer-p nil :sync-keyboard-p nil)))))
      (setf (gethash key (gethash keymap (keymaps display))) action))))

(defmethod describe-key (key-desc (display display) &optional (kmap :top-map))
  (let ((key-seq (split-string key-desc " "))
	(keymap kmap))
    (dolist (kbd key-seq)
      (let ((key (kbd-internal kbd (key-mod-map display))))
	(multiple-value-bind (action exist-p) (gethash key (find-keymap keymap display))
	  (if exist-p
	      (let ((args (if (listp action) (cdr action) nil))
		    (action (if (listp action) (car action) action)))
		(cond ((command-p action) (return-from describe-key action))
		      ((symbol->function action) (return-from describe-key action))
		      ((keywordp action) (setf keymap action))
		      (t (return-from describe-key nil))))
	      (return-from describe-key nil)))))
    keymap))

(defun symbol->function (symbol)
  (handler-case (symbol-function symbol)
    (undefined-function () nil)
    (type-error () nil)))

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

(let ((key-desc "-"))
  (defmethod do-bind (key xwindow (display display))
    (when (eql (current-keymap display) :top-map)
      (let ((grab-state nil))
	(setf grab-state (grab-keyboard (root (current-screen display)) :owner-p nil :sync-pointer-p nil :sync-keyboard-p nil))))
    (multiple-value-bind (action exist-p) (gethash key (find-keymap (current-keymap display) display))
      (if exist-p
	  (let ((args (if (listp action) (cdr action) nil))
		(action (if (listp action) (car action) action))
		(current-key-desc (key->key-desc display key)))
	    (cond
	      ((command-p action) (progn	;if action is a command,we run it,and restore the keymap state
				    (run-command action)
				    (xlib:ungrab-keyboard (xdisplay display))
				    (setf (current-keymap display) :top-map
					  key-desc "-")
				    (screen-message (current-screen display) (string action))))
	      ((symbol->function action) (progn	;if it's a function,we call it
					   (apply (symbol-function action) args)
					   (unless (eql (current-keymap display) :input-map)
					     (xlib:ungrab-keyboard (xdisplay display))
					     (setf (current-keymap display) :top-map)
					     (screen-message (current-screen display) (string action)))
					   (setf key-desc "-")))
	      ((keywordp action) (setf (current-keymap display) action
				       key-desc (concat
						 (subseq key-desc 0 (1- (length key-desc)))
						 " "
						 current-key-desc
						 "-"))
	       (screen-message (current-screen display) key-desc nil)))) ;else,we asume it a keymap,so set the keymap state
	  (progn				      ;there is no action corresponding the key
	    (setf (current-keymap display) :top-map
		  key-desc "-")
	    (xlib:ungrab-keyboard (xdisplay display))
	    (screen-message (current-screen display) (format nil "no action bind,I'm ~a,event is ~a" (bordeaux-threads:thread-name (bordeaux-threads:current-thread)) key)))))))

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
    (if (and exist-p (xlib:window-equal xwin (xwindow win)))
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
    (multiple-value-bind (w exist-p) (gethash id (windows obj))
      (declare (ignore w))
      (when exist-p
	(remhash id (windows obj))))))

(defun flush-display (display)
  (xlib:display-finish-output (xdisplay display)))

(defmethod ungrab-keyboard ((display display) &key time)
  (xlib:ungrab-keyboard (xdisplay display) :time time))
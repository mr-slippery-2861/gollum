(in-package :gollum)

(defun key->mod (key key-mod-map)
  (cdr (assoc key key-mod-map)))

(defun update-lock-type (display)
  (setf (lock-type display) (if (find "Caps_Lock" (second (mod-key-map display)) :test #'string=)
			:caps-lock
			:shift-lock)))

(defun code-state->keysym (code state display)
  (let* ((mods (xlib:make-state-keys state))
	 (keysym-0 (xlib:keycode->keysym (xdisplay display) code 0))
	 (keysym-1 (xlib:keycode->keysym (xdisplay display) code 1))
	 (shift-p (and (find :shift mods) t))
	 (lock-p (and (find :lock mods) t))
	 (num-lock-p (and (find (key->mod :num-lock (key-mod-map display)) mods) t))
	 (keysym0-character (xlib:keysym->character (xdisplay display) keysym-0)))
    (labels ((keypad-p (keysym)
	       (let ((keysym-name (keysym->keysym-name keysym)))
		 (if (> (length keysym-name) 3)
		     (string= "KP_" keysym-name :end2 3)
		     nil))))
      (cond
	((and num-lock-p (keypad-p keysym-1))
	 (if (or shift-p (and lock-p (eql (lock-type display) :shift-lock)))
	     keysym-0
	     keysym-1))
	((and (not shift-p) (not lock-p))
	 keysym-0)
	((and (not shift-p) lock-p (eql (lock-type display) :caps-lock))
	 (if (and (characterp keysym0-character) (lower-case-p keysym0-character))
	     keysym-1
	     keysym-0))
	((and shift-p lock-p (eql (lock-type display) :caps-lock))
	 (write-line "3nd cond")
	 (if (and (characterp keysym0-character) (lower-case-p keysym0-character))
	     keysym-0
	     keysym-1))
	((or shift-p (and lock-p (eql (lock-type display) :shift-lock)))
	 keysym-1)))))

(defparameter *event-handlers* (make-array 64 :initial-element nil))

(defmacro define-event-handler (event-key actual-keys &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p ,@actual-keys &allow-other-keys)
		(declare (ignore event-slots))
		(handler-case (progn ,@body)
		  (no-such-window () t))))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

(defmacro define-event-handler-2 ((event-key restart &rest actual-keys) &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p ,@actual-keys &allow-other-keys)
		(declare (ignore event-slots))
		(handler-bind ((no-such-window (lambda (c)
						 ,(if restart
						      `(invoke-restart 'copy-superior window)
						      `(invoke-restart 'ignore-window)))))
		  (restart-case (progn ,@body)
		    ,(if restart
			 `(copy-superior (xwindow)
					 (,restart xwindow))
			 `(ignore-window () t))))))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

;; Input Focus Events

(defun find-toplevel (window)
  (if (or (window-equal window (root (screen window))) (toplevel-p window))
      window
      (find-toplevel (parent window))))

(define-event-handler :focus-in (window mode kind)
  (unless (find kind '(:pointer :virtual :nonlinear-virtual))
    (let* ((d (xdisplay-display display))
	   (win (xwindow-window window d))
	   (top (find-toplevel win))
	   (screen (screen top))
	   (workspace (current-workspace screen)))
      (set-input-focus top)
      (dformat 1 "window ~a get focus" (wm-name win))))
  t)

(define-event-handler :focus-out (window mode kind)
  t)

;; Keyboard and Pointer Events

;; key-press key-release button-press button-release
;; event-slot-names:
;; window event-window code x y state time root root-x root-y child same-screen-p
(defun filt-button-from-state (state)
  (let ((mods (xlib:make-state-keys state)))
    (setf mods (remove-if-not (lambda (x) (typep x 'xlib:modifier-key)) mods))
    (apply #'xlib:make-state-mask mods)))

(defun filt-state (state)
  (let ((mods (xlib:make-state-keys state)))
    (setf mods (remove :lock (remove :shift (remove-if-not (lambda (x) (typep x 'xlib:modifier-key)) mods))))
    (apply #'xlib:make-state-mask mods)))

(define-event-handler :key-press (code state window)
  (let* ((d (xdisplay-display display))
	 (keysym (code-state->keysym code state d))
	 (key (key->hash (filt-state state) keysym))) ;FIME:we simply filt :shift out
    (unless (find code (mod-keycodes d))
      (do-bind key window d)))
  t)

(define-event-handler :key-release ()
  t)

(defun check-peek-event (xdisplay)
    (xlib:event-case (xdisplay :peek-p t :timeout nil)
      (t (event-key)
	 event-key)))

(defun pointer-current-xwindow (xroot)
  (multiple-value-bind (x y same-screen-p child state-mask root-x root-y root) (xlib:query-pointer xroot)
    (declare (ignore x y same-screen-p state-mask root-x root-y root))
    child))

(define-event-handler :button-press (code window)
  (let* ((dpy (xdisplay-display display))
	 (xcurrent (pointer-current-xwindow window)))
    (dformat 1 "button ~a pressed" code)
    (when (and (= code 1) xcurrent)		;FIXME:should be customizable
      (if (eql (check-peek-event display) :motion-notify)
	  (set-drag-move-window (xwindow-window xcurrent dpy)))))
  t)

(define-event-handler :button-release ()
  (dformat 1 "button released")
  t)

(define-event-handler :motion-notify (state window root-x root-y)
  (let* ((d (xdisplay-display display))
	 (win (xwindow-window window d)))
    (if (eql (check-peek-event display) :button-release)
	(progn
	  (drag-move-window root-x root-y)
	  (set-drag-move-window nil))
	(drag-move-window root-x root-y :prompt t))
    (dformat 1 "motion-notify, window ~a root-x ~a root-y ~a next-event ~a" (wm-name win) root-x root-y (check-peek-event display)))
  t)

(define-event-handler :enter-notify (window mode kind)
  (if (and (eql mode :normal) (not (find kind '(:virtual :nonlinear-virtual :inferior))))
      (let* ((d (xdisplay-display display))
	     (win (xwindow-window window d))
	     (top (find-toplevel win))
	     (screen (screen win)))
	(unless (window-equal win (root screen))
	  (setf (current-window (current-workspace screen)) top))
	(dformat 1 "enter-notify received,window ~a mode ~a kind ~a" (wm-name win) mode kind)))
  t)

;; Keyboard and Pointer State Events

(define-event-handler :mapping-notify (request)
  (let ((d (xdisplay-display display)))
    (case request
      (:modifier (progn
		   (update-key-mod-map d)
		   (update-lock-type d))))
    t))

;; Exposure Events


;; Window State Events
(define-event-handler :circulate-notify ()
  t)

(define-event-handler :configure-notify ()
  t)

(define-event-handler :create-notify (window parent override-redirect-p)
  (let* ((d (xdisplay-display display))
	 (p (xwindow-window parent d))
	 (s (screen p)))
    (dformat 1 "create-notify received, window ~a" (xlib:wm-name window))
    (unless (or override-redirect-p (not (xlib:window-equal parent (xlib:drawable-root window)))) ;we ignore override-redirection window and non toplevel window
      (manage-new-window window parent s)))
  t)

(define-event-handler :destroy-notify (event-window window)
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window event-window d)))
    ;; XLIB:The ordering of the :DESTROY-NOTIFY events is such that for any given window, :DESTROY-NOTIFY is generated on all inferiors of a window before :DESTROY-NOTIFY is generated on the _window_.
    (unless (window-equal w (root (screen w)))
      (delete-window w d)))
  t)

(define-event-handler :gravity-notify ()
  t)

(define-event-handler :map-notify (window override-redirect-p)
  t)

(define-event-handler :reparent-notify (window parent override-redirect-p)
  (dformat 1 "window ~a reparented to its new parent ~a" (xlib:get-wm-class window) (xlib:get-wm-class parent))
  t)

(define-event-handler :unmap-notify ()
  t)

;; Structure Control Events

(define-event-handler :circulate-request (window place)
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window window d)))
    t))

(define-event-handler :configure-request (parent window x y width height border-width stack-mode above-sibling value-mask)
  (dformat 1 "configure-request received")
  (let* ((d (xdisplay-display display))
	 (pwin (xwindow-window parent d))
	 (screen (screen pwin)))
    (multiple-value-bind (root-x root-y dst-child) (translate-coordinates pwin x y (root screen))
      (let* ((x-p (plusp (logand value-mask 1)))
	     (y-p (plusp (logand value-mask 2)))
	     (width-p (plusp (logand value-mask 4)))
	     (height-p (plusp (logand value-mask 8)))
	     (double-border (* 2 *default-window-border-width*))
;	 (border-width-p (plusp (logand value-mask 16)))
	     (new-x (if x-p (max root-x (x screen)) (orig-x pwin)))
	     (new-y (if y-p (max root-y (y screen)) (orig-y pwin)))
	     (new-width (if width-p (min width (- (width screen) double-border)) (orig-width pwin)))
	     (new-height (if height-p (min height (- (height screen) double-border)) (orig-height pwin))))
;	(stack-mode-p (plusp (logand value-mask 32)))
;	(above-sibling-p (plusp (logand value-mask 64))))
	(when (not (maximized win))
	  (xlib:with-state (window)		;FIXME:check the geometric first
	    (if x-p (setf (xlib:drawable-x window) (+ new-x *default-window-border-width*)))
	    (if y-p (setf (xlib:drawable-y window) (+ new-y *default-window-border-width*)))
	    (if width-p (setf (xlib:drawable-width window) new-width))
	    (if height-p (setf (xlib:drawable-height window) new-height)))
	  (xlib:with-state (parent)
	    (if x-p (setf (xlib:drawable-x parent) new-x))
	    (if y-p (setf (xlib:drawable-y parent) new-y))
	    (if width-p (setf (xlib:drawable-width parent) new-width))
	    (if height-p (setf (xlib:drawable-height parent) new-height)))
	  (xlib:display-finish-output display))
	(setf (orig-x pwin) new-x
	      (orig-y pwin) new-y
	      (orig-width pwin) new-width
	      (orig-height pwin) new-height))))
  t)

(defun withdrawn-to-mapped (window)
  (let* ((workspace (workspace window))
	 (withdrawn (withdrawn-windows workspace))
	 (mapped (mapped-windows workspace)))
    (setf (withdrawn-windows workspace) (remove window withdrawn :test #'window-equal)
	  (mapped-windows workspace) (sort-by-stacking-order (list* window mapped) (screen workspace)))))

(define-event-handler :map-request (parent window)
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window parent d))
	 (ws (workspace w)))
    (dformat 1 "map-request received,window ~a" (wm-name w))
    (unless (xlib:window-equal (xmaster w) (xlib:drawable-root window))
      (xlib:map-window window)
      (setf (ws-map-state w) :viewable)
      (set-wm-state parent 1)		;1 for normal
      (withdrawn-to-mapped w)
      (when (workspace-equal (current-workspace (current-screen d)) ws)
	(map-workspace-window w))
      (flush-display d)))
  t)

(define-event-handler :resize-request (window width height)
  (let* ((d (xdisplay-display display))
	 (win (xwindow-window window d))
	 (screen (screen win))
	 (actual-width (min (width screen) width))
	 (actual-height (min (height screen) height)))
    (dformat 1 "resize-request received")
    (xlib:with-state (window)
      (setf (xlib:drawable-width window) actual-width
	    (xlib:drawable-height window) actual-height))
    (setf (orig-width win) actual-width
	  (orig-height win) actual-height)
    (xlib:display-finish-output display))
  t)

(defun event-processor (&optional (display (current-display)))
  (loop 
     (xlib:process-event (xdisplay display) :handler *event-handlers*)))



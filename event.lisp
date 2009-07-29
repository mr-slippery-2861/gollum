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
	 (if (and (characterp keysym0-character) (lower-case-p keysym0-character))
	     keysym-0
	     keysym-1))
	((or shift-p (and lock-p (eql (lock-type display) :shift-lock)))
	 keysym-1)))))

(defparameter *event-handlers* (make-array 64 :initial-element nil))

(defmacro define-event-handler (event-key actual-keys &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p ,@actual-keys &allow-other-keys)
		(declare (ignore display event-key event-slots send-event-p))
		(dformat 1 "get into ~a" ,event-key)
		(handler-bind ((xlib:window-error (lambda (c)
						    (declare (ignore c))
						    (invoke-restart 'ignore)))
			       (no-such-window (lambda (c)
						 (declare (ignore c))
						 (invoke-restart 'ignore))))
		  (restart-case (progn ,@body)
		    (ignore () t)))))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

;; Input Focus Events

(defun find-toplevel (window)
  (if (or (window-equal window (root (screen window))) (toplevel-p window))
      window
      (find-toplevel (parent window))))

(define-event-handler :focus-in (window mode kind)
  ;; (unless (find kind '(:pointer :virtual :nonlinear-virtual))
  ;;   (let* ((win (xwindow-window window *display*))
  ;; 	   (top (find-toplevel win))
  ;; 	   (screen (screen top))
  ;; 	   (workspace (current-workspace screen)))
  ;;     (set-input-focus top)))
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
  (let* ((keysym (code-state->keysym code state *display*))
	 (key (key->hash (filt-state state) keysym))) ;FIME:we simply filt :shift out
    (unless (find code (mod-keycodes *display*))
      (do-bind key window *display*)))
  t)

(define-event-handler :key-release ()
  t)

(defun check-peek-event (xdisplay)
    (xlib:event-case (xdisplay :peek-p t :timeout nil)
      (t (event-key)
	 event-key)))

(defun pointer-current-xwindow (xwindow)
  (multiple-value-bind (x y same-screen-p child state-mask root-x root-y root) (xlib:query-pointer (xlib:drawable-root xwindow))
    (declare (ignore x y same-screen-p state-mask root-x root-y root))
    child))

(define-event-handler :button-press (code window)
  (let ((xcurrent (pointer-current-xwindow window)))
    (when (and (= code 1) xcurrent)		;FIXME:should be customizable
      (if (eql (check-peek-event display) :motion-notify)
	  (set-drag-move-window (xmaster-window xcurrent *display*)))))
  t)

(define-event-handler :button-release ()
  t)

(define-event-handler :motion-notify (root-x root-y)
  (if (eql (check-peek-event display) :button-release)
      (progn
	(drag-move-window root-x root-y)
	(set-drag-move-window nil))
      (drag-move-window root-x root-y :prompt t))
  t)

(define-event-handler :enter-notify (window mode kind)
  ;; (if (and (eql mode :normal) (not (find kind '(:virtual :nonlinear-virtual :inferior))))
  ;;     (let* ((win (xwindow-window window *display*))
  ;; 	     (top (find-toplevel win))
  ;; 	     (screen (screen win)))
  ;; 	(unless (window-equal win (root screen))
  ;; 	  (setf (current-window (current-workspace screen)) top))))
  t)

;; Keyboard and Pointer State Events

(define-event-handler :mapping-notify (request)
  (case request
    (:modifier (progn
		 (update-key-mod-map *display*)
		 (update-lock-type *display*))))
    t)

;; Exposure Events


;; Window State Events
(define-event-handler :circulate-notify ()
  t)

(define-event-handler :configure-notify ()
  t)

(defun probe-xwindow (xwindow)
  (handler-bind ((xlib:window-error (lambda (c)
				      (declare (ignore c))
				      (invoke-restart 'not-exist))))
    (restart-case (xlib:window-map-state xwindow)
      (not-exist () nil))))

(define-event-handler :create-notify (window parent override-redirect-p)
  (unless (or override-redirect-p (eql (xlib:window-class window) :input-only) (not (xlib:window-equal parent (xlib:drawable-root parent))))
    (let* ((root (xmaster-window (xlib:drawable-root parent) *display*))
	   (screen (screen root)))
      (if (not (gollum-master-p window))
	  (prepare-new-window window parent screen))))
  t)

;; (define-event-handler :create-notify (window parent override-redirect-p)
;;   (xlib:with-server-grabbed ((xdisplay *display*))
;;     (if (probe-xwindow window)	;make sure the window is still alive
;; 	(let* ((p (xwindow-window parent *display*))
;; 	       (s (screen p)))
;; 	  (unless (or override-redirect-p (eql (xlib:window-class window) :input-only) (not (xlib:window-equal parent (xlib:drawable-root window)))) ;we ignore override-redirection window and non toplevel window
;; 	    (manage-new-window window parent s)))
;; 	(dformat 1 "the window does not suvived")))
;;   t)

(define-event-handler :destroy-notify (event-window window)
  (let ((w (xmaster-window event-window *display*)))
    ;; XLIB:The ordering of the :DESTROY-NOTIFY events is such that for any given window, :DESTROY-NOTIFY is generated on all inferiors of a window before :DESTROY-NOTIFY is generated on the _window_.
    (unless (window-equal w (root (screen w)))
      (delete-window w *display*)))
  t)

(define-event-handler :gravity-notify ()
  t)

(define-event-handler :map-notify (window override-redirect-p)
  t)

(define-event-handler :reparent-notify (window parent x y override-redirect-p)
  ;; (if (and (xlib:window-equal parent (xlib:drawable-root parent)) (eql override-redirect-p :off))
  ;;     (xlib:with-server-grabbed ((xdisplay *display*))
  ;; 	(if (probe-xwindow window)
  ;; 	    (manage-new-window window parent (screen (xwindow-window parent *display*))))))
  t)

(define-event-handler :unmap-notify ()
  t)

;; Structure Control Events

(define-event-handler :circulate-request (window place)
  t)

(define-event-handler :configure-request (parent window x y width height border-width stack-mode above-sibling value-mask)
  (unless (xlib:window-equal parent (xlib:drawable-root parent))
  (let* ((pwin (xwindow-window parent *display*))
	 (screen (screen pwin)))
    (multiple-value-bind (root-x root-y dst-child) (translate-coordinates pwin x y (root screen))
      (declare (ignore dst-child))
      (let* ((x-p (plusp (logand value-mask 1)))
	     (y-p (plusp (logand value-mask 2)))
	     (width-p (plusp (logand value-mask 4)))
	     (height-p (plusp (logand value-mask 8)))
	     (double-border (* 2 *default-window-border-width*))
;	 (border-width-p (plusp (logand value-mask 16)))
	     (new-x (if x-p (max x (x screen)) (orig-x pwin)))
	     (new-y (if y-p (max y (y screen)) (orig-y pwin)))
	     (new-width (if width-p (min width (- (width screen) double-border)) (orig-width pwin)))
	     (new-height (if height-p (min height (- (height screen) double-border)) (orig-height pwin))))
;	(stack-mode-p (plusp (logand value-mask 32)))
;	(above-sibling-p (plusp (logand value-mask 64))))
	(when (not (maximized pwin))
	  (xlib:with-state (window)		;FIXME:check the geometric first
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
	      (orig-height pwin) new-height)))))
  t)

(define-event-handler :map-request (parent window)
  (if (or (eql (xlib:window-class window) :input-only)
	  (not (xlib:window-equal parent (xlib:drawable-root parent))))
      (xlib:map-window window)
      (xlib:with-server-grabbed ((xdisplay *display*))
	(xlib:map-window window)
	(if (probe-xwindow window)
	    (unwithdraw window)
	    ;; if the window does not survived,we suppose a destroy-notify will be received
	    ;; so we process the died window later
	    (dformat 1 "the window ~a does not survived" (xlib:window-id window)))))
  t)

(define-event-handler :resize-request (window width height)
  (let* ((win (xwindow-window window *display*))
	 (screen (screen win))
	 (actual-width (min (width screen) width))
	 (actual-height (min (height screen) height)))
    (xlib:with-state (window)
      (setf (xlib:drawable-width window) actual-width
	    (xlib:drawable-height window) actual-height))
    (setf (orig-width win) actual-width
	  (orig-height win) actual-height)
    (xlib:display-finish-output display))
  t)

(defun event-processor (&optional (display *display*))
  (loop
     (xlib:process-event (xdisplay display) :handler *event-handlers*)))


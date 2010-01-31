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

;; warning: we must consume all kinds of events as soon as possible or gollum will be very lag

(defmacro define-event-handler (event-key actual-keys &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p ,@actual-keys &allow-other-keys)
		(declare (ignore display event-key event-slots send-event-p))
		(dformat 9 "get into ~a" ,event-key)
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

(define-event-handler :focus-in (window mode kind)
  ;; (unless (find kind '(:pointer :virtual :nonlinear-virtual))
  ;;   (let ((win (xwindow-window window *display*)))
  ;;     (setf (current-focus (workspace win)) win)))
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

(define-event-handler :key-press (code state window time)
  (let* ((keysym (code-state->keysym code state *display*))
	 (key (key->hash (filt-state state) keysym))) ;FIME:we simply filt :shift out
    (update-event-time time)
    (unless (find code (mod-keycodes *display*))
      (do-bind key window *display*)))
  t)

(define-event-handler :key-release (time)
  (update-event-time time)
  t)

(defun peek-event (xdisplay)
  (xlib:event-case (xdisplay :peek-p t :timeout nil)
    (t (event-key)
       event-key)))

(defun pointer-current-xwindow (xwindow)
  (multiple-value-bind (x y same-screen-p child state-mask root-x root-y root) (xlib:query-pointer (xlib:drawable-root xwindow))
    (declare (ignore x y same-screen-p state-mask root-x root-y root))
    child))

(defun find-parent (xwindow)
  (multiple-value-bind (children parent root) (xlib:query-tree xwindow)
    (declare (ignore children root))
    parent))

(define-event-handler :button-press (code window root-x root-y time)
  (labels ((find-window (xwindow type)
	     (case type
	       ((:title :border-right :border-left :border-top :border-bottom :border-nw :border-ne :border-sw :border-se)
		(xmaster-window (find-parent xwindow) *display*)))))
    (let* ((type (get-internal-window-type window))
	   (win (find-window window type)))
      (update-event-time time)
      (when (= code 1)		;FIXME:should be customizable
	(init-drag-moveresize win root-x root-y (case type
						  (:title :move)
						  (:border-nw :nw-resize)
						  (:border-top :top-resize)
						  (:border-ne :ne-resize)
						  (:border-right :right-resize)
						  (:border-se :se-resize)
						  (:border-bottom :bottom-resize)
						  (:border-sw :sw-resize)
						  (:border-left :left-resize)))
	(if (eql type :title)
	    (xlib:grab-pointer window '(:button-motion :button-release) :cursor (cursor-drag-move *display*))
	    (xlib:grab-pointer window '(:button-motion :button-release))))))
  t)

(define-event-handler :button-release (window time)
  (update-event-time time)
  (let ((type (get-internal-window-type window)))
    (case type
      ((:title :border-right :border-left :border-top :border-bottom :border-nw :border-ne :border-sw :border-se)
       (progn
	 (reset-drag-moveresize)
	 (xlib:ungrab-pointer (xdisplay *display*))))))
  t)

(define-event-handler :motion-notify (window root-x root-y time)
  (update-event-time time)
  (let ((type (get-internal-window-type window)))
    (case type
      ((:title :border-right :border-left :border-top :border-bottom :border-nw :border-ne :border-sw :border-se)
       (drag-moveresize-window root-x root-y))))
  t)

(define-event-handler :enter-notify (window mode kind time)
  (update-event-time time)
  ;; (if (and (eql mode :normal) (not (find kind '(:virtual :nonlinear-virtual :inferior))))
  ;;     (let* ((win (xwindow-window window *display*))
  ;; 	     (top (find-toplevel win))
  ;; 	     (screen (screen win)))
  ;; 	(unless (window-equal win (root screen))
  ;; 	  (setf (current-window (current-workspace screen)) top))))
  t)

(define-event-handler :leave-notify (time)
  (update-event-time time)
  t)

;; Keyboard and Pointer State Events

(define-event-handler :mapping-notify (request)
  (case request
    (:modifier (progn
		 (update-key-mod-map *display*)
		 (update-lock-type *display*))))
    t)

;; Exposure Events

(define-event-handler :exposure (window count)
  (if (zerop count)
      (if (eql :title (get-internal-window-type window))
	  (update-title (decorate (xmaster-window (find-parent window) *display*)))))
  t)

;; Window State Events
(define-event-handler :circulate-notify ()
  t)

(define-event-handler :configure-notify (window x y width height)
  (when (eql (get-internal-window-type window) :master)
    (dformat 2 "window-type: ~a :configure-notify received, x: ~a y: ~a width: ~a height: ~a" (get-internal-window-type window) x y width height))
   t)

(defun probe-xwindow (xwindow)
  (handler-bind ((xlib:window-error (lambda (c)
				      (declare (ignore c))
				      (invoke-restart 'not-exist))))
    (restart-case (xlib:window-map-state xwindow)
      (not-exist () nil))))

(defun find-screen (xroot)
  (loop for k being the hash-keys in (screens *display*) using (hash-value screen)
     when (xlib:window-equal xroot (xwindow (root screen)))
     return screen))

(define-event-handler :create-notify (window parent override-redirect-p)
  (unless (or override-redirect-p
	      (eql (xlib:window-class window) :input-only)
	      (not (xlib:window-equal parent (xlib:drawable-root parent))))
    (if (not (eql (get-internal-window-type window) :master))
	(prepare-for-new-window window)))
  t)

;; XLIB:The ordering of the :DESTROY-NOTIFY events is such that for any given window, :DESTROY-NOTIFY is generated on all inferiors of a window before :DESTROY-NOTIFY is generated on the _window_.
(define-event-handler :destroy-notify (event-window)
  (labels ((process-mapped-window (xmaster)
	     (let* ((window (xmaster-window xmaster *display*))
		    (workspace (workspace window)))
	       (delete-window window *display*)
	       (workspace-set-focus workspace (current-window workspace))))
	   (process-withdrawn-window (xroot)
	     (let* ((screen (find-screen xroot))
		    (window (loop for k being the hash-keys in (withdrawn-windows screen) using (hash-value window)
			       when (not (probe-xwindow (xwindow window)))
			       return window)))
		(delete-window window *display*))))
    (let ((type (get-internal-window-type event-window)))
      (case type
	(:root (process-withdrawn-window event-window))
	(:master (process-mapped-window event-window))))))

(define-event-handler :gravity-notify ()
  t)

(define-event-handler :map-notify (window override-redirect-p)
  t)

(define-event-handler :reparent-notify (window parent override-redirect-p)
  (unless override-redirect-p
    (if (eql (get-internal-window-type parent) :root)
	(prepare-for-new-window window parent)
	(progn
	  (remove-window window *display*)
	  (set-internal-window-type window nil))))
  t)

(define-event-handler :unmap-notify ()
  t)

;; Structure Control Events

(define-event-handler :circulate-request (window place)
  t)

(define-event-handler :configure-request (parent window x y width height border-width stack-mode above-sibling value-mask)
  ;; (unless (xlib:window-equal parent (xlib:drawable-root parent))
  ;; (let* ((pwin (xwindow-window parent *display*))
  ;; 	 (screen (screen pwin)))
  ;;   (multiple-value-bind (root-x root-y dst-child) (translate-coordinates pwin x y (root screen))
  ;;     (declare (ignore dst-child))
  ;;     (let* ((x-p (plusp (logand value-mask 1)))
  ;; 	     (y-p (plusp (logand value-mask 2)))
  ;; 	     (width-p (plusp (logand value-mask 4)))
  ;; 	     (height-p (plusp (logand value-mask 8)))
  ;; 	     (double-border (* 2 *default-window-border-width*))
  ;; 	     (border-width-p (plusp (logand value-mask 16)))
  ;; 	     (new-x (if x-p (max x (x screen)) (orig-x pwin)))
  ;; 	     (new-y (if y-p (max y (y screen)) (orig-y pwin)))
  ;; 	     (new-width (if width-p (min width (- (width screen) double-border)) (orig-width pwin)))
  ;; 	     (new-height (if height-p (min height (- (height screen) double-border)) (orig-height pwin)))
  ;; 	     (stack-mode-p (plusp (logand value-mask 32)))
  ;; 	     (above-sibling-p (plusp (logand value-mask 64))))
  ;; 	(when (not (maximized pwin))
  ;; 	  (xlib:with-state (window)		;FIXME:check the geometric first
  ;; 	    (if width-p (setf (xlib:drawable-width window) new-width))
  ;; 	    (if height-p (setf (xlib:drawable-height window) new-height)))
  ;; 	  (xlib:with-state (parent)
  ;; 	    (if x-p (setf (xlib:drawable-x parent) new-x))
  ;; 	    (if y-p (setf (xlib:drawable-y parent) new-y))
  ;; 	    (if width-p (setf (xlib:drawable-width parent) new-width))
  ;; 	    (if height-p (setf (xlib:drawable-height parent) new-height)))
  ;; 	  (xlib:display-finish-output display))
  ;; 	(setf (orig-x pwin) new-x
  ;; 	      (orig-y pwin) new-y
  ;; 	      (orig-width pwin) new-width
  ;; 	      (orig-height pwin) new-height)))))
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
  ;; (let* ((win (xwindow-window window *display*))
  ;; 	 (screen (screen win))
  ;; 	 (actual-width (min (width screen) width))
  ;; 	 (actual-height (min (height screen) height)))
  ;;   (xlib:with-state (window)
  ;;     (setf (xlib:drawable-width window) actual-width
  ;; 	    (xlib:drawable-height window) actual-height))
  ;;   (setf (orig-width win) actual-width
  ;; 	  (orig-height win) actual-height)
  ;;   (xlib:display-finish-output display))
  t)

(define-event-handler :client-message (window type data)
  (case type
    (:WM_CHANGE_STATE (when (= (elt data 0) 3) ;3 is IconicState
			(normal->iconic window))))
  t)

(defun event-processor (&optional (display *display*))
  (loop
     (xlib:process-event (xdisplay display) :handler *event-handlers*)))


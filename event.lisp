(in-package :gollum)

(defun key->mod (key key-mod-map)
  (cadr (assoc key key-mod-map)))

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
		,@body))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

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
    (setf mods (remove :shift (remove-if-not (lambda (x) (typep x 'xlib:modifier-key)) mods)))
    (apply #'xlib:make-state-mask mods)))

(define-event-handler :key-press (code state window)
  (declare (ignore event-key send-event-p))
  (let* ((d (xdisplay-display display))
	 (keysym (code-state->keysym code state d))
	 (key (key->hash (filt-state state) keysym))) ;FIME:we simply filt :shift out
    (unless (find code (mod-keycodes d))
      (do-bind key window d))
    t))

(define-event-handler :key-release ()
  (declare (ignore display event-key send-event-p))
  t)

;; Keyboard and Pointer State Events

(define-event-handler :mapping-notify (request)
  (declare (ignore event-key send-event-p))
  (let ((d (xdisplay-display display)))
    (case request
      (:modifier (progn
		   (update-key-mod-map d)
		   (update-lock-type d))))
    t))

;; Exposure Events


;; Window State Events
(define-event-handler :circulate-notify ()
  (declare (ignore display event-key send-event-p))
  t)

(define-event-handler :configure-notify ()
  (declare (ignore display event-key send-event-p))
  t)

(define-event-handler :create-notify (window parent override-redirect-p)
  (declare (ignore event-key send-event-p))
  (let* ((d (xdisplay-display display))
	 (p (xwindow-window parent d))
	 (s (screen p)))
    (write-line (format nil "create-notify received,id:~a" (xlib:window-id window)) *error-output*)
    (unless override-redirect-p
      (manage-new-window window parent s)))
  t)

(define-event-handler :destroy-notify (window)
  (declare (ignore event-key send-event-p))
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window window d)))
    (write-line (format nil "destroy-notify received,id:~a" (xlib:window-id window)) *error-output*)
    (delete-window w d))
  t)

(define-event-handler :gravity-notify ()
  (declare (ignore display event-key send-event-p))
  t)

(define-event-handler :map-notify ()
  (declare (ignore display event-key send-event-p))
  t)

(define-event-handler :reparent-notify ()
  (declare (ignore display event-key send-event-p))
  t)

(define-event-handler :unmap-notify ()
  (declare (ignore display event-key send-event-p))
  t)

;; Structure Control Events

(define-event-handler :circulate-request (window place)
  (declare (ignore event-key send-event-p))
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window window d)))
    t))

(define-event-handler :configure-request (window x y width height border-width stack-mode above-sibling value-mask)
  (declare (ignore event-key send-event-p stack-mode above-sibling))
  (let ((x-p (plusp (logand value-mask 1)))
	(y-p (plusp (logand value-mask 2)))
	(width-p (plusp (logand value-mask 4)))
	(height-p (plusp (logand value-mask 8)))
	(border-width-p (plusp (logand value-mask 16))))
;	(stack-mode-p (plusp (logand value-mask 32)))
;	(above-sibling-p (plusp (logand value-mask 64))))
    (xlib:with-state (window)		;FIXME:check the geometric first
      (when x-p (setf (xlib:drawable-x window) x))
      (when y-p (setf (xlib:drawable-y window) y))
      (when width-p (setf (xlib:drawable-width window) width))
      (when height-p (setf (xlib:drawable-height window) height))
      (when border-width-p (setf (xlib:drawable-border-width window) border-width))
      ))
  (xlib:display-finish-output display)
  t)

(define-event-handler :map-request (window)
  (declare (ignore event-key send-event-p))
  (let* ((d (xdisplay-display display))
	 (w (xwindow-window window d)))
    (write-line "map-request received" *error-output*)
    (map-window w)
    (xlib:display-finish-output display))
  t)

(define-event-handler :resize-request (window width height)
  (declare (ignore event-key send-event-p))
  (xlib:with-state (window)
    (setf (xlib:drawable-width window) width
	  (xlib:drawable-height window) height))
  (xlib:display-finish-output display)
  t)

(defun event-processor (&optional (display (current-display)))
  (loop
     (xlib:process-event (xdisplay display) :handler *event-handlers*)))


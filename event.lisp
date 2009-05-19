(in-package :gollum)

(defun key->mod (key)
  (cadr (assoc key *key-mod-map*)))

(defvar *lock-type* (if (find "Caps_Lock" (second *mod-key-map*) :test #'string=)
			:caps-lock
			:shift-lock))

(defun code-state->keysym (code state)
  (let* ((mods (xlib:make-state-keys state))
	 (keysym-0 (xlib:keycode->keysym *display* code 0))
	 (keysym-1 (xlib:keycode->keysym *display* code 1))
	 (shift-p (and (find :shift mods) t))
	 (lock-p (and (find :lock mods) t))
	 (num-lock-p (and (find (key->mod :num-lock) mods) t)))
    (labels ((keypad-p (keysym)
	       (let ((keysym-name (keysym->keysym-name keysym)))
		 (if (> (length keysym-name) 3)
		     (string= "KP_" keysym-name :end2 3)
		     nil))))
      (cond
	((and num-lock-p (keypad-p keysym-1))
	 (if (or shift-p (and lock-p (eql *lock-type* :shift-lock)))
	     keysym-0
	     keysym-1))
	((and (not shift-p) (not lock-p))
	 keysym-0)
	((and (not shift-p) lock-p (eql *lock-type* :caps-lock))
	 (if (and (= (length (keysym->keysym-name keysym-0)) 1)
		  (lower-case-p (char (keysym->keysym-name keysym-0) 0)))
	     keysym-1
	     keysym-0))))))

(defparameter *event-handlers* (make-array 64 :initial-element nil))

(defmacro define-event-handler (event-key actual-keys &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p ,@actual-keys &allow-other-keys)
		(declare (ignore event-slots))
		,@body))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

;; key-press key-release button-press button-release
;; event-slot-names:
;; window event-window code x y state time root root-x root-y child same-screen-p
(define-event-handler :key-press (code state)
  (declare (ignore display event-key send-event-p))
  )

(define-event-handler :button-press
  (declare (ignore display event-key send-event-p))
  event-slots)


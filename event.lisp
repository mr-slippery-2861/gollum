(in-package :gollum)

(defparameter *event-handlers* (make-array 64 :initial-element nil))

(defmacro define-event-handler (event-key &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (&rest event-slots &key display event-key send-event-p &allow-other-keys)
		,@body))
       (setf (elt *event-handlers* (position ,event-key xlib::*event-key-vector*)) #',fn-name))))

;; key-press key-release button-press button-release
;; event-slot-names:
;; window event-window code x y state time root root-x root-y child same-screen-p
(define-event-handler :key-press
  (declare (ignore display event-key send-event-p))
  )

(define-event-handler :button-press
  (declare (ignore display event-key send-event-p))
  event-slots)


(in-package :gollum)

(defvar *event-thread* nil)

;; (bordeaux-threads:make-thread #'thread-test :name "test" :initial-bindings '((what . :this)))

(defvar *timer-thread* nil)

(defun parse-display-name (display-name)
  (let* ((colon-1 (position #\: display-name))
	 (colon-2 (position #\: display-name :from-end t))
	 (dot (position #\. display-name))
	 (host (subseq display-name 0 colon-1))
	 (display (handler-case (subseq display-name (1+ colon-2) dot)
		    (type-error () (subseq display-name (1+ colon-2)))))
	 (screen (and dot (subseq display-name (1+ dot)))))
    (values host (parse-integer display) (and screen (parse-integer screen))
	    (cond
	      ((string-equal host "") :local)
	      ((= colon-1 colon-2) :tcp)
	      (t :dna)))))

(defvar *display* nil)

(defun update-event-time (timestamp)
  (update-last-event-timestamp *display* timestamp))

(defun get-event-time ()
  (last-event-timestamp *display*))

(defun gollum (display-name &optional (debug nil))
  (multiple-value-bind (host display screen protocol) (parse-display-name display-name)
    (declare (ignore screen))
    (setf *debug-level* debug)
    (setf *display* (open-display host :display display :protocol protocol))
    (init-display-top-half *display*)
    (load-rc)
    (init-display-bottom-half *display*)
    (setf *event-thread* (bordeaux-threads:make-thread #'event-processor :name "event-processor"))
    (setf *timer-thread* (bordeaux-threads:make-thread #'timers-runner :name "timers-runner"))
    (unless debug
      (bordeaux-threads:join-thread *timer-thread*))))

(defun gollum-quit ()
  (and (bordeaux-threads:thread-alive-p *timer-thread*) (bordeaux-threads:destroy-thread *timer-thread*))
  (and (bordeaux-threads:thread-alive-p *event-thread*) (bordeaux-threads:destroy-thread *event-thread*))
  (close-display *display*))

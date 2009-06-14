(in-package :gollum)

(defvar *event-threads* nil)

;; (bordeaux-threads:make-thread #'thread-test :name "test" :initial-bindings '((what . :this)))

(defvar *timer-threads* nil)

(defun parse-display-name (display-name)
  (let* ((colon-1 (position #\: display-name))
	 (colon-2 (position #\: display-name :from-end t))
	 (dot (position #\. display-name))
	 (host (subseq display-name 0 colon-1))
	 (display (subseq display-name (1+ colon-2) dot))
	 (screen (subseq display-name (1+ dot))))
    (values host (parse-integer display) (parse-integer screen)
	    (cond
	      ((string-equal host "") :local)
	      ((= colon-1 colon-2) :tcp)
	      (t :dna)))))

(defun gollum (display-name)
  (multiple-value-bind (host display screen protocol) (parse-display-name display-name)
    (declare (ignore screen))
    (init-display-top-half (open-display host :display display :protocol protocol))
    (load-rc)
    (init-display-bottom-half (current-display))
    (setf *event-threads* (bordeaux-threads:make-thread #'event-processor :name "event-processor"))
    (setf *timer-threads* (bordeaux-threads:make-thread #'timers-runner :name "timers-runner"))))

(defun gollum-quit ()
  (bordeaux-threads:destroy-thread *event-threads*)
  (bordeaux-threads:destroy-thread *timer-threads*)
  (close-display (current-display)))
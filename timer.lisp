(in-package :gollum)

(defclass timer ()
  ((real-time :initarg :real-time
	      :accessor real-time
	      :initform nil)
   (repeat :initarg :repeat
	   :accessor repeat
	   :initform nil)
   (action :initarg :action
	   :accessor action
	   :initform nil)))

(defvar *all-timers* nil)

(defvar *timers-lock* (bordeaux-threads:make-lock "timers-lock"))

(defconstant +timer-sleep-time+ 0.5
  "number of seconds the timer runner sleeps everytime.")

(defgeneric timer< (timer1 timer2))

(defgeneric add-timer (timer))

(defgeneric delete-timer (timer))

(defgeneric run-timer (timer))

(defmethod timer< ((timer1 timer) (timer2 timer))
  (< (real-time timer1) (real-time timer2)))

(defmethod add-timer ((timer timer))
  (bordeaux-threads:with-lock-held (*timers-lock*)
    (let ((new-timer (list timer)))
      (setf *all-timers* (sort (append *all-timers* new-timer) #'timer<)))))

(defmethod delete-timer ((timer timer))
  (bordeaux-threads:with-lock-held (*timers-lock*)
    (setf *all-timers* (sort (remove timer *all-timers*) #'timer<))))

(defun make-timer (time action &optional (repeat nil))
  (add-timer (make-instance 'timer
			    :real-time (+ (get-internal-real-time) (* time internal-time-units-per-second))
			    :repeat repeat
			    :action action)))

(defmethod run-timer ((timer timer))
  (let ((time (get-internal-real-time))
	(repeat (repeat timer))
	(action (action timer)))
    (when (> time (real-time timer))
      (cond
	((functionp action) (funcall action))
	((and (stringp action) (command-p action)) (run-command action)))
      (if (numberp repeat)
	  (setf (real-time timer) (+ (real-time timer) (* repeat internal-time-units-per-second)))))))

(defun run-timers ()
  (when *all-timers*
    (bordeaux-threads:with-lock-held (*timers-lock*)
      (mapc #'run-timer *all-timers*)
      (setf *all-timers* (sort *all-timers* #'timer<)))))

(defun timers-runner ()
  (loop
     (sleep +timer-sleep-time+)
     (run-timers)))

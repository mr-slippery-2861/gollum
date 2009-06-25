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

(defclass message-timer (timer)
  ((message-window :initarg :message-window
		   :accessor message-window
		   :initform nil)
   (screen :initarg :screen
	   :accessor screen
	   :initform nil)))

(defvar *all-timers* nil)

(defvar *timers-lock* (bordeaux-threads:make-recursive-lock "timers-lock"))
(defvar *timers-cv* (bordeaux-threads:make-condition-variable))
(defvar *idle-lock* (bordeaux-threads:make-lock "idle-lock"))

(defconstant +timer-sleep-time+ 0.5
  "number of seconds the timer runner sleeps everytime.")

(defconstant +message-time-out+ 5)

(defgeneric timer< (timer1 timer2))

(defgeneric add-timer (timer)
  (:documentation "this is for internal using. use MAKE-TIMER and SCHEDULE-TIMER."))

(defgeneric delete-timer (timer))

(defgeneric schedule-timer (timer &optional time)
  (:documentation "TIME is in how many seconds TIMER will trigger, repeat time of TIMER if not provided."))

(defgeneric run-timer (timer))

(defmethod timer< ((timer1 timer) (timer2 timer))
  (< (real-time timer1) (real-time timer2)))

(defmethod add-timer ((timer timer))
  (bordeaux-threads:with-recursive-lock-held (*timers-lock*)
    (let ((new-timer (list timer)))
      (setf *all-timers* (sort (append *all-timers* new-timer) #'timer<))))
  (bordeaux-threads:condition-notify *timers-cv*))

(defmethod delete-timer ((timer timer))
  (bordeaux-threads:with-recursive-lock-held (*timers-lock*)
    (setf *all-timers* (sort (remove timer *all-timers*) #'timer<))))

(defun make-timer (action &optional (repeat nil))
  (make-instance 'timer
		 :repeat repeat
		 :action action))

(defmethod schedule-timer ((timer timer) &optional (time nil))
  (let ((real-time (or time (repeat timer))))
    (when real-time
      (setf (real-time timer) (+ (get-internal-real-time) (* real-time internal-time-units-per-second)))
      (add-timer timer))))

(defmethod schedule-timer ((timer message-timer) &optional (time +message-time-out+))
  (setf (real-time timer) (+ (get-internal-real-time) (* time internal-time-units-per-second)))
  (add-timer timer))

(defmethod run-timer ((timer timer))
  (let ((time (get-internal-real-time))
	(repeat (repeat timer))
	(action (action timer)))
    (when (> time (real-time timer))
      (cond
	((symbol->function action) (funcall (symbol->function action)))
	((command-p action) (run-command action)))
      (if (numberp repeat)
	  (setf (real-time timer) (+ (real-time timer) (* repeat internal-time-units-per-second)))
	  (delete-timer timer)))))

(defmethod run-timer ((timer message-timer))
  (let ((time (get-internal-real-time))
	(action (action timer))
	(screen (screen timer)))
    (when (> time (real-time timer))
      (funcall (symbol->function action) screen)
      (delete-timer timer))))

(defun run-timers ()
  (bordeaux-threads:with-recursive-lock-held (*timers-lock*)
    (mapc #'run-timer *all-timers*)
    (setf *all-timers* (sort *all-timers* #'timer<)))
  (sleep +timer-sleep-time+))

(defun timers-runner ()
  (bordeaux-threads:with-lock-held (*idle-lock*)
    (loop
       (if *all-timers*
	   (run-timers)
	   (bordeaux-threads:condition-wait *timers-cv* *idle-lock*)))))

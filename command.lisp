(in-package :gollum)

(defvar *user-commands* (make-hash-table :test #'equal))

(defclass command ()
  ((name :initarg :name
	 :accessor name
	 :initform nil)
   (fn-name :initarg :fn-name
	    :accessor fn-name
	    :initform nil)))

(defmacro defcommand (name args &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name ,args
		,@body))
       (setf (gethash ,name *user-commands*) (make-instance 'command :name ,name :fn-name #',fn-name)))))

(defun run-command-1 (name &rest rest)
  (let* ((command (gethash name *user-commands*))
	 (fn (fn-name command)))
    (apply fn rest)))

(defun run-command (cmd)
  "CMD is a string looks like \"cmd [args]\",
and ARGS will be treated as string by default."
  (let ((command (split-string cmd " ")))
    (apply #'run-command-1 command)))
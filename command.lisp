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
  (declare (type string name)
	   (type list args))
  (let ((fn-name (gensym)))
    `(labels ((,fn-name ,args
		,@body))
       (setf (gethash ,name *user-commands*) (make-instance 'command :name ,name :fn-name #',fn-name)))))

(defun run-command-1 (name &rest rest)
  (multiple-value-bind (command exist-p) (gethash name *user-commands*)
    (if exist-p
	(apply (fn-name command) rest)))) ;FIXME:what to do if no command found

(defun run-command (cmd)
  "CMD is a string looks like \"cmd [args]\",
and ARGS will be treated as string by default."
  (let ((command (split-string cmd " ")))
    (apply #'run-command-1 command)))

(defun command-p (string)
  (typep (gethash (car (split-string (string-trim " " string) " ")) *user-commands*) 'command))

(defun exec (program)
  (sb-ext:run-program program nil :search t :wait nil))
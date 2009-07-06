(in-package :gollum)

(defvar *user-commands* (make-hash-table))

(defmacro defcommand (name args &body body)
  (declare (type symbol name)
	   (type list args))
  (let ((fn-name (gensym)))
    `(labels ((,fn-name ,args
		,@body))
       (setf (gethash ',name *user-commands*) #',fn-name))))

(defun commandp (symbol)
  (let ((fn (gethash symbol *user-commands*)))
    (functionp fn)))

(deftype command ()
  '(satisfies commandp))

(defun run-command (cmd &rest args)
  (if (commandp cmd)
      (apply (gethash cmd *user-commands*) args)))

(defun exec (program)
  #+sbcl (sb-ext:run-program program nil :search t :wait nil))

(defcommand emacs ()
  (raise-or-exec "emacs" :class "Emacs"))

(defcommand firefox ()
  (raise-or-exec "firefox" :class "Firefox"))
(in-package :gollum)

(defclass decorate ()
  ((window :initarg :window
	   :accessor window
	   :initform nil)
   (height :initarg :height
	   :accessor height
	   :initform nil)))


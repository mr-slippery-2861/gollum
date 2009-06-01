(in-package :gollum)

(defclass workspace ()
  ((name :initarg :name
	 :accessor name
	 :initform nil)
   (id :initarg :id
	   :accessor id
	   :initform nil)
   (screen :initarg :screen
	   :accessor screen
	   :initform nil)
   (windows :initarg :windows		;it's by stacking order?
	     :accessor windows
	     :initform nil)))

(defgeneric move-window-to-workspace (win ws)
  (:documentation "move a window from its original workspace to another"))

(defgeneric workspace-equal (w1 w2)
  (:documentation "return t if the two workspaces are refer to the same one,nil otherwise"))

(defgeneric unmap-workspace (ws)
  (:documentation "unmap all the windows in a workspace"))

(defgeneric map-workspace (ws)
  (:documentation "map all the windows in a workspace"))

(defgeneric list-workspace-windows (ws)
  (:documentation "list the windows of the workspace"))

(defun next-workspace-id (workspaces)
  (1+ (hash-table-count workspaces)))

(defun find-workspace-by-id (id workspaces)
  (multiple-value-bind (w exist-p) (gethash id workspaces)
    (if exist-p w nil)))

(defun find-workspace-by-name (name workspaces)
  (loop for k being the hash-keys in workspaces using (hash-value v)
     when (string= (name v) name)
     return v))

(defun find-workspace (what workspaces)
  (cond
    ((typep what 'number) (find-workspace-by-id what workspaces))
    ((typep what 'string) (find-workspace-by-name what workspaces))))

(defun get-nworkspace (workspaces)
  (hash-table-count workspaces))

(defmethod workspace-equal ((w1 workspace) (w2 workspace))
  (= (id w1) (id w2)))

(defmethod add-window ((win window) (obj workspace))
  (setf (workspace win) obj)
  (push win (windows obj))
  (when (workspace-equal obj (current-workspace (screen win)))
    (map-workspace-window win)))

(defmethod delete-window ((win window) (obj workspace))
  (setf (workspace win) nil
	(windows obj) (remove win (windows obj) :test #'window-equal))
  (when (workspace-equal obj (current-workspace (screen win)))
    (unmap-workspace-window win)))

(defmethod move-window-to-workspace ((win window) (ws workspace))
  (let ((source (find-workspace-by-id (workspace win) (workspaces (screen win))))
	(dest ws))
    (delete-window win source)
    (add-window win dest)))

(defmethod map-workspace ((ws workspace))
  (mapc #'map-workspace-window (windows ws)))

(defmethod unmap-workspace ((ws workspace))
  (mapc #'unmap-workspace-window (windows ws)))

(defmethod list-workspace-windows ((ws workspace))
  (let ((windows-list (windows ws)))
    (mapcar (lambda (win) (win-name win)) windows-list)))

(defvar *workspace-layout* nil
  "A list of strings whose elements are the names of workspaces.")


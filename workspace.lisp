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
   (mapped-windows :initarg :mapped-windows ;FIXME:windows sorted by stack order?
		   :accessor mapped-windows
		   :initform nil)
   (withdrawn-windows :initarg :withdrawn-windows
		      :accessor withdrawn-windows
		      :initform nil)
   (current-window :initarg :current-window
		   :accessor current-window
		   :initform nil)))

(defgeneric add-window (win obj))

(defgeneric delete-window (win obj)
  (:documentation "make a window out of gollum's control"))

(defgeneric move-window-to-workspace (win ws)
  (:documentation "move a window from its original workspace to another"))

(defgeneric workspace-equal (w1 w2)
  (:documentation "return t if the two workspaces are refer to the same one,nil otherwise"))

(defgeneric unmap-workspace (ws)
  (:documentation "unmap all the windows in a workspace"))

(defgeneric map-workspace (ws)
  (:documentation "map all the windows in a workspace"))

(defgeneric list-workspace-windows (ws &key name class)
  (:documentation "list the windows of the workspace"))

(defgeneric workspace-next-window (workspace))

(defgeneric workspace-prev-window (workspace))

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
  (case (get-wm-state win)
    (0 (push win (withdrawn-windows obj)))
    (1 (push win (mapped-windows obj)))) ;FIXME:which position to put
  (when (workspace-equal obj (current-workspace (screen win)))
    (map-workspace-window win)))

(defmethod delete-window ((win window) (obj workspace))
  (setf (workspace win) nil)
  (setf (withdrawn-windows obj) (remove win (withdrawn-windows obj) :test #'window-equal))
  (setf (mapped-windows obj) (remove win (mapped-windows obj) :test #'window-equal))
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

(defmethod list-workspace-windows ((ws workspace) &key (name t) class)
  (let ((windows-list (mapped-windows ws)))
    (mapcar (lambda (win) (list (and name (wm-name win)) (and class (wm-class win)))) windows-list)))

(defmethod current-window ((obj null))
  (current-window (current-workspace nil)))

(defmethod workspace-next-window ((workspace workspace))
  (let ((windows (mapped-windows workspace)))
    (when (> (length windows) 1)
      (let ((current (car windows))
	    (next (cadr windows)))
	(setf (mapped-windows workspace) (append (cdr windows) (list current))
	      (current-window workspace) next)
	(raise-window next)
	(set-input-focus next)))))

(defun next-window ()
  (workspace-next-window (current-workspace nil)))

(defmethod workspace-prev-window ((workspace workspace))
  (let ((windows (mapped-windows workspace)))
    (when (> (length windows) 1)
      (let ((prev (car (last windows))))
	(setf (mapped-windows workspace) (list* prev (butlast windows))
	      (current-window workspace) prev)
	(raise-window prev)
	(set-input-focus prev)))))

(defun prev-window ()
  (workspace-prev-window (current-workspace nil)))

(defvar *workspace-layout* nil
  "A list of strings whose elements are the names of workspaces.")
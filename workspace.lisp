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
		   :accessor workspace-current-window
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

(defgeneric list-windows (obj))

(defgeneric find-matching-windows (obj &key instance class name))

(defgeneric workspace-raise-window (workspace window))

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
  (when (toplevel-p win)
    (setf (workspace win) obj)
    (case (get-wm-state win)
      (0 (push win (withdrawn-windows obj)))
      (1 (setf (mapped-windows obj) (sort-by-stacking-order (append (mapped-windows obj) (list win)) (screen obj))))) ;now the mapped-windows are in stacking order
    (if (workspace-equal obj (current-workspace (screen win)))
	(map-workspace-window win)
	(unmap-workspace-window win))
    (if (null (current-window obj))
	(setf (current-window obj) win))))

(defmethod delete-window ((win window) (obj workspace))
  (when (toplevel-p win)
    (setf (workspace win) nil)
    (setf (withdrawn-windows obj) (remove win (withdrawn-windows obj) :test #'window-equal))
    (setf (mapped-windows obj) (remove win (mapped-windows obj) :test #'window-equal)) ;remove doesn't break the stacking order
    (when (workspace-equal obj (current-workspace (screen win)))
      (unmap-workspace-window win))))

(defmethod move-window-to-workspace ((win window) (ws workspace))
  (let ((source (find-workspace-by-id (workspace win) (workspaces (screen win))))
	(dest ws))
    (delete-window win source)
    (add-window win dest)))

(defmethod map-workspace ((workspace workspace))
  (mapc #'map-workspace-window (mapped-windows workspace))
  (when (current-window workspace)
    (raise-window (current-window workspace))
    (set-input-focus (current-window workspace) :parent)))

(defmethod unmap-workspace ((workspace workspace))
  (mapc #'unmap-workspace-window (mapped-windows workspace)))

(defmethod list-windows ((obj workspace))
  (mapped-windows obj))

(defmethod find-matching-windows ((obj workspace) &key instance class name)
  (let ((windows (mapped-windows obj)))
    (remove-if-not (lambda (window)
		     (match-window window :instance instance :class class :name name)) windows)))

(defun current-window (&optional workspace)
  (workspace-current-window (or workspace (current-workspace))))

(defun (setf current-window) (window workspace)
  (setf (workspace-current-window workspace) window))

(defun workspace-nwindows (workspace)
  (length (mapped-windows workspace)))

;; FIXME:should we set input focus when raising or circulating window?
(defmethod workspace-raise-window ((workspace workspace) (window window))
  (let ((windows (mapped-windows workspace)))
    (when (find window windows :test #'window-equal)
      (let* ((id (id window))
	     (screen workspace))
	(setf (stacking-orderd screen) (append (remove id (stacking-orderd screen) :test #'=) (list id)))
	(setf (current-window workspace) window
	      (mapped-windows workspace) (sort-by-stacking-order windows screen))
	(raise-window window)))))

(defmethod workspace-next-window ((workspace workspace))
  (let ((windows (mapped-windows workspace)))
    (when (> (length windows) 1)
      (let* ((current (last windows))
	     (next (last (butlast windows)))
	     (id (id (car current)))
	     (screen (screen workspace)))
	(setf (stacking-orderd screen) (list* id (remove id (stacking-orderd screen) :test #'=)))
	(setf (mapped-windows workspace) (sort-by-stacking-order windows screen)
	      (current-window workspace) (car next))
	(circulate-window-down (root screen))))))

(defun next-window ()
  (workspace-next-window (current-workspace nil)))

(defmethod workspace-prev-window ((workspace workspace))
  (let ((windows (mapped-windows workspace)))
    (when (> (length windows) 1)
      (let* ((prev (first windows))
	     (id (id prev))
	     (screen (screen workspace)))
	(setf (stacking-orderd screen) (append (remove id (stacking-orderd screen) :test #'=) (list id)))
	(setf (mapped-windows workspace) (sort-by-stacking-order windows screen)
	      (current-window workspace) prev)
	(circulate-window-up (root screen))))))

(defun prev-window ()
  (workspace-prev-window (current-workspace nil)))

(defvar *workspace-layout* nil
  "A list of strings whose elements are the names of workspaces.")
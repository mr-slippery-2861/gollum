(in-package :gollum)

(defmethod current-screen ((obj null))
  (current-screen (current-display)))

(defmethod current-workspace ((obj null))
  (current-workspace (current-screen nil)))

(defun raise-or-exec (program &key class name instance)
  (let ((win (find-matching-window (windows (current-screen nil)) :class class :name name :instance instance)))
    (if win
	(raise-workspace-window win (current-screen nil))
	(exec program))))

(defun launch ()
  (with-screen-input ((current-screen nil) "launch: " program)
    (exec program)))

(defun set-key (keymap key-desc action)
  (bind-key keymap key-desc action (current-display)))

(defun define-keymap (keymap)
  (add-keymap keymap (current-display)))

(defun load-rc ()
  (let ((user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".gollumrc"))))
    (if user-rc
	(load user-rc))))

(defun go-to-workspace (ws)
  (let* ((screen (current-screen nil))
	 (workspace (find-workspace ws (workspaces screen))))
    (when workspace
      (switch-to-workspace workspace screen)
      (message "~a" (name workspace))
      (update-mode-line-child screen :workspace (name workspace)))
    (unless workspace
      (message "workspace ~a does not exist." ws))))

(defun message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments)))

(defun permanent-message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments) nil))

(defvar *window-list-resource* :name
  "can be :name :class :instance")

(defun gen-window-list-entry (window)
  (let ((resource (case *window-list-resource*
		    (:name #'wm-name)
		    (:instance #'wm-instance)
		    (:class #'wm-class))))
    (format nil "~a" (funcall resource window))))

(defun window-list (&optional workspace)
  (let* ((windows (list-windows (or workspace (current-workspace nil))))
	 (*internal-window-horizontal-padding* 10)
	 (window-list (mapcar #'gen-window-list-entry windows)))
    (if window-list
	(screen-message (current-screen nil) (append (butlast window-list) (list (concat "^|" (car (last window-list))))))
	(message "No windows"))))

(defun gen-workspace-list-entry (workspace)
  (if (workspace-equal (current-workspace (screen workspace)) workspace)
      (format nil "^|~a-~a" (id workspace) (name workspace))
      (format nil "~a-~a" (id workspace) (name workspace))))

(defun workspace-list (&optional screen)
  (let ((workspaces (list-workspaces (or screen (current-screen nil))))
	(*internal-window-horizontal-padding* 10))
    (screen-message (current-screen nil)
		    (mapcar #'gen-workspace-list-entry workspaces))))

(defun date ()
  (let ((date (multiple-value-list (get-decoded-time))))
    ;;  (sec min hour date month year dow)
    (format nil "~a ~a ~a ~d:~2,'0d:~2,'0d ~a"
	    (nth (seventh date) '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	    (nth (1- (fifth date)) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	    (fourth date)
	    (third date)
	    (second date)
	    (first date)
	    (sixth date))))

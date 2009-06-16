(in-package :gollum)

(defmethod current-screen ((obj null))
  (current-screen (current-display)))

(defmethod current-workspace ((obj null))
  (current-workspace (current-screen nil)))

(defun raise-or-exec (program &key class name)
  (let ((win (find-matching-window (windows (current-screen nil)) :class class :name name)))
    (if win
	(raise-workspace-window win (current-screen nil))
	(exec program))))

(defun set-key (keymap key-desc action)
  (bind-key keymap key-desc action (current-display)))

(defun define-keymap (keymap)
  (add-keymap keymap (current-display)))

(defun unmap-messge-window ()
  (let ((message-window (message-window (current-screen nil))))
    (when (eql (xlib:window-map-state message-window) :viewable)
      (xlib:with-state (message-window)
	(xlib:unmap-window message-window)))))

(defun load-rc ()
  (let ((user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".gollumrc"))))
    (if user-rc
	(load user-rc))))

(defun go-to-workspace (ws)
  (let* ((screen (current-screen nil))
	 (workspace (find-workspace ws (workspaces screen))))
    (when workspace
      (switch-to-workspace workspace screen)
      (message "~a" (name workspace)))
    (unless workspace
      (message "workspace ~a does not exist." ws))))
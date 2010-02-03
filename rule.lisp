(in-package :gollum)

(defvar *all-rules* nil)

;; place-rule  (workspace what-if-not-found name-if-create)
(defmethod apply-place-rule ((window toplevel-window) place-rule)
  (let* ((screen (screen window))
	 (workspaces (workspaces screen))
	 (workspace (find-workspace (first place-rule) workspaces))
	 (policy (second place-rule))
	 (name (or (third place-rule) "default")))
    (when (null workspace)
      (case policy
	(:create (setf workspace (add-workspace-to-screen name screen)))
	(:current (setf workspace (current-workspace screen)))))
    (add-window window workspace)))

;; FIXME: shall we respect net-wm-desktop?
(defmethod place-window-according-to-rule ((window toplevel-window) match-rule place-rule)
  (when (apply #'match-window window match-rule)
    (apply-place-rule window place-rule)
    t))

(defun window-workspace-according-to-rule (window)
  (loop for (match-rule workspace) in *all-rules*
     when (apply #'match-window window match-rule)
     return (find-workspace workspace (workspaces (screen window)))
     finally (return (current-workspace (screen window)))))

(defmethod place-window ((window toplevel-window))
  (loop for rule in *all-rules*
     when (place-window-according-to-rule window (car rule) (cdr rule))
     return t
     finally (place-window-according-to-rule window nil '(nil :current nil))))

(defun add-rule (rule)
  (push rule *all-rules*))

(defun defrules (rules)
  (dolist (rule rules)
    (add-rule rule)))


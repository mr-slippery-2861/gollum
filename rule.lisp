(in-package :gollum)

(defvar *all-rules* nil)

;; place-rule  (workspace what-if-not-found name-if-create)
(defmethod apply-place-rule ((win window) place-rule)
  (let* ((s (screen win))
	 (workspaces (workspaces s))
	 (ws (find-workspace (first place-rule) workspaces))
	 (policy (second place-rule))
	 (name (or (third place-rule) "default")))
    (when (null ws)
      (case policy
	(:create (setf ws (add-workspace-to-screen name s)))
	(:current (setf ws (current-workspace s)))))
    (add-window win ws)))

(defmethod place-window-according-to-rule ((win window) match-rule place-rule)
  (when (apply #'match-window win match-rule)
    (apply-place-rule win place-rule)
    t))

(defmethod place-window ((win window))
  (loop for rule in *all-rules*
     when (place-window-according-to-rule win (car rule) (cdr rule))
     return t
     finally (place-window-according-to-rule win nil '(nil :current nil))))

(defun add-rule (rule)
  (push rule *all-rules*))

(defun defrule (rules)
  (dolist (rule rules)
    (add-rule rule)))


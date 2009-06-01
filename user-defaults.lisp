(in-package :gollum)

(define-keymap :root-map)

(set-key :top-map (kbd "C-t") :root-map)

(defcommand "emacs" ()
  (raise-or-exec "emacs" :class "Emacs"))

(set-key :root-map (kbd "e") "emacs")

(defcommand "firefox" ()
  (raise-or-exec "firefox" :class "Firefox"))

(set-key :root-map (kbd "f") "firefox")

(defcommand "test" ()
  (let ((win (find-matching-window (windows (current-screen nil)) :class "XTerm")))
    (if (eql :viewable (map-state win))
	(unmap-workspace-window win)
	(map-workspace-window win))
    (flush-display (current-display))))

(set-key :root-map (kbd "r") "test")

(defcommand "test-message" ()
  (screen-message (current-screen nil) (format nil "the internal time is ~a" (get-internal-real-time))))

(set-key :root-map (kbd "m") "test-message")
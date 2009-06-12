(in-package :gollum)

(defun setup-default-bindings (display)
  (bind-key :top-map "C-t" :root-map display)
  (bind-key :root-map "w" :window-map display)
  (bind-key :window-map "M" 'maximize display)
  (bind-key :window-map "r" 'restore display))


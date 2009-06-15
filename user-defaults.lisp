(in-package :gollum)

(defun setup-default-bindings (display)
  (bind-key :top-map "C-t" :root-map display)
  (bind-key :root-map "w" :window-map display)
  (bind-key :root-map "n" 'next-window display)
  (bind-key :root-map "p" 'prev-window display)
  (bind-key :root-map "k" 'kill display)
  (bind-key :window-map "M" 'maximize display)
  (bind-key :window-map "r" 'restore display))


(in-package :gollum)

(defvar *mode-line-gravity* :top
  "can be :top or :bottom")

(defvar *mode-line-height* 1)

(defvar *mode-line-border-width* 1)

(defvar *mode-line-border* "gray")

(defvar *mode-line-vertical-padding* 1)

(defvar *mode-line-horizontal-padding* 0)

(defvar *mode-line-background* "black")

(defvar *mode-line-foreground* "white")

;; how to represent the layout in list?
;; ---- ----- -----
;; |  | | 2 | | 3 |
;; |1 | ----- -----
;; |  | -----------
;; |  | |    4    |
;; ---- -----------
(defvar *mode-line-layout* '(:date :workspace))

(defun init-mode-line (screen)
  (let* ((mode-line (mode-line screen))
	 (font (output-font screen))
	 (ascent (xlib:font-ascent font))
	 (descent (xlib:font-descent font))
	 (width (- (width screen) (* 2 *mode-line-border-width*)))
	 (height (+ (* 2 *mode-line-vertical-padding*) (* *mode-line-height* (+ ascent descent)))))
    (xlib:with-state (mode-line)
      (setf (xlib:drawable-width mode-line) width
	    (xlib:drawable-height mode-line) height
	    (xlib:window-border mode-line) (alloc-color *mode-line-border* screen)
	    (xlib:drawable-border-width mode-line) *mode-line-border-width*)
      (case *mode-line-gravity*
	(:top (setf (xlib:drawable-x mode-line) 0
		    (xlib:drawable-y mode-line) 0))
	(:bottom (setf (xlib:drawable-x mode-line) 0
		       (xlib:drawable-y mode-line) (- (height screen) height)))))
    (if *mode-line-layout*
	(let* ((nchildren (length *mode-line-layout*))
	       (total-width width)
	       (cwidth nil)
	       (x 0))
	  (dolist (child *mode-line-layout*)
	    (setf cwidth (floor (/ total-width nchildren))
		  (gethash child (mode-line-children screen)) (alloc-xwindow *display*
									     :parent mode-line
									     :x x :y 0 :width cwidth :height height
									     :override-redirect :on
									     :save-under :on)
		  total-width (- total-width cwidth)
		  nchildren (1- nchildren)
		  x (+ x cwidth))
	    (xlib:map-window (gethash child (mode-line-children screen))))))))

(defun show-mode-line (screen)
  (if (eql (xlib:window-map-state (mode-line screen)) :unmapped)
      (let* ((mode-line (mode-line screen))
	     (height (xlib:drawable-height mode-line)))
	(setf (height screen) (- (height screen) height (* 2 *mode-line-border-width*)))
	(case *mode-line-gravity*
	  (:top (setf (y screen) (+ (y screen) height (* 2 *mode-line-border-width*)))))
	(xlib:map-window mode-line)
	(update-mode-line screen)
	(update-screen-windows-geometry screen)
	(schedule-timer (mode-line-timer screen)))))

(defun hide-mode-line (screen)
  (if (eql (xlib:window-map-state (mode-line screen)) :viewable)
      (let* ((mode-line (mode-line screen))
	     (height (xlib:drawable-height mode-line)))
	(setf (height screen) (+ (height screen) height (* 2 *mode-line-border-width*)))
	(case *mode-line-gravity*
	  (:top (setf (y screen) (- (y screen) height (* 2 *mode-line-border-width*)))))
	(xlib:unmap-window mode-line)
	(cancel-timer (mode-line-timer screen)))))

(defun toggle-mode-line (&optional screen)
  (let* ((screen (or screen (current-screen)))
	 (mode-line (mode-line screen)))
    (if (eql (xlib:window-map-state mode-line) :viewable)
	(hide-mode-line screen)
	(show-mode-line screen))))

(defun update-mode-line (screen)
  (update-mode-line-child screen :date (date)))

(defun update-mode-line-child (screen child content)
  (let ((window (gethash child (mode-line-children screen))))
    (if window
	(output-to-window screen window (mode-line-gc screen) :keep content))))
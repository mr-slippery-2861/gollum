(in-package :gollum)

(defvar *decorate-background-color* "blue")

(defvar *decorate-foreground-color* "black")

(defvar *decorate-vertical-padding* 1)


;; nw         top         ne
;;   ---------------------
;;  |        title       |
;;   ---------------------
;;  l|                   |r
;;  e|                   |i
;;  f|                   |g
;;  t|                   |h
;;   ---------------------t
;; sw        bottom       se

(defclass clx-decorate (decorate)
  ((window :initarg :window
	   :accessor window
	   :initform nil)
   (title :initarg :title
	  :accessor title
	  :initform nil)
   (nw :initarg :nw
       :accessor nw
       :initform nil)
   (ne :initarg :ne
       :accessor ne
       :initform nil)
   (sw :initarg :sw
       :accessor sw
       :initform nil)
   (se :initarg :se
       :accessor se
       :initform nil)
   (top :initarg :top
	:accessor top
	:initform nil)
   (bottom :initarg :bottom
	   :accessor bottom
	   :initform nil)
   (left :initarg :left
	 :accessor left
	 :initform nil)
   (right :initarg :right
	  :accessor right
	  :initform nil)))

(defmethod update-decorate ((decorate clx-decorate))
  (let* ((window (window decorate))
	 (screen (screen window))
	 (font (output-font screen))
	 (font-height (+ (xlib:font-ascent font) (xlib:font-descent font)))
	 (title-height (+ *decorate-vertical-padding* font-height))
;	 (button-size (floor (* 0.618 title-height)))
	 (w-width (xlib:drawable-width (xwindow window)))
	 (w-height (xlib:drawable-height (xwindow window)))
	 (title (title decorate)))
    (labels ((update-border (border position)
	       (let (x y width height)
		 (case position
		   (:nw (setf x 0 y 0 width *default-window-border-width* height *default-window-border-width*))
		   (:ne (setf x (+ w-width *default-window-border-width*) y 0
			      width *default-window-border-width* height *default-window-border-width*))
		   (:sw (setf x 0 y (+ w-height *default-window-border-width* title-height)
			      width *default-window-border-width* height *default-window-border-width*))
		   (:se (setf x (+ w-width *default-window-border-width*) y (+ w-height *default-window-border-width* title-height)
			      width *default-window-border-width* height *default-window-border-width*))
		   (:top (setf x *default-window-border-width* y 0 width w-width height *default-window-border-width*))
		   (:bottom (setf x *default-window-border-width* y (+ w-height *default-window-border-width* title-height)
				  width w-width height *default-window-border-width*))
		   (:left (setf x 0 y *default-window-border-width*
				width *default-window-border-width* height (+ w-height title-height)))
		   (:right (setf x (+ w-width *default-window-border-width*) y *default-window-border-width*
				 width *default-window-border-width* height (+ w-height title-height))))
		 (xlib:with-state (border)
		   (setf (xlib:drawable-x border) x
			 (xlib:drawable-y border) y
			 (xlib:drawable-width border) width
			 (xlib:drawable-height border) height)))))
      (xlib:with-state (title)
	(setf (xlib:drawable-width title) w-width
	      (xlib:drawable-height title) title-height))
      (mapc #'update-border (list (nw decorate) (ne decorate) (sw decorate) (se decorate) (top decorate) (bottom decorate) (left decorate) (right decorate)) '(:nw :ne :sw :se :top :bottom :left :right)))))

(defun make-decorate (window)
  (labels ((make-border (window position &optional (offset 0))
	     (let ((w-width (xlib:drawable-width (xwindow window)))
		   (w-height (xlib:drawable-height (xwindow  window)))
		   x y width height)
	       (case position
		 (:nw (setf x 0 y 0 width *default-window-border-width* height *default-window-border-width*))
		 (:ne (setf x (+ w-width *default-window-border-width*) y 0
			    width *default-window-border-width* height *default-window-border-width*))
		 (:sw (setf x 0 y (+ w-height *default-window-border-width* offset)
			    width *default-window-border-width* height *default-window-border-width*))
		 (:se (setf x (+ w-width *default-window-border-width*) y (+ w-height *default-window-border-width* offset)
			    width *default-window-border-width* height *default-window-border-width*))
		 (:top (setf x *default-window-border-width* y 0 width w-width height *default-window-border-width*))
		 (:bottom (setf x *default-window-border-width* y (+ w-height *default-window-border-width* offset)
				width w-width height *default-window-border-width*))
		 (:left (setf x 0 y *default-window-border-width*
			      width *default-window-border-width* height (+ w-height offset)))
		 (:right (setf x (+ w-width *default-window-border-width*) y *default-window-border-width*
			       width *default-window-border-width* height (+ w-height offset))))
	       (alloc-xwindow *display*
			      :parent (xmaster window)
			      :x x :y y :width width :height height
			      :background (alloc-color *default-window-border* (screen window))
			      :cursor (getf (cursor-drag-resize (display window)) position)
			      :event-mask '(:button-press :button-release :button-motion :owner-grab-button)
			      :override-redirect :on
			      :backing-store :always
			      :save-under :on))))
    (let* ((screen (screen window))
	   (font (output-font screen))
	   (width (xlib:drawable-width (xwindow window)))
	   (font-height (+ (xlib:font-ascent font) (xlib:font-descent font)))
	   (height (+ *decorate-vertical-padding* font-height))
	   (title (alloc-xwindow *display*
				 :parent (xmaster window)
				 :x *default-window-border-width*
				 :y *default-window-border-width*
				 :width width
				 :height height
				 :background (alloc-color *decorate-background-color* screen)
				 :event-mask '(:button-press
					       :button-release
					       :button-motion
					       :owner-grab-button
					       :exposure)
				 :override-redirect :on
				 :backing-store :always
				 :save-under :on))
	   ;; (x-button (xlib:create-window :parent title
	   ;; 				 :x (- width font-height 5)
	   ;; 				 :y (floor (/ (- height button-size 2) 2))
	   ;; 				 :width button-size :height button-size
	   ;; 				 :background (alloc-color "red" screen)
	   ;; 				 :border (alloc-color "gray" screen)
	   ;; 				 :border-width 1
	   ;; 				 :event-mask '(:button-press)
	   ;; 				 :override-redirect :on
	   ;; 				 :save-under :on))
	   (nw (make-border window :nw height))
	   (top (make-border window :top height))
	   (ne (make-border window :ne height))
	   (right (make-border window :right height))
	   (se (make-border window :se height))
	   (bottom (make-border window :bottom height))
	   (sw (make-border window :sw height))
	   (left (make-border window :left height)))
      (set-internal-window-type nw :border-nw)
      (set-internal-window-type top :border-top)
      (set-internal-window-type ne :border-ne)
      (set-internal-window-type right :border-right)
      (set-internal-window-type se :border-se)
      (set-internal-window-type bottom :border-bottom)
      (set-internal-window-type sw :border-sw)
      (set-internal-window-type left :border-left)
      (set-internal-window-type title :title)
      (mapc #'xlib:map-window (list title nw top ne right se bottom sw left))
      (make-instance 'clx-decorate
		     :window window
		     :title title
		     :nw nw
		     :top top
		     :ne ne
		     :right right
		     :se se
		     :bottom bottom
		     :sw sw
		     :left left))))

(defmethod title-height ((decorate clx-decorate)) ;FIXME: dirty hack
  (xlib:drawable-height (title decorate)))

(defmethod update-title ((decorate clx-decorate))
  (let ((screen (screen (window decorate))))
    (output-to-window screen (title decorate) (output-gc screen) :keep (get-window-name (window decorate)))))

(defmethod destroy-decorate ((decorate clx-decorate))
  (free-xwindow (title decorate) *display*)
  (dolist (xwindow (list (nw decorate) (top decorate) (ne decorate) (right decorate) (se decorate) (bottom decorate) (sw decorate) (left decorate)))
    (free-xwindow xwindow *display*)))

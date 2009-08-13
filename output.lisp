(in-package :gollum)

(defvar *message-window-gravity* :bottom-center)

(defvar *key-prompt-window-gravity* :pointer)

(defun hide-screen-message (screen)
  (let ((window (message-window screen)))
    (when (eql (xlib:window-map-state window) :viewable)
      (xlib:unmap-window window)
      (flush-display (display screen)))))

(defun hide-key-prompt (screen)
  (let ((window (key-prompt-window screen)))
    (when (eql (xlib:window-map-state window) :viewable)
      (xlib:unmap-window window)
      (flush-display (display screen)))))

(defun parse-color-controling (string)
  (let ((raw-string nil)
	(caret -3)
	(commands nil)
	(last-cmd nil)
	(last-cmd-start nil))
    (loop for x across string
       for current from 0 to (1- (length string))
	 do (cond
	      ((char= x #\^)
	       (case (- current caret)
		 (1 (setf raw-string (concat raw-string (string x))
			  caret -3
			  last-cmd nil))
		 (t (if last-cmd (setf commands (append commands (list (cons last-cmd-start last-cmd)))))
		    (setf caret current
			  last-cmd-start (length raw-string)
			  last-cmd "^"))))
	      ((or (digit-char-p x) (char= x #\*))
	       (case (- current caret)
		 (1 (setf last-cmd (concat last-cmd (string x))))
		 (2 (if (or (digit-char-p (char last-cmd 1)) (char= (char last-cmd 1) #\*))
			(setf last-cmd (concat last-cmd (string x)))
			(setf raw-string (concat raw-string (string x)))))
		 (t (setf raw-string (concat raw-string (string x))))))
	      (t
	       (case (- current caret)
		 (1 (setf last-cmd (concat last-cmd (string x))))
		 (t (setf raw-string (concat raw-string (string x)))))))
	 finally (if last-cmd (setf commands (append commands (list (cons last-cmd-start last-cmd))))))
    (values raw-string commands)))

(defun draw-substring (window gcontext x y string start end &optional (reverse-p nil))
  (when (> end start)
    (if reverse-p
	(let ((bg (xlib:gcontext-background gcontext))
	      (fg (xlib:gcontext-foreground gcontext)))
	  (xlib:with-gcontext (gcontext :foreground bg :background fg)
	    (xlib:draw-image-glyphs window gcontext x y (subseq string start end) :translate #'translate-id :size 16)))
	(xlib:draw-glyphs window gcontext x y (subseq string start end) :translate #'translate-id :size 16))))

;; FIXME:we share a single output-gc which may cause problems when two different threads access it
(defun colorized-output (screen window gcontext content &optional (offset-y 0))
  (labels ((unpack (command index)
	     (handler-case (char command index)
	       #+sbcl (sb-int:invalid-array-index-error () nil)))
	   (do-command (gc command)
	     (if command
		 (let ((1st (unpack command 1))
		       (2nd (unpack command 2))
		       (reverse-p nil))
		   (case 1st
		     (#\0 (setf (xlib:gcontext-foreground gc) (alloc-color "black" screen)))
		     (#\1 (setf (xlib:gcontext-foreground gc) (alloc-color "red" screen)))
		     (#\2 (setf (xlib:gcontext-foreground gc) (alloc-color "green" screen)))
		     (#\3 (setf (xlib:gcontext-foreground gc) (alloc-color "yellow" screen)))
		     (#\4 (setf (xlib:gcontext-foreground gc) (alloc-color "blue" screen)))
		     (#\5 (setf (xlib:gcontext-foreground gc) (alloc-color "magenta" screen)))
		     (#\6 (setf (xlib:gcontext-foreground gc) (alloc-color "cyan" screen)))
		     (#\7 (setf (xlib:gcontext-foreground gc) (alloc-color "white" screen)))
		     (#\* (setf (xlib:gcontext-foreground gc) (alloc-color *foreground-color* screen)))
		     (#\R (setf reverse-p t))
		     (#\r nil)
		     (t nil))
		   (case 2nd
		     (#\0 (setf (xlib:gcontext-background gc) (alloc-color "black" screen)))
		     (#\1 (setf (xlib:gcontext-background gc) (alloc-color "red" screen)))
		     (#\2 (setf (xlib:gcontext-background gc) (alloc-color "green" screen)))
		     (#\3 (setf (xlib:gcontext-background gc) (alloc-color "yellow" screen)))
		     (#\4 (setf (xlib:gcontext-background gc) (alloc-color "blue" screen)))
		     (#\5 (setf (xlib:gcontext-background gc) (alloc-color "magenta" screen)))
		     (#\6 (setf (xlib:gcontext-background gc) (alloc-color "cyan" screen)))
		     (#\7 (setf (xlib:gcontext-background gc) (alloc-color "white" screen)))
		     (#\* (setf (xlib:gcontext-background gc) (alloc-color *background-color* screen)))
		     (t nil))
		   reverse-p))))
    (multiple-value-bind (raw-string commands) (parse-color-controling content)
      (let* ((font (xlib:gcontext-font gcontext))
	     (start 0)
	     (end (if commands (caar commands) (length raw-string)))
	     (total (1- (length commands)))
	     (x *internal-window-horizontal-padding*)
	     (y (+ offset-y *internal-window-vertical-padding* (xlib:font-ascent font)))
	     (reverse-p nil))
	(when (string= (cdar commands) "^|")
	  (xlib:draw-rectangle window gcontext 0 (+ offset-y *internal-window-vertical-padding*) (xlib:drawable-width window) (+ (xlib:font-ascent font) (xlib:font-descent font)) t)
	  (setf commands (cdr commands)
		end (if commands (caar commands) (length raw-string))
		total (1- total)
		(xlib:gcontext-foreground gcontext) (alloc-color *background-color* screen)
		(xlib:gcontext-background gcontext) (alloc-color *foreground-color* screen)))
	(loop for (next-start . command) in commands
	   for current from 0 to total
	   do (progn
		(draw-substring window gcontext x y raw-string start end reverse-p)
		(setf x (+ (xlib:text-width font raw-string :start start :end end :translate #'translate-id) x)
		      start next-start
		      end (if (< current total)
			      (car (elt commands (1+ current)))
			      (length raw-string))
		      reverse-p (do-command gcontext command)))
	   finally (draw-substring window gcontext x y raw-string start end reverse-p))
	(setf (xlib:gcontext-foreground gcontext) (alloc-color *foreground-color* screen)
	      (xlib:gcontext-background gcontext) (alloc-color *background-color* screen))))))

(defmethod output-to-window ((screen screen) xwindow gcontext gravity content)
  "CONTENT is either a string or a list of string indicating multi-line drawing"
  (if content
      (let* ((font (xlib:gcontext-font gcontext))
	     (ascent (xlib:font-ascent font))
	     (descent (xlib:font-descent font))
	     (lines (if (typep content 'string) (list content) content)))
	(setup-window-for-drawing-glyphs screen xwindow gravity gcontext font (mapcar #'parse-color-controling lines))
	(loop for line in lines
	   for y = 0 then (+ y ascent descent)
	   do (colorized-output screen xwindow gcontext line y))
	(flush-display (display screen)))))

(defmethod screen-message ((screen screen) message &optional (time-out-p t))
  (let ((message-window (message-window screen)))
    (output-to-window screen message-window (output-gc screen) *message-window-gravity* message)
    (if time-out-p
	(schedule-timer (message-timer screen))
	(cancel-timer (message-timer screen)))))

(defmethod screen-prompt-key ((screen screen) key-desc &optional (time-out-p nil))
  (let ((key-prompt-window (key-prompt-window screen)))
    (output-to-window screen key-prompt-window (output-gc screen) *key-prompt-window-gravity* key-desc)
    (if time-out-p
	(schedule-timer (key-prompt-timer screen))
	(cancel-timer (key-prompt-timer screen)))))
(in-package :gollum)

(defvar *message-window-gravity* :bottom-center)

(defun hide-screen-message (screen)
  (xlib:unmap-window (message-window screen))
  (flush-display (display screen)))

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
		 (2 (setf last-cmd (concat last-cmd (string x))))
		 (t (setf raw-string (concat raw-string (string x))))))
	      (t
	       (case (- current caret)
		 (1 (setf last-cmd (concat last-cmd (string x))))
		 (t (setf raw-string (concat raw-string (string x)))))))
	 finally (if last-cmd (setf commands (append commands (list (cons last-cmd-start last-cmd))))))
    (values raw-string commands)))

(defun draw-substring (window gcontext x y string start end)
  (if (> end start)
      (xlib:draw-glyphs window gcontext x y (subseq string start end))))

(let ((gc nil))
  (defun colorized-output (screen window gcontext font content)
    (labels ((unpack (command index)
	       (handler-case (char command index)
		 (sb-int:invalid-array-index-error () nil)))
	     (do-command (gc command)
	       (let ((1st (unpack command 1))
		     (2nd (unpack command 2)))
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
		   (t nil)))))
      (multiple-value-bind (raw-string commands) (parse-color-controling content)
	(if commands
	    (let ((start 0)
		  (end (caar commands))
		  (total (1- (length commands)))
		  (x *internal-window-horizontal-padding*)
		  (y (+ *internal-window-vertical-padding* (xlib:font-ascent font))))
	      (if gc
		  (setf (xlib:gcontext-foreground gc) (alloc-color *foreground-color* screen)
			(xlib:gcontext-background gc) (alloc-color *background-color* screen))
		  (setf gc (xlib:create-gcontext :drawable window
						 :foreground (alloc-color *foreground-color* screen)
						 :background (alloc-color *foreground-color* screen)
						 :font font)))
;	      (xlib:copy-gcontext gcontext gc) ;why raise "unimplemented" error?
	      (loop for (next-start . command) in commands
		 for current from 0 to total
		 do (progn
		      (draw-substring window gc x y raw-string start end)
		      (setf x (+ (xlib:text-width font raw-string :start start :end end) x)
			    start next-start
			    end (if (< current total)
				    (car (elt commands (1+ current)))
				    (length raw-string)))
		      (do-command gc command))
		 finally (draw-substring window gc x y raw-string start end)))
	    (draw-substring window gcontext *internal-window-horizontal-padding* (+ *internal-window-vertical-padding* (xlib:font-ascent font)) raw-string 0 (length raw-string))))))
  (defun destroy-colorizing-gc ()
    (when gc
	(xlib:free-gcontext gc)
	(setf gc nil)))
  (defun get-colorizing-gc ()
    gc))

(defun colorized-screen-message (screen message)
  (let* ((font (message-font screen))
	 (message-window (message-window screen))
	 (message-gc (message-gc screen)))
    (setup-window-for-drawing-glyphs screen message-window *message-window-gravity* message-gc font (parse-color-controling message))
    (colorized-output screen message-window message-gc font message)
    (schedule-timer (message-timer screen))
    (flush-display (display screen))))

(defmethod screen-message ((screen screen) message &optional (time-out-p t))
  (let* ((font (message-font screen))
	 (message-window (message-window screen))
	 (message-gc (message-gc screen)))
    (setup-window-for-drawing-glyphs screen message-window *message-window-gravity* message-gc font (parse-color-controling message))
    (colorized-output screen message-window message-gc font message)
    (when time-out-p
      (schedule-timer (message-timer screen)))
    (flush-display (display screen))))

(defun message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments)))

(defun permanent-message (control-string &rest format-arguments)
  (screen-message (current-screen nil) (apply #'format nil control-string format-arguments) nil))
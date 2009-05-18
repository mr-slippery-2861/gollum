(in-package :gollum)

(defun key-hash (mod keysym)
  (+ (ash mod 32) keysym))

(defun make-key-mod-map ()
  (multiple-value-bind (shift-keycodes lock-keycodes control-keycodes mod1-keycodes mod2-keycodes mod3-keycodes mod4-keycodes mod5-keycodes) (xlib:modifier-mapping *display*)
    (declare (ignore shift-keycodes lock-keycodes control-keycodes))
    (labels ((mod-keycode->mod (keycode)
	       (let ((keysym (xlib:keycode->keysym *display* keycode 0)))
		 (when (zerop keysym)
		     (setf keysym (xlib:keycode->keysym *display* keycode 1)))
		 (keysym->keysym-name keysym)))
	     (names-in-mod-p (names mod)
	       (loop for name in names
		    thereis (find name mod :test #'string=))))
      (let ((mods (mapcar (lambda (codes) (mapcar #'mod-keycode->mod codes))
			  (list mod1-keycodes mod2-keycodes mod3-keycodes mod4-keycodes mod5-keycodes)))
	    map)
	(dolist (key '((:alt "Alt_L" "Alt_R") (:meta "Meta_L" "Meta_R") (:super "Super_L" "Super_R") (:hyper "Hyper_L" "Hyper_R")))
	  (cond
	    ((names-in-mod-p (cdr key) (first mods)) (push (list (car key) :mod-1) map))
	    ((names-in-mod-p (cdr key) (second mods)) (push (list (car key) :mod-2) map))
	    ((names-in-mod-p (cdr key) (third mods)) (push (list (car key) :mod-3) map))
	    ((names-in-mod-p (cdr key) (fourth mods)) (push (list (car key) :mod-4) map))
	    ((names-in-mod-p (cdr key) (fifth mods)) (push (list (car key) :mod-5) map))))
	map))))
	       
(defun abbr->mod (abbr)
  "ABBR may be S(shift),A(alt),C(control),s(super),H(hyper),M(meta)"
  (case abbr
    (#\S :shift)))

(defun kbd (string)
  "STRING should be description of single key event.
modifiers as:S for shift,A for alt,C for control,
s for super,H for hyper,M for meta,while the last character
should be printable key,like number,alphabet,etc.
example:\"C-t\""
  (let* ((keys (reverse (split-string (string-trim " " string) "-")))
	 (mod 0)
	 (keysym (keysym-name->keysym (car keys))))
    (mapc (lambda (modifier)
	    (case (char modifier 0)	;shift lock control mod1 mod2 mod3 mod4 mod5
	      (#\S (setf mod (logior mod #b10000000)))
	      (#\L (setf mod (logior mod #b01000000)))
	      (#\C (setf mod (logior mod #b00100000)))
	      (#\1 (setf mod (logior mod #b00010000)))
	      (#\2 (setf mod (logior mod #b00001000)))
	      (#\3 (setf mod (logior mod #b00000100)))
	      (#\4 (setf mod (logior mod #b00000010)))
	      (#\5 (setf mod (logior mod #b00000001))))) (cdr keys))
    (key-hash mod keysym)))

(defun bind-key (keymap key action)
  "ACTION is either a command or a keymap while KEYMAP and KEY are what their names indicate.
example:(bind-key *top-map* (kbd \"C-h\") '*help-map*)"
  (setf (gethash key keymap) action))

(defmacro define-keymap (keymap)
  "example:(define-keymap *some-map*)"
  `(defvar ,keymap (make-hash-table)))

(define-keymap *top-map*)

(define-keymap *root-map*)
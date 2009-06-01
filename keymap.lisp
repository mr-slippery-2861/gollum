(in-package :gollum)

(defun key->hash (state keysym)
  (+ (ash state 32) keysym))

(defun hash->key (key)
  (values (ash key -32)
	  (logand key #xffffffff)))

(defun make-key-mod-map (xdisplay)
  (multiple-value-bind (shift-keycodes lock-keycodes control-keycodes mod1-keycodes mod2-keycodes mod3-keycodes mod4-keycodes mod5-keycodes) (xlib:modifier-mapping xdisplay)
    (labels ((mod-keycode->mod (keycode)
	       (let ((keysym (xlib:keycode->keysym xdisplay keycode 0)))
		 (when (zerop keysym)
		     (setf keysym (xlib:keycode->keysym xdisplay keycode 1)))
		 (keysym->keysym-name keysym)))
	     (names-in-mod-p (names mod)
	       (loop for name in names
		    thereis (find name mod :test #'string=))))
      (let ((mods (mapcar (lambda (codes) (mapcar #'mod-keycode->mod codes))
			  (list shift-keycodes lock-keycodes control-keycodes mod1-keycodes mod2-keycodes mod3-keycodes mod4-keycodes mod5-keycodes)))
	    map)
	(dolist (key '((:alt "Alt_L" "Alt_R") (:meta "Meta_L" "Meta_R") (:super "Super_L" "Super_R") (:hyper "Hyper_L" "Hyper_R") (:num-lock "Num_Lock")))
	  (cond
	    ((names-in-mod-p (cdr key) (first mods)) (push (list (car key) nil) map))
	    ((names-in-mod-p (cdr key) (second mods)) (push (list (car key) nil) map))
	    ((names-in-mod-p (cdr key) (third mods)) (push (list (car key) nil) map))
	    ((names-in-mod-p (cdr key) (fourth mods)) (push (list (car key) :mod-1) map))
	    ((names-in-mod-p (cdr key) (fifth mods)) (push (list (car key) :mod-2) map))
	    ((names-in-mod-p (cdr key) (sixth mods)) (push (list (car key) :mod-3) map))
	    ((names-in-mod-p (cdr key) (seventh mods)) (push (list (car key) :mod-4) map))
	    ((names-in-mod-p (cdr key) (eighth mods)) (push (list (car key) :mod-5) map))))
	(values map mods (append shift-keycodes lock-keycodes control-keycodes mod1-keycodes mod2-keycodes mod3-keycodes mod4-keycodes mod5-keycodes ))))))

(defun abbr->mod (abbr key-mod-map)
  "ABBR may be S(super),A(alt),C(control),H(hyper),M(meta)"
  (labels ((key->mod (key)
	     (cadr (assoc key key-mod-map))))
    (case abbr
      (#\A (key->mod :alt))
      (#\C :control)
      (#\S (key->mod :super))
      (#\H (key->mod :hyper))
      (#\M (key->mod :meta)))))

(defun kbd-internal (string key-mod-map)
  "STRING should be description of single key event.
modifiers as:A for alt,C for control,
S for super,H for hyper,M for meta,while the last character
should be printable key,like number,alphabet,etc.
example:\"C-t\""
  (let* ((keys (reverse (split-string (string-trim " " string) "-")))
	 (keysym (keysym-name->keysym (car keys))))
    (key->hash (apply #'xlib:make-state-mask
		     (mapcar (lambda (modifier) (abbr->mod (char modifier 0) key-mod-map)) (cdr keys)))
	      keysym)))



(in-package :gollum)


;; clx error handling

  ;; *xerror-vector*
  ;;    unknown-error
  ;;    request-error                              ; 1  bad request code
  ;;    value-error                                ; 2  integer parameter out of range
  ;;    window-error                               ; 3  parameter not a Window
  ;;    pixmap-error                               ; 4  parameter not a Pixmap
  ;;    atom-error                                 ; 5  parameter not an Atom
  ;;    cursor-error                               ; 6  parameter not a Cursor
  ;;    font-error                                 ; 7  parameter not a Font
  ;;    match-error                                ; 8  parameter mismatch
  ;;    drawable-error                             ; 9  parameter not a Pixmap or Window
  ;;    access-error                               ; 10 attempt to access private resource
  ;;    alloc-error                                ; 11 insufficient resources
  ;;    colormap-error                             ; 12 no such colormap
  ;;    gcontext-error                             ; 13 parameter not a GContext
  ;;    id-choice-error                            ; 14 invalid resource ID for this connection
  ;;    name-error                                 ; 15 font or color name does not exist
  ;;    length-error                               ; 16 request length incorrect;
  ;;                                               ;    internal Xlib error
  ;;    implementation-error                       ; 17 server is defective

(defparameter *error-handlers* (make-array 24 :initial-element #'xlib:default-error-handler))

(defmacro define-error-handler (error-key actual-keys &body body)
  (let ((fn-name (gensym)))
    `(labels ((,fn-name (display error-key &rest key-vals &key asynchronous ,@actual-keys &allow-other-keys)
		,@body))
       (setf (elt *error-handlers* (position ,error-key xlib::*xerror-vector*)) #',fn-name))))

;; errors in clx:
;; access-error alloc-error atom-error closed-display colormap-error connection-failure cursor-error device-busy drawable-error font-error gcontext-error id-choice-error implementation-error length-error lookup-error match-error missing-parameter name-error pixmap-error reply-length-error reply-timeout request-error resource-error sequence-error server-disconnect unexpected-reply unknown-error value-error window-error
(define-error-handler 'xlib:window-error (resource-id)
  (dformat 1 "window-error received")
  (if asynchronous
      t
      (apply #'error 'xlib:window-error :display display :error-key 'xlib:window-error key-vals)))

(define-error-handler 'xlib:drawable-error ()
  (dformat 1 "drawable-error received")
  (if asynchronous
      t
      (apply #'error 'xlib:drawable-error :display display :error-key 'xlib:drawable-error key-vals)))

;; gollum error handling

(define-condition no-such-window (error)
  ((xwindow :initarg :xwindow
	    :reader xwindow)))
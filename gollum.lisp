(in-package :gollum)

;; copy from stumpwm

;; errors in clx:
;; access-error alloc-error atom-error closed-display colormap-error connection-failure cursor-error device-busy drawable-error font-error gcontext-error id-choice-error implementation-error length-error lookup-error match-error missing-parameter name-error pixmap-error reply-length-error reply-timeout request-error resource-error sequence-error server-disconnect unexpected-reply unknown-error value-error window-error
(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (cond
    ;; ignore asynchronous window errors
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     (dformat 4 "Ignoring error: ~s~%" error-key))
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running.")
     (throw :top-level :quit))
     ;; all other asynchronous errors are printed.
     (asynchronous
      (message "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))


(in-package :gollum)

(defun split-seq (seq separators &key test default-value)
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
	     then (position-if (complement #'sep) seq :start j)
	     as j = (position-if #'sep seq :start (or i 0))
	     while i
	     collect (subseq seq i j)
	     while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))

(defun split-string (string &optional (separators "
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
***If SEPARATORS is absent, it defaults to \"[ \f\t\n\r\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that.

Modifies the match data; use `save-match-data' if necessary."
  (split-seq string separators :test #'char= :default-value '("")))

(defmacro concat (&rest strings)
  `(concatenate 'string ,@strings))

(defun symbol->function (symbol)
  (handler-case (symbol-function symbol)
    (undefined-function () nil)
    (type-error () nil)))

;; from stumpwm,thanks:)
(defvar *debug-level* 0)

(defvar *debug-stream* *error-output*)

(defun dformat (level control-string &rest format-arguments)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (decode-universal-time (get-universal-time))
      (format *debug-stream* "~2,'0d:~2,'0d:~2,'0d " h m sec))
    (write-line (apply #'format nil control-string format-arguments) *debug-stream*)))

;; I cannot draw chinese characters without this function,thanks to stumpwm again
(defun translate-id (src src-start src-end font dst dst-start)
  "A simple replacement for xlib:translate-default.  just the
identity with a range check."
  (let ((min (xlib:font-min-char font))
        (max (xlib:font-max-char font)))
    (decf src-end)
    (if (stringp src)      ; clx does this test so i guess it's needed
        (loop for i from src-start to src-end
              for j from dst-start
              as c = (char-code (char src i))
              if (<= min c max) do (setf (aref dst j) c)
              ;; replace unknown characters with question marks
              else do (setf (aref dst j) (char-code #\?))
              finally (return i))
        (loop for i from src-start to src-end
              for j from dst-start
              as c = (elt src i)
              as n = (if (characterp c) (char-code c) c)
              if (and (integerp n) (<= min n max)) do (setf (aref dst j) n)
              ;; ditto
              else do (setf (aref dst j) (char-code #\?))
              finally (return i)))))
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
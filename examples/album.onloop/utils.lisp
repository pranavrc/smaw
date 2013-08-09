(in-package :album-onloop)

(defun string-split (string delim)
  ;; Splits a string into substrings around the delimiter.
  (loop for x = 0 then (1+ y)
     as y = (position delim string :start x)
     collect (subseq string x y)
     while y))

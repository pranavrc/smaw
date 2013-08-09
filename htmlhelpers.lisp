(in-package :cl-smaw)

(defun generate-embed-html (uri &key (width 300) (height 380))
  "Generate HTML markup for embedding spotify widgets."
  (format nil "<iframe src=\"https://embed.spotify.com/?uri=~a\" width=\"~d\" height=\"~d\"
frameborder=\"0\" allowtransparency=\"true\"></iframe>" uri width height))

(defun format-plist (plist &optional (include-keys? t) &rest keys-to-include)
  "Recursive function to pretty-print the plist returned by searches and lookups."
  (loop for (key value) on plist by #'cddr
     when (member key
		  (if keys-to-include
		      keys-to-include
		      plist) :test #'equal)
     collect (cond
	       ((member key '(:album :availability) :test #'equal)
		(if include-keys?
		    (format nil "~a:~a" (string key) (format-plist value include-keys?))
		    (format nil "~a" (format-plist value include-keys?))))

	       ((member key '(:external-ids :artists :tracks :albums) :test #'equal)
		(if include-keys?
		    (format nil "~a:~a" (string key)
			    (mapcar #'(lambda (each) (format-plist each include-keys?)) value))
		    (format nil "~a" (mapcar #'(lambda (each) (format-plist each include-keys?)) value))))

	       (t
		(if include-keys?
		    (format nil "~a:~a" key value)
		    (format nil "~a" value))))))

(defun markup-plist (plist)
  (loop for (key value) on plist by #'cddr collect
       (who:with-html-output-to-string (*standard-output* nil)
	 (:b (who:str (concatenate 'string (string-downcase (string key)) " : ")))
	 (:i (who:str value)))))

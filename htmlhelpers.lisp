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

(defun entry (category plist)
  (if (typep (getf plist category) 'list)
      (if (typep (first (getf plist category)) 'list)
	  (string-from-plists category plist)
	  (string-from-plist category plist))
      (who:with-html-output-to-string (*standard-output* nil)
	(:p (who:str (concatenate 'string
				  (who:with-html-output-to-string (*standard-output* nil)
				    (:strong (who:str (string-capitalize category))))
				  " "
				  (if (member category '(:href :artist-id)) ;;; Hack for album-onloop links.
				      (who:with-html-output-to-string (*standard-output* nil)
					(:a :href (concatenate 'string
							       "/"
							       (second (string-split 
									(getf plist category) #\:))
							       "/" (getf plist category))
					    (who:str (getf plist category))))
				      (if (typep (getf plist category) 'number)
					  (write-to-string (getf plist category))
					  (string (getf plist category))))))))))

(defun string-from-plists (category plist)
  (concatenate 'string
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:strong (who:str (string-capitalize category)))
		 (:br))
	       (with-output-to-string (s)
		 (loop for each-plist in (getf plist category) do
		      (format s
			      (who:with-html-output-to-string (*standard-output* nil)
				(:div :id "inner"
				      (who:str (with-output-to-string (d)
						 (loop for (key value) on each-plist by #'cddr do
						      (format d (entry key each-plist))))))))))))

(defun string-from-plist (category plist)
  (who:with-html-output-to-string (*standard-output* nil)
    (:strong (who:str (string-capitalize category)))
    (:br)
    (:div :id "inner"
	  (who:str 
	   (with-output-to-string (s)
	     (loop for (key value) on (getf plist category) by #'cddr
		do (format s
			   (entry key (getf plist category)))))))
    (:br)))

(defun album-search-html (plist)
  (concatenate 'string
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (entry :popularity plist) (list #\Newline)
	       (string-from-plists :artists plist) (list #\Newline)
	       (string-from-plists :external-ids plist) (list #\Newline)
	       (string-from-plist :availability plist) (list #\Newline)))

(defun artist-search-html (plist)
  (concatenate 'string
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (entry :popularity plist) (list #\Newline)))

(defun track-search-html (plist)
  (concatenate 'string
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (entry :popularity plist) (list #\Newline)
	       (string-from-plist :album plist) (list #\Newline) 
	       (string-from-plists :external-ids plist) (list #\Newline)
	       (string-from-plists :artists plist) (list #\Newline)
	       (entry :track-number plist) (list #\Newline)
	       (entry :length plist) (list #\Newline)))

(defun album-lookup-html (plist)
  (concatenate 'string
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:div :id "widget" (who:str (generate-embed-html (getf plist :href))))
		 (:hr))
	       (entry :name plist) (list #\Newline)
	       (entry :artist plist) (list #\Newline)
	       (entry :artist-id plist) (list #\Newline)
	       (string-from-plist :availability plist) (list #\Newline)
	       (string-from-plists :tracks plist) (list #\Newline)))

(defun artist-lookup-html (plist)
  (concatenate 'string
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:div :id "widget" (who:str (generate-embed-html (getf plist :href))))
		 (:hr))
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (string-from-plists :albums plist)  (list #\Newline)))

(defun track-lookup-html (plist)
  (concatenate 'string	 
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:div :id "widget" (who:str (generate-embed-html (getf plist :href))))
		 (:hr))
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (entry :length plist) (list #\Newline)
	       (entry :popularity plist) (list #\Newline)
	       (entry :available plist) (list #\Newline)
	       (entry :track-number plist) (list #\Newline)
	       (string-from-plist :album plist) (list #\Newline)
	       (string-from-plists :artists plist) (list #\Newline)
	       (string-from-plist :availability plist) (list #\Newline)
	       (string-from-plists :external-ids plist) (list #\Newline)))

(in-package :smaw)

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
  (if (typep (cdr (assoc category plist)) 'list)
      (if (typep (first (cadr (assoc category plist))) 'list)
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
									(cdr (assoc category plist)) #\:))
							       "/" (cdr (assoc category plist)))
					    (who:str (cdr (assoc category plist)))))
				      (if (typep (cdr (assoc category plist)) 'number)
					  (write-to-string (cdr (assoc category plist)))
					  (string (cdr (assoc category plist)))))))))))

(defun string-from-plists (category plist)
  (concatenate 'string
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:strong (who:str (string-capitalize category)))
		 (:br))
	       (with-output-to-string (s)
		 (loop for each-plist in (cdr (assoc category plist)) do
		      (format s
			      (who:with-html-output-to-string (*standard-output* nil)
				(:div :id "inner"
				      (who:str (with-output-to-string (d)
						 (loop for sub-list in each-plist do
						      (format d (entry (first sub-list) each-plist))))))))))))

(defun string-from-plist (category plist)
  (who:with-html-output-to-string (*standard-output* nil)
    (:strong (who:str (string-capitalize category)))
    (:br)
    (:div :id "inner"
	  (who:str 
	   (with-output-to-string (s)
	     (loop for each-list in (cdr (assoc category plist))
		do (format s
			   (entry (car each-list)
				  (cdr (assoc category plist))))))))
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
		 (:div :id "widget" (who:str (generate-embed-html (cdr (assoc :href plist)))))
		 (:hr))
	       (entry :name plist) (list #\Newline)
	       (entry :artist plist) (list #\Newline)
	       (entry :artist-id plist) (list #\Newline)
	       (string-from-plist :availability plist) (list #\Newline)
	       (string-from-plists :tracks plist) (list #\Newline)))

(defun artist-lookup-html (plist)
  (concatenate 'string
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:div :id "widget" (who:str (generate-embed-html (cdr (assoc :href plist)))))
		 (:hr))
	       (entry :name plist) (list #\Newline)
	       (entry :href plist) (list #\Newline)
	       (string-from-plists :albums plist)  (list #\Newline)))

(defun track-lookup-html (plist)
  (concatenate 'string	 
	       (who:with-html-output-to-string (*standard-output* nil)
		 (:div :id "widget" (who:str (generate-embed-html (cdr (assoc :href plist)))))
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

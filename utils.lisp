(in-package :cl-smaw)

;; Get JSON response from an URL.
(defun get-json-response (url)
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))

;; Extract Name, URI and Artist values from the JSON.
(defun extract-values (json type &optional (pretty nil))
  (let* ((list-of-tracks (gethash type json)))
    (if pretty
	(mapcar #'(lambda (track artists uri) (format nil "~a, By: ~a, URI: ~a"
						      track 
						      (format nil "~{~a~^ and ~}" artists)
						      uri))
		(mapcar #'(lambda (each) (gethash "name" each)) list-of-tracks)
		(mapcar #'(lambda (each-list) (mapcar #'(lambda (each)
							  (gethash "name" each)) each-list))
			(mapcar #'(lambda (each) (gethash "artists" each)) list-of-tracks))
		(mapcar #'(lambda (each) (gethash "href" each)) list-of-tracks))
	(mapcar #'(lambda (each) (list :track (gethash "name" each)
				       :artists 
				       (mapcar #'(lambda (each-list)
						   (gethash "name" each-list))
					       (gethash "artists" each))
				       :uri (gethash "href" each))) list-of-tracks))))

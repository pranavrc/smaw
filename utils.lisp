(in-package :cl-smaw)

;; Get JSON response from an URL.
(defun get-json-response (url)
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))

;; Extract Name, URI and Artist values from the JSON.
(defun extract-values (json type &optional (detailed nil))
  (let* ((list-of-tracks (gethash type json)))
    (if detailed
	(mapcar #'(lambda (track artists uri) (format nil "~a, By: ~a, URI: ~a"
						      track 
						      (format nil "~{~a~^ and ~}" artists)
						      uri))
		(mapcar #'(lambda (each) (gethash "name" each)) list-of-tracks)
		(mapcar #'(lambda (each-list) (mapcar #'(lambda (each)
							  (gethash "name" each)) each-list))
			(mapcar #'(lambda (each) (gethash "artists" each)) list-of-tracks))
		(mapcar #'(lambda (each) (gethash "href" each)) list-of-tracks))
	(mapcar #'(lambda (each) (gethash "href" each)) list-of-tracks))))

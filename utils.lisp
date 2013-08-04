(in-package :cl-smaw)

;; Get JSON response from an URL.
(defun get-json-response (url)
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))

;; Extract Name, URI and Artist values from the JSON.
(defun album-and-track-parser (json albums-or-tracks &optional (pretty nil))
  (let* ((list-of-tracks (gethash albums-or-tracks json)))
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

;; Parses information from a JSON response for album queries.
(defun album-parser (album-object)
  (list :name (gethash "name" album-object)
	:popularity (gethash "popularity" album-object)
	:external-ids (mapcar #'(lambda (each) (list
						:type (gethash "type" each)
						:id (gethash "id" each)))
			      (gethash "external-ids" album-object))
	:href (gethash "href" album-object)
	:artists (mapcar #'(lambda (each) (list
					   :href (gethash "href" each)
					   :name (gethash "name" each)))
			 (gethash "artists" album-object))
	:availability (list :territories (gethash "territories"
						  (gethash "availability" album-object)))))

;; Parses information from a JSON response for track queries.
(defun track-parser (track-object)
  (list :name (gethash "name" track-object)
	:album (let* ((album-key (gethash "album" track-object)))
		 (list :name (gethash "name" album-key)
		       :released (gethash "released" album-key)
		       :href (gethash "href" album-key)
		       :availability (list :territories (gethash "territories"
								 (gethash "availability" album-key)))))
	:popularity (gethash "popularity" track-object)
	:external-ids (mapcar #'(lambda (each) (list
						:type (gethash "type" each)
						:id (gethash "id" each)))
			      (gethash "external-ids" track-object))
	:length (gethash "length" track-object)
	:href (gethash "href" track-object)
	:artists (mapcar #'(lambda (each) (list
					   :href (gethash "href" each)
					   :name (gethash "name" each)))
			 (gethash "artists" track-object))
	:track-number (gethash "track-number" track-object)))

;; Parses information from a JSON response for artist queries.
(defun artist-parser (artist-object)
  (list :name (gethash "name" artist-object)
	:popularity (gethash "popularity" artist-object)
	:href (gethash "href" artist-object)))
	

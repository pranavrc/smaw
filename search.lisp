(in-package :cl-smaw)

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
(defun album-search-parser (album-search-object)
  (list :name (gethash "name" album-search-object)
	:popularity (gethash "popularity" album-search-object)
	:external-ids (mapcar #'(lambda (each) (list
						:type (gethash "type" each)
						:id (gethash "id" each)))
			      (gethash "external-ids" album-search-object))
	:href (gethash "href" album-search-object)
	:artists (mapcar #'(lambda (each) (list
					   :href (gethash "href" each)
					   :name (gethash "name" each)))
			 (gethash "artists" album-search-object))
	:availability (list :territories (gethash "territories"
						  (gethash "availability" album-search-object)))))

;; Parses information from a JSON response for track queries.
(defun track-search-parser (track-search-object)
  (list :name (gethash "name" track-search-object)
	:album (let* ((album-key (gethash "album" track-search-object)))
		 (list :name (gethash "name" album-key)
		       :released (gethash "released" album-key)
		       :href (gethash "href" album-key)
		       :availability (list :territories (gethash "territories"
								 (gethash "availability" album-key)))))
	:popularity (gethash "popularity" track-search-object)
	:external-ids (mapcar #'(lambda (each) (list
						:type (gethash "type" each)
						:id (gethash "id" each)))
			      (gethash "external-ids" track-search-object))
	:length (gethash "length" track-search-object)
	:href (gethash "href" track-search-object)
	:artists (mapcar #'(lambda (each) (list
					   :href (gethash "href" each)
					   :name (gethash "name" each)))
			 (gethash "artists" track-search-object))
	:track-number (gethash "track-number" track-search-object)))

;; Parses information from a JSON response for artist queries.
(defun artist-search-parser (artist-search-object)
  (list :name (gethash "name" artist-search-object)
	:popularity (gethash "popularity" artist-search-object)
	:href (gethash "href" artist-search-object)))

(in-package :cl-smaw)

;; Parses information from a JSON response for album queries.
(defun album-search-parser (album-search-object)
  (let* ((toplevel-keys (make-plist album-search-object
				    :name :popularity :href)))
    (setf (getf toplevel-keys :external-ids)
	  (mapcar #'(lambda (each) (make-plist each :type :id))
		  (gethash "external-ids" album-search-object))
	  (getf toplevel-keys :artists)
	  (mapcar #'(lambda (each) (make-plist each :href :name))
		  (gethash "artists" album-search-object))
	  (getf toplevel-keys :availability)
	  (make-plist (gethash "availability" album-search-object) :territories))
    toplevel-keys))

;; Parses information from a JSON response for track queries.
(defun track-search-parser (track-search-object)
  (let* ((toplevel-keys (make-plist track-search-object
				    :name :popularity :length :href :track-number)))
    (setf (getf toplevel-keys :album)
	  (let* ((album-key (gethash "album" track-search-object)))
	    (let* ((album-properties (make-plist album-key :name :released :href)))
	      (setf (getf album-properties :availability)
		    (make-plist (gethash "availability" album-key) :territories))
	      album-properties))
	  (getf toplevel-keys :external-ids)
	  (mapcar #'(lambda (each) (make-plist each :type :id))
		  (gethash "external-ids" track-search-object))
	  (getf toplevel-keys :artists)
	  (mapcar #'(lambda (each) (make-plist each :href :name))
		  (gethash "artists" track-search-object)))
    toplevel-keys))

;; Parses information from a JSON response for artist queries.
(defun artist-search-parser (artist-search-object)
  (make-plist artist-search-object :name :popularity :href))

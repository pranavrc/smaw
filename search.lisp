(in-package :cl-smaw)

;; Parses information from a JSON response for album queries.
(defun album-search-parser (album-search-object)
  (let* ((toplevel-keys (make-plist album-search-object
				    :name :popularity :href
				    :external-ids :artists :availability)))
    toplevel-keys))

;; Parses information from a JSON response for track queries.
(defun track-search-parser (track-search-object)
  (let* ((toplevel-keys (make-plist track-search-object
				    :name :popularity :length :href :track-number
				    :artists :external-ids)))
    (setf (getf toplevel-keys :album)
	  (make-plist (gethash "album" track-search-object) :name :released
		      :href :availability))
    toplevel-keys))

;; Parses information from a JSON response for artist queries.
(defun artist-search-parser (artist-search-object)
  (make-plist artist-search-object :name :popularity :href))

;; Parses information from a JSON response for track lookups.
(defun track-lookup-parser (track-lookup-object)
  (let* ((toplevel-keys (make-plist track-lookup-object :name :available 
				    :popularity :length :href :track-number
				    :external-ids :artists :availability)))
    (setf (getf toplevel-keys :album)
	  (make-plist (gethash "album" track-lookup-object)
		      :name :released :href))
    toplevel-keys))

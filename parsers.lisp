(in-package :smaw)

(defun album-search-parser (album-search-object)
  "Parses information from a JSON response for album queries."
  (let ((toplevel-keys (make-plist album-search-object
                                    :name :popularity :href
                                    :external-ids :artists :availability)))
    toplevel-keys))


(defun track-search-parser (track-search-object)
  "Parses information from a JSON response for track queries."
  (let ((toplevel-keys (make-plist track-search-object
                                    :name :popularity :length :href :track-number
                                    :artists :external-ids)))
    (setf (getf toplevel-keys :album)
          (make-plist (gethash "album" track-search-object) :name :released
                      :href :availability))
    toplevel-keys))

(defun artist-search-parser (artist-search-object)
  "Parses information from a JSON response for artist queries."
  (make-plist artist-search-object :name :popularity :href))

(defun album-lookup-parser (album-lookup-object)
  "Parses information from a JSON response for album lookups."
  (let* ((toplevel-keys (make-plist album-lookup-object :artist-id :name :artist
                                    :external-ids :released :href :availability)))
    (setf (getf toplevel-keys :tracks)
          (mapcar #'(lambda (each) (make-plist each
                                               :available :track-number :popularity
                                               :external-ids :length :href
                                               :artists :disc-number :name))
                  (gethash "tracks" album-lookup-object)))
    toplevel-keys))

(defun track-lookup-parser (track-lookup-object)
  "Parses information from a JSON response for track lookups."
  (let ((toplevel-keys (make-plist track-lookup-object :name :available
                                    :popularity :length :href :track-number
                                    :external-ids :artists :availability)))
    (setf (getf toplevel-keys :album)
          (make-plist (gethash "album" track-lookup-object)
                      :name :released :href))
    toplevel-keys))

(defun artist-lookup-parser (artist-lookup-object)
  "Parses information from a JSON response for artist lookups."
  (let* ((toplevel-keys (make-plist artist-lookup-object :name :href)))
    (setf (getf toplevel-keys :albums)
          (mapcar #'(lambda (each) (make-plist (gethash "album" each)
                                               :artist-id :name :artist :external-ids
                                               :released :href :availability))
                  (gethash "albums" artist-lookup-object)))
    toplevel-keys))

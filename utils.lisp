(in-package :cl-smaw)

(enable-interpol-syntax)

(defun get-json-response (url)
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))

(defun construct-url (url-type parameters &optional (type "track"))
  (let* ((encoded-parameters (url-encode parameters :utf-8))
         (search-url        #?"http://ws.spotify.com/search/1/${type}.json?q=${encoded-parameters}")
         (track-lookup-url  #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}")
         (album-lookup-url  #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}&extras=trackdetail")
         (artist-lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}&extras=albumdetail"))
    (case url-type
      (search search-url)
      (track-lookup track-lookup-url)
      (album-lookup album-lookup-url)
      (artist-lookup artist-lookup-url)
      (otherwise (error "Specify one of: '(search track-lookup album-lookup artist-lookup)")))))


(defun make-plist (json-object &rest keys)
  "Makes a property list out of a list of keys in a JSON object."
  (let ((list-object ()))
    (loop for key in keys do
         (setf (getf list-object key)
               (case key
                 (:artists (mapcar #'(lambda (each) (make-plist each :href :name))
                                   (gethash "artists" json-object)))

                 (:external-ids (mapcar #'(lambda (each) (make-plist each :type :id))
                                        (gethash "external-ids" json-object)))

                 (:availability (make-plist (gethash "availability" json-object) :territories))
                 (otherwise (gethash (string-downcase (string key)) json-object)))))
    list-object))

(defun filter-results (results &rest filters)
  "Generates a list of results that match only the specified results (key-value pairs)."
  (remove nil
          (loop for each-result in results collect
               (loop for (key value) on filters by #'cddr
                  when (string-equal value (getf each-result key))
                  do (return each-result)))))

(defun api-service (service query)
  "Rudimentary exception handling for all services."
  (handler-case
      (progn
        (case service
          (album-search (album-search query))
          (artist-search (artist-search query))
          (track-search (track-search query))
          (album-lookup (album-lookup query))
          (artist-lookup (artist-lookup query))
          (track-lookup (track-lookup query))
          (otherwise (format t "Specify one of: '(album-search artist-search track-search album-lookup artist-lookup track-lookup)"))))
    (error (e) (print e))))

(defun album-search (search-term)
  (mapcar #'(lambda (each) (album-search-parser each))
          (gethash "albums"
                   (get-json-response (construct-url 'search search-term "album")))))

(defun album-lookup (lookup-term)
  (album-lookup-parser (gethash "album"
                                (get-json-response
                                 (construct-url 'album-lookup lookup-term "album")))))

(defun artist-search (search-term)
  (mapcar #'(lambda (each) (artist-search-parser each))
          (gethash "artists"
                   (get-json-response
                    (construct-url 'search search-term "artist")))))

(defun artist-lookup (lookup-term)
  (artist-lookup-parser (gethash "artist"
                                 (get-json-response
                                  (construct-url 'artist-lookup lookup-term "artist")))))

(defun track-search (search-term)
  (mapcar #'(lambda (each) (track-search-parser each))
          (gethash "tracks"
                   (get-json-response
                    (construct-url 'search search-term)))))

(defun track-lookup (lookup-term)
  (track-lookup-parser (gethash "track"
                                (get-json-response
                                 (construct-url 'track-lookup lookup-term)))))

(defun is-uri (input)
  "Scans string to check if Spotify URI."
  (and (ppcre:scan "^spotify:.+$" input) t))

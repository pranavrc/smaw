(in-package :cl-smaw)

(enable-interpol-syntax)

;; Get JSON response from an URL.
(defun get-json-response (url)
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))

;; Generic function to construct a search or lookup URL.
(defun construct-url (search-or-lookup parameters &optional (type "track"))
  (let* ((encoded-parameters (url-encode parameters :utf-8))
         (search-url #?"http://ws.spotify.com/search/1/${type}.json?q=${encoded-parameters}")
	 (track-lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}")
         (album-lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}&extras=trackdetail")
	 (artist-lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}&extras=albumdetail"))
    (case search-or-lookup
      (search search-url)
      (track-lookup track-lookup-url)
      (album-lookup album-lookup-url)
      (artist-lookup artist-lookup-url)
      (otherwise (error "Specify either 'search or 'lookup symbol.")))))

;; Make a property list out of a list of keys in a JSON object.
(defun make-plist (json-object &rest keys)
  (let* ((list-object ()))
    (loop for each-key in keys do
	 (setf (getf list-object each-key) 
	       (case each-key
		 (:artists (mapcar #'(lambda (each) (make-plist each :href :name))
				   (gethash "artists" json-object)))
		 (:external-ids (mapcar #'(lambda (each) (make-plist each :type :id))
					(gethash "external-ids" json-object)))
		 (:availability (make-plist (gethash "availability" json-object) :territories))
		 (otherwise (gethash (string-downcase (string each-key)) json-object)))))
    list-object))

;; Album search and lookup.
(defun album-search (search-term) (construct-url 'search search-term "album"))
(defun album-lookup (lookup-term) (construct-url 'album-lookup lookup-term "album"))

;; Artist search and lookup.
(defun artist-search (search-term) (construct-url 'search search-term "artist"))
(defun artist-lookup (lookup-term) (construct-url 'artist-lookup lookup-term "artist"))

;; Track search and lookup.
(defun track-search (search-term) (construct-url 'search search-term))
(defun track-lookup (lookup-term) (construct-url 'track-lookup lookup-term))

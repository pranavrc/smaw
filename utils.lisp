(in-package :cl-smaw)

(enable-interpol-syntax)

(defun get-json-response (url)
  "Gets JSON response from a URL."
  (parse
   (http-request url
                 :method :get
                 :preserve-uri t
                 :want-stream t)))


(defun construct-url (url-type parameters &optional (type "track"))
  "Constructs a URL."
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


(defun album-search (search-term)
  (mapcar #'(lambda (each) (album-search-parser each))
          (gethash "albums"
                   (get-json-response (construct-url 'search search-term "album")))))
(defun album-lookup (lookup-term)
  (album-lookup-parser (gethash "album"
                                (get-json-response (construct-url 'album-lookup lookup-term "album")))))

(defun artist-search (search-term)
  (mapcar #'(lambda (each) (artist-search-parser each))
          (gethash "artists"
                   (get-json-response (construct-url 'search search-term "artist")))))
(defun artist-lookup (lookup-term)
  (artist-lookup-parser (gethash "artist"
                                 (get-json-response (construct-url 'artist-lookup lookup-term "artist")))))

(defun track-search (search-term)
  (mapcar #'(lambda (each) (track-search-parser each))
          (gethash "tracks"
                   (get-json-response (construct-url 'search search-term)))))

(defun track-lookup (lookup-term)
  (track-lookup-parser (gethash "track"
                                (get-json-response (construct-url 'track-lookup lookup-term)))))

(in-package :cl-smaw)

(enable-interpol-syntax)

;; Generic function to construct a search or lookup URL.
(defun construct-url (search-or-lookup parameters &optional (type "track"))
  (let* ((encoded-parameters (url-encode parameters :utf-8))
         (search-url #?"http://ws.spotify.com/search/1/${type}.json?q=${encoded-parameters}")
         (lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}"))
    (case search-or-lookup
      (search search-url)
      (lookup lookup-url)
      (otherwise (error "Specify either 'search or 'lookup symbol.")))))

;; Album search and lookup.
(defun album-search (search-term) (construct-url 'search search-term "album"))
(defun album-lookup (lookup-term) (construct-url 'lookup lookup-term "album"))

;; Artist search and lookup.
(defun artist-search (search-term) (construct-url 'search search-term "artist"))
(defun artist-lookup (lookup-term) (construct-url 'lookup lookup-term "artist"))

;; Track search and lookup.
(defun track-search (search-term) (construct-url 'search search-term))
(defun track-lookup (lookup-term) (construct-url 'lookup lookup-term))

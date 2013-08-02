(in-package :cl-smaw)

(enable-interpol-syntax)

(defun construct-url (search-or-lookup parameters &optional (type "track"))
  (let* ((encoded-parameters (url-encode parameters :utf-8))
         (search-url #?"http://ws.spotify.com/search/1/${type}.json?q=${encoded-parameters}")
         (lookup-url #?"http://ws.spotify.com/lookup/1/.json?uri=${encoded-parameters}"))
    (case search-or-lookup
      (search search-url)
      (lookup lookup-url)
      (otherwise (error "Specify either 'search or 'lookup")))))

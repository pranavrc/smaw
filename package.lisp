(in-package :cl-user)

(defpackage :smaw
  (:use :cl :drakma :cl-json :cl-interpol :cl-who)
  (:export #:get-json-response
		   #:construct-url
		   #:make-plist
		   #:filter-results
		   #:api-service

		   ;; Search functions.
		   #:album-search
		   #:artist-search
		   #:track-search

		   ;; Lookup functions.
		   #:album-lookup
		   #:artist-lookup
		   #:track-lookup

		   ;; URI helpers.
		   #:is-uri
		   #:string-split
		   #:get-type-and-id-from-uri

		   ;; Symbols
		   #:search-album
		   #:search-artist
		   #:search-track
		   #:lookup-album
		   #:lookup-artist
		   #:lookup-track

		   ;; HTML Helpers.
		   #:generate-embed-html
		   #:format-plist
		   #:markup-plist
		   #:entry
		   #:string-from-plists
		   #:string-from-plist
		   #:album-search-html
		   #:album-lookup-html
		   #:artist-search-html
		   #:artist-lookup-html
		   #:track-search-html
		   #:track-lookup-html))

(defpackage :cl-smaw
  (:use :cl :drakma :yason))

(in-package :cl-smaw)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *lookup* "http://ws.spotify.com/lookup/1/.json?uri=~a")
(defparameter *search* "http://ws.spotify.com/search/1/~a.json?q=~a"))

(defun get-json-response (url)
  (yason:parse
   (drakma:http-request url
			:method :get
			:preserve-uri t
			:want-stream t)))

(defmacro construct-url (lookup-or-search params &optional (type "track"))
  `(format nil
	   ,@(case lookup-or-search
		   ((lookup)
		    (list *lookup*
			  (drakma:url-encode params :utf-8)))
		   ((search)
		    (list *search*
			  type
			  (drakma:url-encode params :utf-8))))))

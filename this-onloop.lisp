(restas:define-module :restas.this-onloop (:use :cl))

(in-package :restas.this-onloop)

(restas:debug-mode-on)
(setf (who:html-mode) :html5)
(defparameter *not-found* "Something like that doesn't seem to exist on Spotify...<i>yet</i>.")
(defparameter *invalid-url* "Yeah, well, y'know, that's just like, uh, a bad URL, man.")

(defmacro response-template (&body response)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title "Here you go.")
       (:link :rel "stylesheet" :href (restas:genurl 'css))
       (:link :rel "shortcut icon" :type "image/x-icon" :href (restas:genurl 'favicon)))
      (:body
       (:div :id "response"
	     (who:str ,@response))))))

(restas:define-route main ("")
  (pathname "/home/vanharp/workbase/smaw/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "/home/vanharp/workbase/smaw/res/index.css"))

(restas:define-route favicon ("favicon.ico")
  (pathname "/home/vanharp/workbase/smaw/res/favicon.ico"))

(restas:define-route album-lookup-route ("album/:(query)")
  (response-template
    (handler-case
	(progn
	  (let ((params (smaw:string-split query #\!)))
	    (if (smaw:is-uri (first params))
		(smaw:album-lookup-html (smaw:album-lookup (first params)))
		(if (second params)
		    (smaw:album-lookup-html
		     (smaw:album-lookup 
		      (cdr (assoc :href (nth (- (parse-integer (second params)) 1)
					     (smaw:album-search (first params)))))))
		    (smaw:album-lookup-html
		     (smaw:album-lookup (cdr (assoc :href (first
							   (smaw:album-search 
							    (first params)))))))))))
      (error (e) *not-found*))))

(restas:define-route artist-lookup-route ("artist/:(query)")
  (response-template
    (handler-case
	(progn
	  (let ((params (smaw:string-split query #\!)))
	    (if (smaw:is-uri (first params))
		(smaw:artist-lookup-html (smaw:artist-lookup (first params)))
		(if (second params)
		    (smaw:artist-lookup-html
		     (smaw:artist-lookup 
		      (cdr (assoc :href (nth (- (parse-integer (second params)) 1)
					     (smaw:artist-search (first params)))))))
		    (smaw:artist-lookup-html
		     (smaw:artist-lookup (cdr (assoc :href (first
							    (smaw:artist-search
							     (first params)))))))))))
      (error (e) *not-found*))))

(restas:define-route track-lookup-route ("track/:(query)")
  (response-template
    (handler-case
	(progn
	  (let ((params (smaw:string-split query #\!)))
	    (if (smaw:is-uri (first params))
		(smaw:track-lookup-html (smaw:track-lookup (first params)))
		(if (second params)
		    (smaw:track-lookup-html
		     (smaw:track-lookup 
		      (cdr (assoc :href (nth (- (parse-integer (second params)) 1)
					     (smaw:track-search (first params)))))))
		    (smaw:track-lookup-html
		     (smaw:track-lookup (cdr (assoc :href (first
							   (smaw:track-search
							    (first params)))))))))))
      (error (e) *not-found*))))

(restas:define-route album-search-route ("albums/:(query)")
  (response-template
    (handler-case
	(progn
	  (let* ((params (smaw:string-split query #\!))
		 (search-results (smaw:album-search (first params))))
	    (if search-results
		(format nil "~{~a~^<hr />~}"
			(mapcar #'smaw:album-search-html (smaw:album-search (first params))))
		(error ""))))
      (error (e) *not-found*))))

(restas:define-route artist-search-route ("artists/:(query)")
  (response-template
    (handler-case
	(progn
	  (let* ((params (smaw:string-split query #\!))
		 (search-results (smaw:artist-search (first params))))
	    (if search-results
		(format nil "~{~a~^<hr />~}"
			(mapcar #'smaw:artist-search-html (smaw:artist-search (first params))))
		(error ""))))
      (error (e) *not-found*))))

(restas:define-route track-search-route ("tracks/:(query)")
  (response-template
    (handler-case
	(progn
	  (let* ((params (smaw:string-split query #\!))
		 (search-results (smaw:track-search (first params))))
	    (if search-results
		(format nil "~{~a~^<hr />~}"
			(mapcar #'smaw:track-search-html (smaw:track-search (first params))))
		(error ""))))
      (error (e) *not-found*))))

(restas:define-route reviews ("reviews/:(query)")
  (restas:redirect (format nil "http://album.onloop.net/?q=~a" query)))
	  
(restas:define-route not-found ("*any")
  (response-template *invalid-url*))

(restas:start '#:restas.this-onloop :port 8082)

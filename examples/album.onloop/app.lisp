(restas:define-module :restas.album-onloop (:use :cl :album-onloop))

(in-package :restas.album-onloop)

(restas:debug-mode-on)

(defmacro response-template (&body response)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title "album.onloop")
       (:link :rel "stylesheet" :href (restas:genurl 'css))
       (:link :rel "shortcut icon" :type "image/x-icon" :href (restas:genurl 'favicon)))
      (:body
       (:p :id "response"
	   ,@response)))))

(restas:define-route main ("")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.css"))

(restas:define-route favicon ("favicon.ico")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/favicon.ico"))

(restas:define-route query (":(type)/:(query)/l")
  (response-template 
    (:div :id "response"
	  (who:str
	   (handler-case
	       (let* ((params (album-onloop::string-split query #\!)))
		 (if (cl-smaw::is-uri (first params))
		     (cond
		       ((string= type "album") (cl-smaw::album-lookup-html (cl-smaw::album-lookup (first params))))
		       ((string= type "artist") (cl-smaw::artist-lookup-html (cl-smaw::artist-lookup (first params))))
		       ((string= type "track") (cl-smaw::track-lookup-html (cl-smaw::track-lookup (first params))))
		       (t "Invalid URL."))
		     (cond
		       ((string= type "album")
			(if (second params)
			    (cl-smaw::album-lookup-html
			     (cl-smaw::album-lookup 
			      (getf (nth (- (parse-integer (second params)) 1)
					 (cl-smaw::album-search (first params))) :href)))
			    (cl-smaw::album-lookup-html
			     (cl-smaw::album-lookup (getf (first
							   (cl-smaw::album-search (first params))) :href)))))
		       ((string= type "artist")
			(if (second params)
			    (cl-smaw::artist-lookup-html
			     (cl-smaw::artist-lookup (getf (nth (- (parse-integer (second params)) 1)
								(cl-smaw::artist-search (first params))) :href)))
			    (cl-smaw::artist-lookup-html (cl-smaw::artist-lookup
							  (getf (first (cl-smaw::artist-search (first params)))
								:href)))))
		       ((string= type "track")
			(if (second params)
			    (cl-smaw::track-lookup-html (cl-smaw::track-lookup
							 (getf (nth (- (parse-integer (second params)) 1)
								    (cl-smaw::track-search (first params))) :href)))
			    (cl-smaw::track-lookup-html (cl-smaw::track-lookup
							 (getf (first (cl-smaw::track-search (first params))) :href)))))
		       (t "Invalid URL."))))
	     (error (e) "Oops, something went wrong."))))))

(restas:define-route not-found ("*any")
  (response-template (:div (who:str "Invalid URL."))))

(restas:start '#:restas.album-onloop :port 8080)

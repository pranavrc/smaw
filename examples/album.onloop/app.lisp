(restas:define-module :restas.album-onloop (:use :cl :album-onloop))

(in-package :restas.album-onloop)

(restas:debug-mode-on)
(setf (who:html-mode) :html5)

(defmacro response-template (&body response)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title "album.onloop")
       (:link :rel "stylesheet" :href (restas:genurl 'css))
       (:link :rel "shortcut icon" :type "image/x-icon" :href (restas:genurl 'favicon)))
      (:body
       ,@response))))

(restas:define-route main ("")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.css"))

(restas:define-route favicon ("favicon.ico")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/favicon.ico"))

(restas:define-route query (":(type)/:(query)")
  (defparameter response
    (handler-case
	(let ((params (cl-smaw::string-split query #\!))
	      (lookup-query nil)
	      (search-query nil))
	  (progn
	    (cond
	      ((string= type "album")
	       (setf lookup-query t)
	       (setf (symbol-function 's-lookup-html) (function cl-smaw::album-lookup-html))
	       (setf (symbol-function 's-lookup) (function cl-smaw::album-lookup))
	       (setf (symbol-function 's-search) (function cl-smaw::album-search)))
	      ((string= type "artist")
	       (setf lookup-query t)
	       (setf (symbol-function 's-lookup-html) (function cl-smaw::artist-lookup-html))
	       (setf (symbol-function 's-lookup) (function cl-smaw::artist-lookup))
	       (setf (symbol-function 's-search) (function cl-smaw::artist-search)))
	      ((string= type "track")
	       (setf lookup-query t)
	       (setf (symbol-function 's-lookup-html) (function cl-smaw::track-lookup-html))
	       (setf (symbol-function 's-lookup) (function cl-smaw::track-lookup))
	       (setf (symbol-function 's-search) (function cl-smaw::track-search)))
	      ((string= type "albums")
	       (setf search-query t)
	       (setf (symbol-function 's-search-html) (function cl-smaw::album-search-html))
	       (setf (symbol-function 's-search) (function cl-smaw::album-search)))
	      ((string= type "artists")
	       (setf search-query t)
	       (setf (symbol-function 's-search-html) (function cl-smaw::artist-search-html))
	       (setf (symbol-function 's-search) (function cl-smaw::artist-search)))
	      ((string= type "tracks")
	       (setf search-query t)
	       (setf (symbol-function 's-search-html) (function cl-smaw::track-search-html))
	       (setf (symbol-function 's-search) (function cl-smaw::track-search)))
	      (t (error "Yeah, well, y'know, that's just like, uh, a bad URL, man."))))
	  (handler-case
	      (progn
		(cond
		  ((and lookup-query (not search-query))
		   (if (cl-smaw::is-uri (first params))
		       (s-lookup-html (s-lookup (first params)))
		       (if (second params)
			   (s-lookup-html
			    (s-lookup 
			     (getf (nth (- (parse-integer (second params)) 1)
					(s-search (first params))) :href)))
			   (s-lookup-html
			    (s-lookup (getf (first
					     (s-search (first params))) :href))))))
		  ((and search-query (not lookup-query))
		   (let ((search-results (s-search (first params))))
		     (if search-results
			 (format nil "~{~a~^<hr />~}"
				 (mapcar #'s-search-html (s-search (first params))))
			 (error ""))))))
	    (error (e) (error "Something like that doesn't seem to exist on Spotify...<i>yet</i>."))))
      (error (e) e)))
  (response-template (:div :id "response" (who:str response))))

(restas:define-route not-found ("*any")
  (response-template (:div :id "response" "Yeah, well, y'know, that's just like, uh, a bad URL, man.")))

(restas:start '#:restas.album-onloop :port 8080)

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
       (:div :id "response"
	   ,@response)))))

(restas:define-route main ("")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.css"))

(restas:define-route favicon ("favicon.ico")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/favicon.ico"))

(restas:define-route query (":(type)/:(query)")
  (response-template 
    (who:str
     (handler-case
	 (let ((params (cl-smaw::string-split query #\!)))
	   (progn
	     (cond
	       ((string= type "album")
		(setf (symbol-function 's-lookup-html) (function cl-smaw::album-lookup-html))
		(setf (symbol-function 's-lookup) (function cl-smaw::album-lookup))
		(setf (symbol-function 's-search) (function cl-smaw::album-search)))
	       ((string= type "artist")
		(setf (symbol-function 's-lookup-html) (function cl-smaw::artist-lookup-html))
		(setf (symbol-function 's-lookup) (function cl-smaw::artist-lookup))
		(setf (symbol-function 's-search) (function cl-smaw::artist-search)))
	       ((string= type "track")
		(setf (symbol-function 's-lookup-html) (function cl-smaw::track-lookup-html))
		(setf (symbol-function 's-lookup) (function cl-smaw::track-lookup))
		(setf (symbol-function 's-search) (function cl-smaw::track-search)))
	       (t "Invalid URL.")))
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
       (error (e) "Oops, something went wrong.")))))

(restas:define-route not-found ("*any")
  (response-template (:div (who:str "Invalid URL."))))

(restas:start '#:restas.album-onloop :port 8080)

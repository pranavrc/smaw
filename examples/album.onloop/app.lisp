(restas:define-module :restas.album-onloop (:use :cl :album-onloop))

(in-package :restas.album-onloop)

(restas:debug-mode-on)

(restas:define-route main ("")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.css"))

(restas:define-route query ("album/:(query)/l")
  (let ((params (album-onloop::string-split query #\!)))
    (if (cl-smaw::is-uri (first params))
	(cl-smaw::album-lookup-html (cl-smaw::album-lookup (first params)))
	(if (second params)
	    (cl-smaw::album-search-html (nth (- (parse-integer (second params)) 1)
				    (cl-smaw::album-search (first params))))
	    (cl-smaw::album-search-html (first (cl-smaw::album-search (first params))))))))

(restas:start '#:restas.album-onloop :port 8080)

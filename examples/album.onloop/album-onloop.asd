(defpackage #:album-onloop-system
	(:use :cl :asdf))

(in-package :album-onloop-system)

(defsystem album-onloop
	:name "album.onloop"
	:author "Pranav Ravichandran"
	:description "Proof of concept app for smaw, and an extension to http://album.onloop.net/"
	:license "MIT"
	:serial t
	:components
	((:file "package")
	 (:file "app"))
	 :depends-on (restas smaw cl-who))

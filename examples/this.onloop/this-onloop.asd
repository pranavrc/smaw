(defpackage #:this-onloop-system
	(:use :cl :asdf))

(in-package :this-onloop-system)

(defsystem this-onloop
	:name "this.onloop"
	:author "Pranav Ravichandran"
	:description "Proof of concept app for smaw, and an extension to http://album.onloop.net/"
	:license "MIT"
	:serial t
	:components
	((:file "package")
	 (:file "app"))
	 :depends-on (restas smaw cl-who))

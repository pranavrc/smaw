(defpackage #:ao-extension
	(:use :cl :asdf))

(in-package :ao-extension)

(defsystem ao-ex
	:name "ao-ex"
	:author "Pranav Ravichandran"
	:description "Proof of concept app for cl-smaw, and an extension to http://album.onloop.net/"
	:license "MIT"
	:serial t
	:components
	((:file "app")
	 (:file "package"))
	 :depends-on (restas cl-smaw))

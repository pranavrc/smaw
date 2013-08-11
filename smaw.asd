(defpackage #:smaw-system
  (:use :cl :asdf))

(in-package :smaw-system)

(defsystem smaw
  :name "smaw"
  :author "Pranav Ravichandran"
  :description "Common Lisp Wrapper for the Spotify Metadata API."
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "parsers")
   (:file "htmlhelpers")
   (:file "this-onloop"))
   :depends-on (drakma cl-json cl-interpol restas cl-who))

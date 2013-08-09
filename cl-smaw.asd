(defpackage #:cl-smaw-system
  (:use :cl :asdf))

(in-package :cl-smaw-system)

(defsystem cl-smaw
  :name "cl-smaw"
  :author "Pranav Ravichandran"
  :description "Common Lisp Wrapper for the Spotify Metadata API."
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "parsers")
   (:file "htmlhelpers"))
   :depends-on (drakma yason cl-interpol cl-who))

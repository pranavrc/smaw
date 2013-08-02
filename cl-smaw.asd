(defpackage #:cl-smaw-system
  (:use :cl :asdf))

(in-package :cl-smaw-system)

(defsystem cl-smaw
  :name "sudoku-solver"
  :author "Pranav Ravichandran"
  :description "CL bindings for the Spotify API"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "cl-smaw"))
   :depends-on (drakma yason))

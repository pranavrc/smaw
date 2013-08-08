(restas:define-module :restas.album-onloop (:use :cl :album-onloop))

(in-package :restas.album-onloop)

(restas:debug-mode-on)

(restas:define-route main ("")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.html"))

(restas:define-route css ("index.css")
  (pathname "~/workbase/cl-smaw/examples/album.onloop/res/index.css"))

(restas:start '#:restas.album-onloop :port 8080)

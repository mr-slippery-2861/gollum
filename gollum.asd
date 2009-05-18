(defpackage :gollum-system
  (:use :cl
	:asdf))

(in-package :gollum-system)

(defsystem :gollum
  :name "gollum"
  :author "Shelling Chen - fvwmer@gmail.com"
  :maintainer "Shelling Chen - fvwmer@gmail.com"
  :description "My own wm"
  :depends-on (:clx)
  :components ((:file "package")
	       (:file "gollum")))
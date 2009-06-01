(defpackage :gollum-system
  (:use :cl
	:asdf))

(in-package :gollum-system)

(defsystem :gollum
  :name "gollum"
  :author "Shelling Chen - fvwmer@gmail.com"
  :maintainer "Shelling Chen - fvwmer@gmail.com"
  
  :description "My own wm"
  :depends-on (:clx :bordeaux-threads)
  :components ((:file "package")
	       (:file "util")
	       (:file "command")
	       (:file "keysymdef")
	       (:file "keymap")
	       (:file "window" :depends-on ("package"))
	       (:file "workspace" :depends-on ("package" "window"))
	       (:file "rule")
	       (:file "color")
	       (:file "font")
	       (:file "timer")
	       (:file "screen" :depends-on ("package" "window" "workspace"))
	       (:file "display" :depends-on ("package" "window"))
	       (:file "event" :depends-on ("display"))
	       (:file "gollum")
	       (:file "user" :depends-on ("package" "display"))
	       (:file "user-defaults")))
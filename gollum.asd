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
  :serial t
  :components ((:file "package")
	       (:file "util"); :depends-on ("package"))
	       (:file "command")
	       (:file "keysymdef")
	       (:file "keymap"); :depends-on ("package"))
	       (:file "window"); :depends-on ("package"))
	       (:file "workspace"); :depends-on ("package" "window"))
	       (:file "rule")
	       (:file "color")
	       (:file "font")
	       (:file "timer")
	       (:file "screen"); :depends-on ("package" "window" "workspace"))
	       (:file "error")
	       (:file "display"); :depends-on ("package" "window"))
	       (:file "message")
	       (:file "input")
	       (:file "event"); :depends-on ("display"))
	       (:file "gollum")
	       (:file "user"); :depends-on ("package" "display"))
	       (:file "user-defaults")
	       ))
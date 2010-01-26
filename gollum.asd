(defpackage :gollum-system
  (:use :cl
	:asdf))

(in-package :gollum-system)

(defsystem :gollum
  :name "gollum"
  :author "Shelling Chen - fvwmer@gmail.com"
  :maintainer "Shelling Chen - fvwmer@gmail.com"
  :description "My own wm"
  :depends-on (:clx :bordeaux-threads :babel)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "clxwrapper")
	       (:file "command")
	       (:file "keysymdef")
	       (:file "keymap")
	       (:file "window")
	       (:file "netwm.lisp")
	       (:file "decorate")
	       (:file "clx-decorate")
	       (:file "focus")
	       (:file "workspace")
	       (:file "rule")
	       (:file "color")
	       (:file "font")
	       (:file "timer")
	       (:file "mode-line")
	       (:file "screen")
	       (:file "error")
	       (:file "display")
	       (:file "output")
	       (:file "input")
	       (:file "event")
	       (:file "gollum")
	       (:file "user")
	       (:file "user-defaults")))
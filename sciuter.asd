;;;; sciuter.asd

(asdf:defsystem #:sciuter
  :description "Describe sciuter here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "package")
               (:file "sciuter")
	       (:file "color")))

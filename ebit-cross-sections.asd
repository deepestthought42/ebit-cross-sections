;;;; ebit-cross-sections.asd

(asdf:defsystem #:ebit-cross-sections
  :serial t
  :description "Describe ebit-cross-sections here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:iterate
               #:ionization-energies)
  :components ((:file "package")
	       (:file "utils")
	       (:file "rr")
	       (:file "lotz")
               (:file "ebit-cross-sections")))


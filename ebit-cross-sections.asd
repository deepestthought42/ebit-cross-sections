;;;; ebit-cross-sections.asd

(asdf:defsystem #:ebit-cross-sections
  :serial t
  :description "This is a lisp package to calculate radiative recombination and ionization cross sections
for the interaction of ions with (mono-energetic) electrons."
  :author "Renee Klawitter <klawitterrenee@gmail.com>"
  :license "Apache 2.0"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:iterate
               #:ionization-energies)
  :components ((:file "package")
	       (:file "utils")
	       (:file "rr")
	       (:file "lotz")
               (:file "ebit-cross-sections")))


;;;; package.lisp

(defpackage #:ebit-cross-sections
  (:use #:cl #:iterate)
  (:export
   #:rr-cross-section
   #:ionization-cross-section
   #:create-ionization-cross-sections
   #:create-rr-cross-sections)
  (:nicknames
   #:cross))


;;;; lotz.lisp

(in-package #:ebit-cross-sections)


(proclaim '(inline ionization-cross-section))

(defun ionization-cross-section (electron-energy Z-ion e-to-remove)
  "Lotz ionization cross-section by electron impact. Given the e-
energy ELECTRON-ENERGY, calculates the cross-section to remove electon with number e- "
  (declare (optimize (speed 3) (safety 1))
	   (type double-float electron-energy)
	   (type (integer 0 100) Z-ion e-to-remove))
  (iter
   (for j from (1+ e-to-remove) to Z-ion)
   (declare (type (integer -1 100) j)
	    (type (double-float 0d0 2d7) corrected-b-e sigma))
   ;; carlson's correction for the binding energies in the neutral
   ;; atom by ionization, assuming a spherical electrostatic model
   ;; of the atom/ion.
   (for corrected-b-e = (ionization-energies:get-carslon-corrected-bind-energy
			 Z-ion (1+ e-to-remove) j))
   (if (and (< corrected-b-e electron-energy)
	    (> corrected-b-e 0d0))
       (sum (/ (* 4.5d-14 
		  (log (/ electron-energy
			  corrected-b-e)))
	       (* electron-energy
		  corrected-b-e))
	    into sigma))
   (finally (return sigma))))



(defun create-ionization-cross-sections (electron-energy Z-ion)
  "Return array with dimension Z-ION+1 with ionization cross-sections
   according to the Lotz formula for electron energy ELECTRON-ENERGY."
  (let ((cross-sections (make-array (list (1+ Z-ion))
				    :element-type 'double-float)))
    (iter
     (for q from 0 to Z-ion)
     (setf (aref cross-sections q)
	   (ionization-cross-section electron-energy Z-ion q)))
    cross-sections))








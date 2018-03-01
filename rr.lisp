;;;; rr.lisp

(in-package #:ebit-cross-sections)

;;; "wsuk" goes here. Hacks and glory await!


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *g-size* 101)
  (defparameter *n-max* 30))


(defun g-0 ()
  "I have no idea what this does ... yet"
  (let ((ret (make-array (list *g-size*) :initial-element 0d0 :element-type 'double-float)))
    (@[] (ret 1) (* (sqrt (* 8d0 pi)) (exp (- (log 4d0) 2d0))))
    (iter
      (for n from 2 to *n-max*)
      (@[] (ret n)
	   (/
	    (* ([] ret (- n 1)) 4.0d0 (- n 1)
	       (exp (* n (log (/ (* n 1.0d0) (* (- n 1) 1.0d0))))))
	    (sqrt (- (* 2.0d0 n) 1.0d0)) (sqrt (- (* 2.0d0 n) 2.0d0))
	    (exp 2.0d0))
           
	   ))
    ret))

(proclaim '(inline create-gg))

(defun create-gg (n kappa g-0 gg)
  "I have no idea what this does ... looks like transition matrix
elements though"
  (declare (optimize (speed 3) (space 3) (safety 0))
	   (type double-float kappa)
	   (inline + * - 1- 1+)
	   (type (integer 0 #.*g-size*) n)
	   (type (simple-array double-float) gg g-0))
  (with-doubles (a b c d e f f1)
    (setf f1 (+ 1.0d0 (expt (* n kappa) 2))
	  f (*
	     (/
	      (* (sqrt (the (double-float 0d0)
			    (/ 1.0d0
			       (- 1.0d0 (exp (- (/ (* 2.0d0 pi) kappa)))))))
		 (exp (- (* 2.0d0 n)
			 (* (/ 2.0d0 kappa) (atan (* n kappa))))))
	      (expt f1 2))
	     ([] g-0 n)))

    (- (expt n 2) (expt n 2))

    (iter (for s from 1 to n)
      (declare (type (integer 0 200) s))
      (setq f (/ (* f (sqrt (the (double-float 0d0)
				 (+ 1.0d0 (expt (* s kappa) 2d0)))))
		 f1)))
    (setf (aref gg (1- n) n) f)
    (if (< (- n 2) 0)
	(return-from create-gg gg))
    
    (@ (gg (- n 2) (- n 1))
       (* 0.5d0
	  (sqrt (the (double-float 0d0)
		     (* (- (* 2.0d0 n) 1.0d0) f1)))
	  (aref gg (- n 1) n)))
    (@ (gg (- n 1) (- n 2))
       (* (/ 0.5d0 n)
	  (sqrt (/ f1
		   (+ 1.0d0 (expt (* (- n 1.0d0) kappa) 2))))
	  (aref gg (- n 1) n)))

    (if (< (- n 3) 0)		  
	(return-from create-gg gg))

    (setf (aref gg (- n 2) (- n 3))
	  (* (/ (+ 4.0d0 (* (- n 1.0d0) f1)) 2.0d0 n)
	     (sqrt (the (double-float 0d0)
			(/ (- (* 2.0d0 n) 1.0d0)
			   (+ 1.0d0 (expt (* (- n 2) kappa) 2)))))
	     (aref gg (- n 1) (- n 2))))
    ;; and then for some iteration
    (iter (for l from (- n 1) downto 2)
      (declare (type (integer 0 #.*g-size*) l))
      (setf a (+ (* 4.0d0 (- (expt n 2) (expt l 2))
		    )
		 (* l (- (* 2d0 l) 1) f1))
	    b (- (* 2.0d0 n
		    (sqrt (the (double-float 0d0)
			       (* (- (expt n 2) (expt l 2))
				  (+ 1.0d0
				     (expt (* (+ l 1.0d0) kappa)
					   2)))))))
	    c (* 2.0d0 n
		 (sqrt (the (double-float 0d0)
			    (* (- (expt n 2) (expt (1- l) 2))
			       (+ 1.0d0 (expt (* l kappa) 2))))))
	    d (+ (- (* 4.0d0 (expt n 2))
		    (* 4 (expt l 2)))
		 (* l (1+ (* 2.0d0 l)) f1))
	    e (- (* 2.0d0 n
		    (sqrt  (the (double-float 0d0)
				(* (- (expt n 2) (expt (1+ l) 2))
				   (+ 1.0d0 (expt (* l kappa) 2)))))))
	    f (* 2 n
		 (sqrt (the (double-float 0d0)
			    (* (- (expt n 2) (expt l 2))
			       (+ 1
				  (expt (* (- l 1) kappa) 2)))))))
      (setf (aref gg (- l 2) (1- l))
	    (/ (+ (* a (aref gg (1- l) l))
		  (* b (aref gg l (1+ l))))
	       c))
      (if (< l (1- n))
	  (@ (gg (1- l) (- l 2))
	     (/ (+ (* d (aref gg l (1- l)))
		   (* e (aref gg (1+ l) l)))
		f)))
      
      gg)))

(proclaim '(inline determine-shell))

(defun determine-shell (q Z)
  "determine n,l of outer electron"
  ;; fixme: this seems to be overly complicated
  (let ((nmin 0)
	(lstart 0) 
	(num-electrons (- Z q)))
    (declare (optimize (speed 3) (space 3) (safety 0))
	     (type (integer #.(- *g-size*) #.*g-size*) nmin lstart num-electrons q Z))
    (if (= q Z)
	(setf nmin 1 lstart 0)
	(iter outer
	  (while (> num-electrons 0))
	  (setf lstart 0)
	  (incf nmin)
	  (decf num-electrons 2) ;; fill s-orbital
	  (if (< num-electrons 0)
	      ;; electrons left ?
	      (return-from outer)
	      (iter
                (for l from 1 to (1- nmin))
		(declare (type (integer #.(- *g-size*) #.*g-size*) l))
		(decf num-electrons (+ (* 4 l) 2))
		(if (< num-electrons 0)
		    (progn
		      (setf lstart l)
		      (return-from outer)))))))
    (values nmin lstart (coerce (if (not (equal 0 num-electrons))
				    (/ (- num-electrons)
				       (* 2 (1+ (* 2 lstart))))
				    1d0)
				'double-float))))


(defun create-gg-array ()
  (make-array (list *g-size* *g-size*)
	      :element-type 'double-float
	      :initial-element 0.0d0))


(defparameter *gg* (create-gg-array))
(defparameter *g-0* (g-0))


(defun rr-cross-section (e-kin q Z
			 &optional (gg *gg*)
				   (g-0 *g-0*))
    
  "compute radiative recombination cross section for ... fixme "
  (declare (inline create-gg)
	   (optimize (speed 3) (space 3) (safety 1))
	   (type (double-float 0d0) e-kin)
	   (type (integer 0 200) q Z))
  
  (let* ((ry 13.605698d0)
	 (alpha (/ 1d0 137.036d0))
	 (a0 5.29177249d-9)
	 (const (/ (* 4d0 pi alpha (expt a0 2)) 3d0))
	 (k (sqrt (the (double-float 0d0) (/ e-kin ry))))
	 (eta (/ (+ q Z)
		 (* 2d0 k)))
	 (cross 0d0))
    (multiple-value-bind (n-min l-start factor)
	(determine-shell q Z)
      (declare (type double-float cross ry alpha a0
		     const eta factor)
	       (inline + * - 1- 1+ create-gg)
	       (type (simple-array double-float) gg g-0))
      (iter shell
	(for n from n-min to *n-max*)
	(declare (type (integer 0 #.*g-size*) n n-min *n-max*)
		 (type (double-float) cross-shell))
	
	(for cross-shell = 0d0)
	(for loop = (if (equal n n-min) l-start 0))
	(iter sub-shell
	  (for l from loop to (1- n))
	  (declare (type double-float cross-sub-shell z-eff
			 kappa tmp-cross kk)
		   (type (integer -1 #.*g-size*) l loop))
	  (for z-eff =
	       (if (equal q Z)
		   (coerce Z 'double-float)
		   (*
		    (- (* (+ q Z) 0.5d0)
		       (/ (* (- Z q) 0.5d0 (- eta 1.0d0))
			  (+ eta 1.0d0 (* 3.0d0 l))))
		    (exp (- (* 0.05d0 (expt (- l 1) 2)))))))
	  (for kappa = (/ k z-eff))
	  (create-gg n kappa g-0 gg)
	  (for kk = (+ (* k k) (/ (* z-eff z-eff) (* n n))))
	  ;; creating temp cross section
	  (for tmp-cross = (* (+ 1d0 (* (expt n 2) kappa kappa))
			      (expt (aref gg l (1+ l)) 2)
			      (1+ l)))
	  (if (> l 0)
	      (incf tmp-cross 
		    (* (+ 1d0 (* (expt n 2) kappa kappa))
		       (expt (aref gg l (1- l)) 2)
		       l)))
	  (setf tmp-cross (/ (* tmp-cross n n const) z-eff z-eff))
	  (for cross-sub-shell = (* 0.5d0
				    tmp-cross
				    (expt (/ (* alpha kk) k ) 2)))
	  (if (and (equal n n-min)
		   (equal l l-start))
	      (setf cross-sub-shell (* cross-sub-shell factor)))
	  (if (and (> l loop)
		   (< cross-sub-shell 1d-50))
	      (return-from sub-shell)
	      (incf cross-shell cross-sub-shell)))
	(incf cross cross-shell)))
    cross))




(defun create-rr-cross-sections (electron-energy Z-ion)
  "Return array with dimension Z-ION+1 with radiative recombination
   cross-sections for electron energy ELECTRON-ENERGY."
  (let ((cross-sections (make-array (list (1+ Z-ion))
				    :element-type 'double-float)))
    ; can't recombine into neutral atom
    (setf (aref cross-sections 0) 0d0) 
    (iter
      (for q from 1 to Z-ion)
      (setf (aref cross-sections q)
	    (rr-cross-section electron-energy q Z-ion)))
    cross-sections))



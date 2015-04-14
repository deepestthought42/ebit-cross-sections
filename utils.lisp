;;;; wsuk.lisp

(in-package #:ebit-cross-sections)


(defmacro [] (array &rest subscripts)
  "[] is expands to aref where every subscript is reduced by one to be
equivalent to fortran."
  `(aref ,array ,@(iter (for i in subscripts)
		    (collect (list '1- i)))))


(defmacro @ ((array &rest subscripts) val)
  `(setf (aref ,array ,@subscripts) ,val))

(defmacro @[] ((array &rest subscripts) val)
  `(setf ([] ,array ,@subscripts) ,val))

(defmacro with-doubles ((&rest names) &body body)
  `(let (,@(iter (for n in names)
	     (collect (list n 0d0))))
     (declare (type double-float ,@names))
     ,@body))

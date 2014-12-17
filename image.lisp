(in-package #:cl-z80) 

(defparameter *image* (make-array #x4000 :adjustable t))
(defparameter *ip* 0)
(defparameter *org* 0)

(defun emit-byte (n)
  (if (numberp n)
      (setf (elt *image* *ip*) (two-complement n))
      (add-forward-label *ip* n))
  (setq *ip* (1+ *ip*)))

(defun emit (&rest l)
  (loop for i in l do (emit-byte i)))

(defun save-image (fname)
  (emit-forward-labels)
  (with-open-file (stream fname
                          :direction :output
                          :element-type 'unsigned-byte
                          :if-exists :supersede)
    (loop for i below (array-dimension *image* 0) do
         (write-byte (elt *image* i) stream))))

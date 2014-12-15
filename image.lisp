(in-package #:cl-z80) 
(defparameter *image* (make-array #x4000 :adjustable t))
(defparameter *ip* 0)
(defparameter *org* 0)
(defparameter *forward-labels* '())

(defun clear-forward-labels ()
  (setq *forward-labels* '()))

(defun add-forward-label (ip l)
  (setq *forward-labels*
        (cons (cons ip l)
              *forward-labels*)))

(defun emit-byte (n)
  (if (numberp n)
      (setf (elt *image* *ip*) n)
      (add-forward-label *ip* n))
  (setq *ip* (1+ *ip*)))

(defun emit (&rest l)
  (loop for i in l do (emit-byte i)))

(defun emit-forward-labels ()
  (loop for l in *forward-labels* do
       (let ((ip (car l))
             (type (cadr l))
             (lbl (get-label-address (caddr l))))
         (cond ((null lbl)
                (error (format t "label ~a not found" (caddr l))))
               ((eq type 'byte)
                (setf (elt *image* ip) lbl))
               ((eq type 'low-word)
                (setf (elt *image* ip) (low-word lbl)))
               ((eq type 'high-word)
                (setf (elt *image* ip) (high-word lbl)))
               ((eq type 'index)
                (setf (elt *image* ip) lbl))))) ; TODO
  (clear-forward-labels))

(defun save-image (fname)
  (emit-forward-labels)
  (with-open-file (stream fname
                          :direction :output
                          :element-type 'unsigned-byte
                          :if-exists :supersede)
    (loop for i below (array-dimension *image* 0) do
         (write-byte (elt *image* i) stream))))

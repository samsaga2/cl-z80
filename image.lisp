(in-package #:cl-z80) 
(defparameter *image* (make-array #x4000 :adjustable t))
(defparameter *ip* 0)
(defparameter *org* 0)
(defparameter *forward-labels* '())

(defun clear-forward-labels ()
  (setq *forward-labels* '()))

(defun add-forward-label (ip l)
  (setq *forward-labels*
        (cons (list ip *org* l)
              *forward-labels*)))

(defun emit-byte (n)
  (if (numberp n)
      (setf (elt *image* *ip*) (two-complement n))
      (add-forward-label *ip* n))
  (setq *ip* (1+ *ip*)))

(defun emit (&rest l)
  ;; (format t "~x ~%-----" l)
  (loop for i in l do (emit-byte i)))

(defun emit-forward-labels ()
  (loop for l in *forward-labels* do
       (let ((ip (car l))
             (org (cadr l))
             (type (caaddr l))
             (lbl (get-label-address (car (cdaddr l)))))
         (cond ((null lbl)
                (error (format t "label ~a not found" (caddr l))))
               ((eq type 'byte)
                (setf (elt *image* ip) lbl))
               ((eq type 'low-word)
                (setf (elt *image* ip) (low-word lbl)))
               ((eq type 'high-word)
                (setf (elt *image* ip) (high-word lbl)))
               ((eq type 'index)
                (setf (elt *image* ip) (- lbl (+ org ip)))))))
  (clear-forward-labels))

(defun save-image (fname)
  (emit-forward-labels)
  (with-open-file (stream fname
                          :direction :output
                          :element-type 'unsigned-byte
                          :if-exists :supersede)
    (loop for i below (array-dimension *image* 0) do
         (write-byte (elt *image* i) stream))))

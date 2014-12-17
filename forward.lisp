(in-package #:cl-z80) 

(defparameter *forward-labels* '())

(defun make-forward-low-word (lbl)
  (list 'low-word lbl))

(defun make-forward-high-word (lbl)
  (list 'high-word lbl))

(defun make-forward-byte (lbl)
  (list 'byte lbl))

(defun make-forward-index (lbl)
  (list 'index lbl))

(defun clear-forward-labels ()
  (setq *forward-labels* '()))

(defun add-forward-label (ip l)
  (setq *forward-labels*
        (cons (list ip *org* l)
              *forward-labels*)))

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

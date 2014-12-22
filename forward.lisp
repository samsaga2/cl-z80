(in-package #:cl-z80) 

(defparameter *forward-labels* '())
(defstruct forward-label type id ns ip org)

(defun clear-forward-labels ()
  (setq *forward-labels* '()))

(defun add-foward-label (ip lbl)
  (setq *forward-labels*
        (cons
         (make-forward-label :type (car lbl)
                             :id (cdr lbl)
                             :ns *namespace*
                             :ip ip
                             :org *org*)
         *forward-labels*)))

(defun make-forward-low-word (lbl)
  (cons 'low-word lbl))

(defun make-forward-high-word (lbl)
  (cons 'high-word lbl))

(defun make-forward-byte (lbl)
  (cons 'byte lbl))

(defun make-forward-index (lbl)
  (cons 'index lbl))

(defun emit-forward-labels ()
  (loop for l in *forward-labels* do
       (let* ((ip (forward-label-ip l))
              (org (forward-label-org l))
              (type (forward-label-type l))
              (lbl (forward-label-id l))
              (ns (forward-label-ns l))
              (address (get-label-in-ns lbl ns)))
         (cond ((null address)
                (error (format t "label ~a not found" lbl)))
               ((eq type 'byte)
                (setf (elt *image* ip) address))
               ((eq type 'low-word)
                (setf (elt *image* ip) (low-word address)))
               ((eq type 'high-word)
                (setf (elt *image* ip) (high-word address)))
               ((eq type 'index)
                (setf (elt *image* ip) (- address (+ org ip) 1))))))
  (clear-forward-labels))

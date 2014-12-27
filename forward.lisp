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
         (if (null address)
             (error (format t "label ~a not found" lbl))
             (setf (elt *image* ip)
                   (cond ((eq type 'byte)
                          (byte-two-complement address))
                         ((eq type 'low-word)
                          (low-word (word-two-complement address)))
                         ((eq type 'high-word)
                          (high-word (word-two-complement address)))
                         ((eq type 'index)
                          (byte-two-complement (- address (+ org ip) 1))))))))
  (clear-forward-labels))

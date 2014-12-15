(in-package #:cl-z80)

;; labels
(defparameter *labels* (make-hash-table))

(defun getlabel (label)
  (gethash label *labels*))

(defun putlabel (label value)
  (setf (gethash label *labels*) value))

(let ((count 0))
  (defun genlabel (value)
    (setq count (1+ count))
    (putlabel (format t "l~a" count) value)))

;; foward labels
(defparameter *forward-labels* '())

(defun add-forward-label (ip l)
  (setq *forward-labels*
        (cons (cons ip l)
              *forward-labels*)))

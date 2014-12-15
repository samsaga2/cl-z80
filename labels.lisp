(in-package #:cl-z80)

;; namespace
(defparameter *namespace* '())

(defun push-namespace (n)
  (setq *namespace*
        (cons n *namespace*)))

(defun pop-namespace ()
  (setq *namespace* (cdr *namespace*)))

(defun get-full-label-name (lbl ns)
  (format nil "~{~a.~}~a" (reverse ns) lbl))

;; labels
(defparameter *labels* (make-hash-table :test 'equal))

(defun get-label-in-namespace (label ns)
  (let* ((label-ns (get-full-label-name label ns))
         (address (gethash label-ns *labels*)))
    (cond (address address)
          ((null ns) '())
          (t (get-label-in-namespace label (cdr ns))))))

(defun get-label-address (label)
  (get-label-in-namespace label *namespace*))

(defun set-label-address (label value)
  (setf (gethash (get-full-label-name label *namespace*) *labels*)
        value))

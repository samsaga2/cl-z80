(in-package #:cl-z80) 

(defparameter *forward-labels* '())

(defstruct forward-label type id ns ip page-index)

(defun clear-forward-labels ()
  (setq *forward-labels* nil))

(defun add-forward-label (page-index ip lbl)
  (setq *forward-labels*
        (cons
         (make-forward-label :type (car lbl)
                             :id (cdr lbl)
                             :ns *namespace*
                             :ip ip
                             :page-index page-index)
         *forward-labels*)))

(defun make-forward-low-word (lbl)
  (cons 'low-word lbl))

(defun make-forward-high-word (lbl)
  (cons 'high-word lbl))

(defun make-forward-byte (lbl)
  (cons 'byte lbl))

(defun make-forward-index (lbl)
  (cons 'index lbl))

(defun set-forward-label-address (l address)
  (let* ((page-index (forward-label-page-index l))
         (page (aref *pages* page-index))
         (org (page-origin page))
         (ip (forward-label-ip l))
         (image (page-image page))
         (type (forward-label-type l)))
    (setf (aref image ip)
          (cond ((eq type 'byte)
                 (byte-two-complement address))
                ((eq type 'low-word)
                 (low-word (word-two-complement address)))
                ((eq type 'high-word)
                 (high-word (word-two-complement address)))
                ((eq type 'index)
                 (let ((index (- address (+ org ip) 1)))
                   (byte-two-complement index)))))))

(defun emit-forward-label (l)
  (let* ((lbl (forward-label-id l))
         (ns (forward-label-ns l))
         (address (get-label-in-ns lbl ns)))
    (if (null address)
        (error (format t "label ~a not found" lbl))
        (set-forward-label-address l address))))

(defun emit-forward-labels ()
  (dolist (l *forward-labels*)
    (emit-forward-label l))
  (clear-forward-labels))

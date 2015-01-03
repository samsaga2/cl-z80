(in-package #:cl-z80)

;; namespace
(defparameter *namespace* nil)

(defun set-namespace (ns)
  (setq *namespace*
        (when ns
          (reverse (cl-ppcre:split "\\." ns)))))

(defun push-namespace (n)
  (setq *namespace* (cons n *namespace*)))

(defun pop-namespace ()
  (setq *namespace* (cdr *namespace*)))

(defun get-full-label-name (lbl ns)
  (format nil "~{~a.~}~a" (reverse ns) lbl))

;; labels
(defparameter *labels* (make-hash-table :test 'equal))

(defun get-label-in-ns (label ns)
  (let* ((label-ns (get-full-label-name label ns))
         (address (gethash label-ns *labels*)))
    (cond (address address)
          ((null ns) nil)
          (t (get-label-in-ns label (cdr ns))))))

(defun get-label (label)
  (get-label-in-ns label *namespace*))

(defun set-label (label value)
  (if (get-label label)
      (format t "duplicated label ~a~%"
              (get-full-label-name label *namespace*))
      (setf (gethash (get-full-label-name label *namespace*) *labels*)
            value)))

(defun save-symbols (fname)
  (with-open-file (stream fname
                          :direction :output
                          :if-exists :supersede)
    (loop for key being the hash-keys of *labels* using (hash-value value) do
         (format stream "~a: equ 0x~x~%" key value))))

(let ((count 0))
  (defun genlabel ()
    (setq count (1+ count))
    (intern (format nil "~_l~a" count))))

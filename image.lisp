(in-package #:cl-z80) 

(defstruct page origin size image ip)

(defun default-page ()
  (make-page :origin 0
             :size #x2000
             :image (make-array #x2000 :adjustable t
                                :initial-element 0)
             :ip 0))

(defparameter *pages* (make-array 256 :initial-element nil))

(defparameter *current-page* 0)

(defun defpage (index origin size)
  (setf (aref *pages* index)
        (make-page :origin origin
                   :size size
                   :image (make-array size)
                   :ip 0)))

(defpage 0 0 #x4000)

(defun change-page (index)
  (setq *current-page* index))

(defun get-current-page ()
  (aref *pages* *current-page*))

(defun page-address (page)
  (let ((page (get-current-page))
        (org (page-origin page))
        (ip (page-ip page)))
    (+ org ip)))

(defun emit-byte (n)
  (let* ((page (get-current-page))
         (image (page-image page))
         (ip (page-ip page)))
    (if (numberp n)
        (setf (aref image ip) n)
        (add-forward-label *current-page* ip n))
    (setf (page-ip page) (1+ ip))))

(defun emit (&rest l)
  (dolist (i l)
    (emit-byte i)))

(defun emit-string (str)
  (loop for i across str do
       (emit-byte (char-int i))))

(defun save-page (stream page)
  (let ((image (page-image page))
        (size (page-size page)))
    (dotimes (i size)
      (write-byte (aref image i) stream))))
            
(defun save-image (fname)
  (emit-forward-labels)
  (with-open-file (stream fname
                          :direction :output
                          :element-type 'unsigned-byte
                          :if-exists :supersede)
    (dotimes (i 256)
      (let ((page (aref *pages* i)))
        (when page (save-page stream page))))))

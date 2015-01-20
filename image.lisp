(in-package #:cl-z80) 

(defstruct page origin size image ip)

(defun default-page ()
  (make-page :origin 0
             :size #x2000
             :image (make-array #x2000 :adjustable t
                                :initial-element 0)
             :ip 0))

(defparameter *pages* (make-array 256 :initial-element nil))

(defparameter *current-pages* (list 0))

(defun defpage (index origin size)
  (setf (aref *pages* index)
        (make-page :origin origin
                   :size size
                   :image (make-array size)
                   :ip 0)))

(defpage 0 0 #x4000)

(defun set-page (&rest indices)
  (setq *current-pages* indices))

(defun get-page ()
  (car *current-pages*))

(defun get-current-page ()
  (aref *pages* (get-page)))

(defun page-address (page)
  (let ((page (get-current-page))
        (org (page-origin page))
        (ip (page-ip page)))
    (+ org ip)))

(defun out-of-bounds ()
  (let ((page (get-current-page)))
    (= (page-ip page) (page-size page))))

(defun move-to-next-page ()
  (if (null *current-pages*)
      (error "out of bounds")
      (setq *current-pages* (cdr *current-pages*))))

(defun emit-byte (n)
  (when (out-of-bounds)
    (move-to-next-page))
  (let* ((page (get-current-page))
         (image (page-image page))
         (ip (page-ip page)))
    (if (numberp n)
        (setf (aref image ip) n)
        (add-forward-label (get-page) ip n))
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

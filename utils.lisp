(in-package #:cl-z80)

;; utils
(defun between? (i low high)
  (and (>= i low)
       (<= i high)))

(defun ormap (fn xs)
  (unless (null xs)
    (let ((i (funcall fn (car xs))))
      (if i i (ormap fn (cdr xs))))))

;; numbers
(defun low-word (w)
  (logand w 255))

(defun high-word (w)
  (ash w -8))

(defun two-complement (n)
  (if (< n 0)
      (- (logxor #xff (- n)) 1)
      n))

(defun byte? (num)
  (and (numberp num)
       (between? num -127 255)))

(defun word? (num)
  (and (numberp num)
       (between? num -32767 65535)))

(defun index? (num)
  (and (numberp num)
       (between? num -127 128)))

(in-package #:cl-z80)

;; utils
(defun make-keyword (str)
  (intern (string-upcase str) :keyword))

(defun quote-to-keywords (lst)
  (mapcar (lambda (i)
            (cond ((listp i) (quote-to-keywords i))
                  ((symbolp i) (make-keyword i))
                  (t i)))
          lst))

(defun ormap (fn xs)
  (unless (null xs)
    (let ((i (funcall fn (car xs))))
      (if i i (ormap fn (cdr xs))))))

;; numbers
(defun low-word (w)
  (logand w 255))

(defun high-word (w)
  (ash w -8))

(defun byte-two-complement (n)
  (if (< n 0)
      (1+ (logxor #xff (- n)))
      n))

(defun word-two-complement (n)
  (if (< n 0)
      (1+ (logxor #xffff (- n)))
      n))

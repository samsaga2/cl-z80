(in-package #:cl-z80)

(defstruct pointer index)

(defun make-space (start-index)
  (make-pointer :index #xc000))

(defun new-space (bytes ptr)
  (let ((p (pointer-index ptr)))
    (setf (pointer-index ptr) (+ p bytes))
    p))

(defun new-byte (ptr)
  (new-space 1 ptr))

(defun new-word (ptr)
  (new-space 2 ptr))

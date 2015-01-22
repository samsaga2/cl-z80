(in-package #:cl-z80)

(defparameter *forbidden* '())

(defun add-forbidden (sym)
  (setq *forbidden*
        (adjoin (string sym) *forbidden*)))

(defun add-forbiddens (pattern)
  (cond ((numberp pattern) nil)
        ((listp pattern)
         (dolist (i pattern)
           (add-forbiddens i)))
        (t
         (add-forbidden pattern))))

(defun forbidden? (sym)
  (member (symbol-name sym) *forbidden*
          :test #'equal))

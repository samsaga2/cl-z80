(in-package #:cl-z80)

;; instructions
(defparameter *insts* nil)

(defun add-inst (pattern out)
  (add-forbiddens pattern)
  (setq *insts* (cons (cons pattern out) *insts*)))

(defmacro definst (pattern &body out)
  (let* ((pattern (quote-to-keywords pattern))
         (args (get-match-ids pattern (list))))
    `(add-inst (quote ,pattern)
               (lambda (env)
                 (declare (ignorable env))
                 ,(reduce (lambda (i j)
                            `(let ((,(intern (string j))
                                    (cdr (assoc ,j env))))
                               ,i))
                          args :initial-value (cons 'progn out))
                 t))))

(defun try-eval (i)
  (if (listp i)
      (let ((funsym (car i)))
        (if (and (symbolp funsym)
                 (not (forbidden? funsym))
                 (fboundp funsym))
            (if (member (car i) '(+ - * /))
                (eval (cons (car i) (mapcar #'try-eval (cdr i))))
                (eval i))
            (mapcar #'try-eval i)))
      (or (get-label i)
          (when (symbolp i) (make-keyword i))
          i)))


;; asm
(defun asm-inst (inst)
  (let ((inst (try-eval inst)))
    (or
     (ormap (lambda (i)
              (match-inst inst (car i) (cdr i) (list)))
            *insts*)
     (format t "syntax error ~a~%" inst))))

(defun asm-insts (insts)
  (dolist (i insts)
    (asm-inst i)))


;; asm util
(defmacro asm (&rest body)
  `(asm-insts (quote ,body)))

(defmacro asmproc (label &body body)
  `(asm (label ,label ,@body)))

(defmacro asmequ (label &rest value)
  `(asm (equ ,label ,@value)))

(defmacro asmpackage (&optional ns)
  (if (null ns)
      `(set-namespace nil)
      `(set-namespace ,(symbol-name ns))))

(defmacro asmenum (label &rest values)
  `(asmproc ,label (enum ,@values)))

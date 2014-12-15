(in-package #:cl-z80)

(defparameter *insts* '())
(defvar *byte* 0)
(defvar *index* 0)
(defvar *word* 0)
(defvar *lword* 0)
(defvar *hword* 0)
(defvar *lst* '())
(defvar *number* 0)
(defvar *sym* '())

(defun add-inst (pattern out)
  (setq *insts*
        (cons (cons pattern out)
              *insts*)))

(defmacro definst (pattern &body out)
  `(add-inst ,pattern (lambda () ,@out)))

(defun match-inst (inst pattern out)
  (labels ((cont () (match-inst (cdr inst) (cdr pattern) out)))
    (let* ((i (car inst))
           (p (car pattern))
           (n (if (numberp i) i (get-label-address i)))) ; if the label is defined avoid add forward label
      (cond ((null pattern)
             (funcall out))
            ((eq p i)
             (cont))
            ((and (listp i) (listp p))
             (match-inst i p out)
             (cont))
            ;; byte
            ((and (eq p 'byte) (numberp n))
             (setq *byte* n)
             (cont))
            ((and (eq p 'byte) (symbolp i))
             (setq *byte* (list 'byte i))
             (cont))
            ;; word
            ((and (eq p 'word) (numberp n))
             (setq *lword* (low-word n))
             (setq *hword* (high-word n))
             (cont))
            ((and (eq p 'word) (symbolp i))
             (setq *lword* (list 'low-word i))
             (setq *hword* (list 'high-word i))
             (cont))
            ;; index
            ((and (eq p 'index) (numberp n))
             (setq *index* n)
             (cont))
            ((and (eq p 'index) (symbolp i))
             (setq *index* (list 'index i))
             (cont))
            ;; for special funcs
            ((eq p 'lst)
             (setq *lst* inst)
             (funcall out))
            ((and (eq p 'number) (numberp n))
             (setq *number* n)
             (cont))
            ((and (eq p 'sym) (symbolp i))
             (setq *sym* i)
             (cont))))))

;; asm
(defun asm-inst (inst)
  (ormap (lambda (i) (match-inst inst (car i) (cdr i)))
         *insts*))

(defmacro asm (&rest body)
  (when body
    `(append
      (emit (asm-inst (quote ,(car body))))
      (asm ,@(cdr body)))))

;; asm util
(defmacro defproc (label &body body)
  `(asm (label ,label ,@body)))

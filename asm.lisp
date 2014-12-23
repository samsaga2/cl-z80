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

(defun clear-inst ()
  (setq *byte* 0
        *index* 0
        *word* 0
        *lword* 0
        *hword* 0
        *lst* '()
        *number* 0
        *sym* '()
        *reg* '()))

(defun add-inst (pattern out)
  (setq *insts*
        (cons (cons pattern out)
              *insts*)))

(defmacro definst (pattern &body out)
  `(add-inst (quote ,pattern)
             (lambda () ,@out T)))

(defun forbidden-label? (sym)
  (member (symbol-name sym)
          '("A" "B" "C" "D" "E" "H" "L" "R" "IXL" "IXH" "IYL" "IYH"
            "AF" "BC" "DE" "HL" "DE" "IX" "IY" "SP"
            "Z" "NZ" "C" "NC" "PO" "PE" "P" "M")
          :test #'equal))

(defun match-inst (inst pattern out)
  (labels ((cont () (match-inst (cdr inst) (cdr pattern) out)))
    (let* ((i (car inst))
           (p (car pattern))
           (n (if (numberp i) i (get-label-address i)))) ; if the label is defined avoid add forward label
      (cond ((and (null pattern) (not (null inst)))
             '())
            ((null pattern)
             (funcall out))
            ((and (symbolp p)
                  (symbolp i)
                  (equal (symbol-name p) (symbol-name i)))
             (cont))
            ((and (numberp p)
                  (numberp i)
                  (equal p i))
             (cont))
            ((and (listp i) (listp p))
             (when (match-inst i p (lambda () T))
               (cont)))
            ;; byte
            ((and (eq p 'byte) (numberp n))
             (setq *byte* n)
             (cont))
            ((and (eq p 'byte) (symbolp i) (not (forbidden-label? i)))
             (setq *byte* (make-forward-byte i))
             (cont))
            ;; word
            ((and (eq p 'word) (numberp n))
             (setq *lword* (low-word n))
             (setq *hword* (high-word n))
             (cont))
            ((and (eq p 'word) (symbolp i) (not (forbidden-label? i)))
             (setq *lword* (make-forward-low-word i))
             (setq *hword* (make-forward-high-word i))
             (cont))
            ;; index
            ((and (eq p 'index) (numberp n))
             (setq *index* n)
             (cont))
            ((and (eq p 'index) (symbolp i) (not (forbidden-label? i)))
             (setq *index* (make-forward-byte i))
             (cont))
            ;; relative index
            ((and (eq p 'rindex) (numberp n))
             (setq *index* (- n (+ *org* *ip*)))
             (cont))
            ((and (eq p 'rindex) (symbolp i) (not (forbidden-label? i)))
             (setq *index* (make-forward-index i))
             (cont))
            ;; for special funcs
            ((eq p 'lst)
             (setq *lst* inst)
             (funcall out))
            ((and (eq p 'number) (numberp n))
             (setq *number* n)
             (cont))
            ((and (eq p 'sym) (symbolp i) (not (forbidden-label? i)))
             (setq *sym* i)
             (cont))
            ((and (eq p 'reg) (symbolp i) (forbidden-label? i))
             (setq *reg* i)
             (cont))))))

;; asm
(defun asm-inst (inst)
  (clear-inst)
  (or
   (ormap (lambda (i) (match-inst inst (car i) (cdr i)))
          *insts*)
   (format t "syntax error ~a~%" inst)))

(defun asm-insts (insts)
  (loop for i in insts do (asm-inst i)))

(defun expand-asm (l)
  (cond ((null l)
         '())
        ((and (listp l)
             (symbolp (car l))
             (equal (symbol-name (car l)) "$"))
         (cons (eval (cadr l)) (expand-asm (cddr l))))
        ((listp l)
         (cons (expand-asm (car l))
               (expand-asm (cdr l))))
        (t l)))

(defmacro asm (&rest body)
  `(asm-insts (quote ,(mapcar #'expand-asm body))))

;; asm util
(defmacro defproc (label &body body)
  `(asm (label ,label ,@body)))

(defmacro defequ (label &body value)
  `(asm (equ ,label ,@value)))

(defmacro defnamespace (&optional ns)
  (if (null ns)
      `(set-namespace '())
      `(set-namespace ,(symbol-name ns))))

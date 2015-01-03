(in-package #:cl-z80)

(defparameter *insts* nil)

(defvar *byte* 0)
(defvar *index* 0)
(defvar *word* 0)
(defvar *lword* 0)
(defvar *hword* 0)
(defvar *lst* nil)
(defvar *number* 0)
(defvar *sym* nil)
(defvar *reg* nil)

(defun clear-inst ()
  (setq *byte* 0
        *index* 0
        *word* 0
        *lword* 0
        *hword* 0
        *lst* nil
        *number* 0
        *sym* nil
        *reg* nil))

(defparameter *forbidden-symbols* '())

(defun add-forbidden-symbols (pattern)
  (cond ((numberp pattern) nil)
        ((listp pattern)
         (dolist (i pattern)
           (add-forbidden-symbols i)))
        (t
         (setq *forbidden-symbols*
               (adjoin (format nil "~a" pattern) *forbidden-symbols*)))))

(defun add-inst (pattern out)
  (add-forbidden-symbols pattern)
  (setq *insts*
        (cons (cons pattern out)
              *insts*)))

(defmacro definst (pattern &body out)
  `(add-inst (quote ,pattern)
             (lambda () ,@out T)))


(defun forbidden-symbol? (sym)
  (member (symbol-name sym)
          *forbidden-symbols*
          :test #'equal))

(declaim (debug 3))

(defun match-inst (skip-fun inst pattern out)
  (labels ((cont ()
             (match-inst nil (cdr inst) (cdr pattern) out))
           (evalarg (i)
             (or (get-label i)
                 (when (and (listp i) (not skip-fun))
                   (let ((funsym (car i)))
                     (when (and (symbolp funsym)
                                (not (forbidden-symbol? funsym))
                                (fboundp funsym))
                       (eval i))))
                 i)))
    (let* ((i (car inst))
           (p (car pattern))
           (n (evalarg i)))
      (cond ((and (null pattern)        ; wrong pattern
                  (not (null inst)))
             nil)                       
            ((null pattern)             ; ok pattern
             (funcall out))
            ((and (symbolp p)           ; match symbol
                  (symbolp i)
                  (equal (symbol-name p) (symbol-name i)))
             (cont))
            ((and (numberp p)           ; match number
                  (numberp i)
                  (equal p i))
             (cont))
            ((and (listp i)             ; match list
                  (listp p))
             (when (match-inst nil i p (lambda () t))
               (cont)))
            ;; byte
            ((and (eq p 'byte)          ; match byte
                  (numberp n))
             (setq *byte* (byte-two-complement n))
             (cont))
            ((and (eq p 'byte)          ; match forward byte
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *byte* (make-forward-byte i))
             (cont))
            ;; word
            ((and (eq p 'word)          ; match word
                  (numberp n))
             (let ((n (word-two-complement n)))
               (setq *lword* (low-word n))
               (setq *hword* (high-word n)))
             (cont))
            ((and (eq p 'word)          ; match forward word
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *lword* (make-forward-low-word i))
             (setq *hword* (make-forward-high-word i))
             (cont))
            ;; index
            ((and (eq p 'index)         ; match index
                  (numberp n))
             (setq *index* (byte-two-complement n))
             (cont))
            ((and (eq p 'index)         ; match forward index
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *index* (make-forward-byte i))
             (cont))
            ;; relative index
            ((and (eq p 'rindex)        ; match relative index
                  (numberp n))
             (setq *index* (byte-two-complement (- n (+ *org* *ip*) 2)))
             (cont))
            ((and (eq p 'rindex)        ; match forward relative index
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *index* (make-forward-index i))
             (cont))
            ;; for special insts
            ((eq p 'lst)                ; match rest
             (setq *lst* inst)
             (funcall out))
            ((and (eq p 'number)        ; match number
                  (numberp n))
             (setq *number* n)
             (cont))
            ((and (eq p 'sym)           ; match symbol
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *sym* i)
             (cont))
            ((and (eq p 'reg)           ; match forbidden sym
                  (symbolp i)
                  (forbidden-symbol? i))
             (setq *reg* i)
             (cont))))))

;; asm
(defun asm-inst (inst)
  (clear-inst)
  (or
   (ormap (lambda (i) (match-inst t inst (car i) (cdr i)))
          *insts*)
   (format t "syntax error ~a~%" inst)))

(defun asm-insts (insts)
  (dolist (i insts)
    (asm-inst i)))


;; asm util
(defmacro asm (&rest body)
  `(asm-insts (quote ,body)))

(defmacro asmproc (label &body body)
  `(asm (label ,label ,@body)))

(defmacro asmequ (label &body value)
  `(asm (equ ,label ,@value)))

(defmacro asmpackage (&optional ns)
  (if (null ns)
      `(set-namespace nil)
      `(set-namespace ,(symbol-name ns))))

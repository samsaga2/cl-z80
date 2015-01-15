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

(defun add-forbidden-symbol (sym)
  (setq *forbidden-symbols*
        (adjoin (string sym) *forbidden-symbols*)))

(defun add-forbidden-symbols (pattern)
  (cond ((numberp pattern) nil)
        ((listp pattern)
         (dolist (i pattern)
           (add-forbidden-symbols i)))
        (t
         (add-forbidden-symbol pattern))))

(defun forbidden-symbol? (sym)
  (member (symbol-name sym) *forbidden-symbols*
          :test #'equal))

(defun add-inst (pattern out)
  (add-forbidden-symbols pattern)
  (setq *insts* (cons (cons pattern out) *insts*)))

(defmacro definst (pattern &body out)
  `(add-inst (quote ,pattern)
             (lambda () ,@out t)))

(defun try-eval (i)
  (if (listp i)
      (let ((funsym (car i)))
        (if (and (symbolp funsym)
                 (not (forbidden-symbol? funsym))
                 (fboundp funsym))
            (if (member (car i) '(+ - * /))
                (eval (cons (car i) (mapcar #'try-eval (cdr i))))
                (eval i))
            (mapcar #'try-eval i)))
      (or (get-label i) i)))

(defun match-inst (inst pattern out)
  (labels ((cont ()
             (match-inst (cdr inst) (cdr pattern) out)))
    (let* ((i (car inst))
           (p (car pattern)))
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
             (when (match-inst i p (lambda () t))
               (cont)))
            ;; byte
            ((and (eq-symbol-name p 'byte)          ; match byte
                  (numberp i))
             (setq *byte* (byte-two-complement i))
             (cont))
            ((and (eq-symbol-name p 'byte)          ; match forward byte
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *byte* (make-forward-byte i))
             (cont))
            ;; word
            ((and (eq-symbol-name p 'word)          ; match word
                  (numberp i))
             (let ((i (word-two-complement i)))
               (setq *lword* (low-word i))
               (setq *hword* (high-word i)))
             (cont))
            ((and (eq-symbol-name p 'word)          ; match forward word
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *lword* (make-forward-low-word i))
             (setq *hword* (make-forward-high-word i))
             (cont))
            ;; index
            ((and (eq-symbol-name p 'index)         ; match index
                  (numberp i))
             (setq *index* (byte-two-complement i))
             (cont))
            ((and (eq-symbol-name p 'index)         ; match forward index
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *index* (make-forward-byte i))
             (cont))
            ;; relative index
            ((and (eq-symbol-name p 'rindex)        ; match relative index
                  (numberp i))
             (setq *index*
                   (byte-two-complement (- i (page-address (get-current-page)) 2)))
             (cont))
            ((and (eq-symbol-name p 'rindex)        ; match forward relative index
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *index* (make-forward-index i))
             (cont))
            ;; for special insts
            ((eq-symbol-name p 'lst)                ; match rest
             (setq *lst* inst)
             (funcall out))
            ((and (eq-symbol-name p 'number)        ; match number
                  (numberp i))
             (setq *number* i)
             (cont))
            ((and (eq-symbol-name p 'sym)           ; match symbol
                  (symbolp i)
                  (not (forbidden-symbol? i)))
             (setq *sym* i)
             (cont))
            ((and (eq-symbol-name p 'reg)           ; match forbidden sym
                  (symbolp i)
                  (forbidden-symbol? i))
             (setq *reg* i)
             (cont))))))

;; asm
(defun asm-inst (inst)
  (clear-inst)
  (or
   (let ((inst (try-eval inst)))
     (ormap (lambda (i)
              (match-inst inst (car i) (cdr i)))
            *insts*))
   (format t "syntax error ~a~%" inst)))

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

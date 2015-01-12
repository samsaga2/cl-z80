;;;; package.lisp

(defpackage #:cl-z80
  (:use #:cl)
  (:export #:asm-insts
           #:asm
           #:asmproc
           #:asmequ
           #:asmpackage
           #:asmenum
           #:add-inst
           #:definst
           #:*byte*
           #:*index*
           #:*word*
           #:*lword*
           #:*hword*
           #:*lst*
           #:*number*
           #:*sym*
           #:*reg*
           #:make-space
           #:new-space
           #:new-byte
           #:new-word
           #:genlabel
           #:get-label
           #:set-label
           #:set-namespace
           #:defpage
           #:change-page
           #:save-image
           #:save-symbols))

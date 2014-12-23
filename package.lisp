;;;; package.lisp

(defpackage #:cl-z80
  (:use #:cl)
  (:export #:asm
           #:defproc
           #:defequ
           #:defnamespace
           #:set-namespace
           #:save-image
           #:save-symbols))

(ql:quickload "cl-z80")

(defpackage #:example
  (:use #:cl #:cl-z80))

(in-package #:example)

(asm (org #x4000)
     (dw start 0 0 0 0 0 0))

(asmproc start
  (ld hl 0)
  (halt))

(asmproc silly-infinite-loop
  (ld a 0)
  (label loop
    (inc a)
    (jp loop)
    (label silly-label))
  (nop)
  (ret))

(asmproc label-namespaces
  (jp silly-infinite-loop.loop)
  (jp silly-infinite-loop.loop.silly-label))

(asmproc lisp-code-inside
  (ld hl 0)
  (ld hl (* 1 2 3 4 5 6))
  (ret))

(asmproc sprite
  (struct x 2 y 2 color 1))

(save-image "test.rom")
(save-symbols "test.sym")

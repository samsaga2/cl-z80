(ql:quickload "cl-z80")

(defpackage #:example
  (:use #:cl #:cl-z80))

(in-package #:example)

(asm (org #x4000)
     (dw start 0 0 0 0 0 0))

(defproc start
  (ld hl 0)
  (halt))

(defproc silly-infinite-loop
  (ld a 0)
  (with-label loop
    (inc a)
    (jp loop)
    (label silly-label))
  (nop))

(defproc label-namespaces
  (jp silly-infinite-loop.loop)
  (jp silly-infinite-loop.loop.silly-label))

(defproc lisp-code-inside
  (ld hl 0)
  (ld hl $ (* 1 2 3 4 5 6)))

(save-image "test.rom")
(save-symbols "test.sym")

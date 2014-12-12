(load "z80.lisp")

(asm (org #x4000)
     (dw start 0 0 0 0 0 0))

(defproc start
  (ld hl 0)
  (halt))

(defproc silly-infinite-loop
  (ld a 0)
  (label loop)
  (inc a)
  (jp loop))

(save-image-and-quit "test.rom")

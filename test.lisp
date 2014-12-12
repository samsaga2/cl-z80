(load "z80.lisp")

(asm (org #x4000)
     (dw start 0 0 0 0 0 0))

(defproc start
  (ld hl 0)
  (halt))

(save-image-and-quit "test.rom")

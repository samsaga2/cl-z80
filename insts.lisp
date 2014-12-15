(in-package #:cl-z80)

;; special funcs
(definst (org number)
  (setq *org* *number*))

(definst (size number)
  (adjust-array *image* *number*))

(definst (label sym)
  (set-label-address *sym* (+ *org* *ip*)))

(definst (with-label sym lst)
  (set-label-address *sym* (+ *org* *ip*))
  (push-namespace *sym*)
  (asm-insts *lst*)
  (pop-namespace))

(definst (equ sym number)
  (set-label-address *sym* *number*))

(definst (db lst)
  (flet ((split-str (str)
           (loop for i across str do (emit-byte (char-int i)))))
    (loop for i in *lst* do
         (cond ((numberp i) (apply #'emit (list i)))
               ((stringp i) (split-str i))
               (t (format nil "unknown value in db: ~a~%" i))))))

(definst (dw lst)
  (loop for i in *lst* append
       (apply #'emit
        (if (numberp i)
            (list (low-word i) (high-word i))
            (list (list 'low-word i) (list 'high-word i))))))

(definst (repeat number lst)
  (loop for i below *number* do
       (asm-insts *lst*)))

;; http://nemesis.lonestar.org/computers/tandy/software/apps/m4/qd/opcodes.html 
;; 8 bit transfer instructions
(definst (ld (bc) a) (emit #x02))
(definst (ld (de) a) (emit #x12))
(definst (ld (hl) a) (emit #x77))
(definst (ld (hl) b) (emit #x70))
(definst (ld (hl) byte) (emit #x36 *byte*))
(definst (ld (hl) c) (emit #x71))
(definst (ld (hl) d) (emit #x72))
(definst (ld (hl) e) (emit #x73))
(definst (ld (hl) h) (emit #x74))
(definst (ld (hl) l) (emit #x75))
(definst (ld (ix index) a) (emit #xdd #x77 *index*))
(definst (ld (ix index) b) (emit #xdd #x70 *index*))
(definst (ld (ix index) byte) (emit #xdd #x36 *index* *byte*)) ; #x36=#x76
(definst (ld (ix index) c) (emit #xdd #x71 *index*))
(definst (ld (ix index) d) (emit #xdd #x72 *index*))
(definst (ld (ix index) e) (emit #xdd #x73 *index*))
(definst (ld (ix index) h) (emit #xdd #x74 *index*))
(definst (ld (ix index) l) (emit #xdd #x75 *index*))
(definst (ld (iy index) a) (emit #xfd #x77 *index*))
(definst (ld (iy index) b) (emit #xfd #x70 *index*))
(definst (ld (iy index) byte) (emit #xfd #x36 *index* *byte*)) ; #x36=#x76
(definst (ld (iy index) c) (emit #xfd #x71 *index*))
(definst (ld (iy index) d) (emit #xfd #x72 *index*))
(definst (ld (iy index) e) (emit #xfd #x73 *index*))
(definst (ld (iy index) h) (emit #xfd #x74 *index*))
(definst (ld (iy index) l) (emit #xfd #x75 *index*))
(definst (ld (word) a) (emit #x32 *lword* *hword*))
(definst (ld a (bc)) (emit #x0a))
(definst (ld a (de)) (emit #x1a))
(definst (ld a (hl)) (emit #x7e))
(definst (ld a (ix index)) (emit #xdd #x7e *index*))
(definst (ld a (iy index)) (emit #xfd #x7e *index*))
(definst (ld a (word)) (emit #x3a *lword* *hword*))
(definst (ld a a) (emit #x7f))
(definst (ld a b) (emit #x78))
(definst (ld a byte) (emit #x3e *byte*))
(definst (ld a c) (emit #x79))
(definst (ld a d) (emit #x7a))
(definst (ld a e) (emit #x7b))
(definst (ld a h) (emit #x7c))
(definst (ld a l) (emit #x7d))
(definst (ld b (hl)) (emit #x46))
(definst (ld b (ix index)) (emit #xdd #x46 *index*))
(definst (ld b (iy index)) (emit #xfd #x46 *index*))
(definst (ld b a) (emit #x47))
(definst (ld b b) (emit #x40))
(definst (ld b byte) (emit #x06 *byte*))
(definst (ld b c) (emit #x41))
(definst (ld b d) (emit #x42))
(definst (ld b e) (emit #x43))
(definst (ld b h) (emit #x44))
(definst (ld b l) (emit #x45))
(definst (ld c (hl)) (emit #x4e))
(definst (ld c (ix index)) (emit #xdd #x4e *index*))
(definst (ld c (iy index)) (emit #xfd #x4e *index*))
(definst (ld c a) (emit #x4f))
(definst (ld c b) (emit #x48))
(definst (ld c byte) (emit #x0e *byte*))
(definst (ld c c) (emit #x49))
(definst (ld c d) (emit #x4a))
(definst (ld c e) (emit #x4b))
(definst (ld c h) (emit #x4c))
(definst (ld c l) (emit #x4d))
(definst (ld d (hl)) (emit #x56))
(definst (ld d (ix index)) (emit #xdd #x56 *index*))
(definst (ld d (iy index)) (emit #xfd #x56 *index*))
(definst (ld d a) (emit #x57))
(definst (ld d b) (emit #x50))
(definst (ld d byte) (emit #x16 *byte*))
(definst (ld d c) (emit #x51))
(definst (ld d d) (emit #x52))
(definst (ld d e) (emit #x53))
(definst (ld d h) (emit #x54))
(definst (ld d l) (emit #x55))
(definst (ld e (hl)) (emit #x5e))
(definst (ld e (ix index)) (emit #xdd #x5e *index*))
(definst (ld e (iy index)) (emit #xfd #x5e *index*))
(definst (ld e a) (emit #x5f))
(definst (ld e b) (emit #x58))
(definst (ld e byte) (emit #x1e *byte*))
(definst (ld e c) (emit #x59))
(definst (ld e d) (emit #x5a))
(definst (ld e e) (emit #x5b))
(definst (ld e h) (emit #x5c))
(definst (ld e l) (emit #x5d))
(definst (ld h (hl)) (emit #x66))
(definst (ld h (ix index)) (emit #xdd #x66 *index*))
(definst (ld h (iy index)) (emit #xfd #x66 *index*))
(definst (ld h a) (emit #x67))
(definst (ld h b) (emit #x60))
(definst (ld h byte) (emit #x26 *byte*))
(definst (ld h c) (emit #x61))
(definst (ld h d) (emit #x62))
(definst (ld h e) (emit #x63))
(definst (ld h h) (emit #x64))
(definst (ld h l) (emit #x65))
(definst (ld l (hl)) (emit #x6e))
(definst (ld l (ix index)) (emit #xdd #x6e *index*))
(definst (ld l (iy index)) (emit #xfd #x6e *index*))
(definst (ld l a) (emit #x6f))
(definst (ld l b) (emit #x68))
(definst (ld l byte) (emit #x2e *byte*))
(definst (ld l c) (emit #x69))
(definst (ld l d) (emit #x6a))
(definst (ld l e) (emit #x6b))
(definst (ld l h) (emit #x6c))
(definst (ld l l) (emit #x6d))

;; 16 bit transfer instructions
(definst (ld bc word) (emit #x01 *lword* *hword*))
(definst (ld de word) (emit #x11 *lword* *hword*))
(definst (ld hl word) (emit #x21 *lword* *hword*))
(definst (ld sp word) (emit #x31 *lword* *hword*))
(definst (ld ix word) (emit #xdd #x21 *lword* *hword*))
(definst (ld iy word) (emit #xfd #x21 *lword* *hword*))
(definst (ld hl (word)) (emit #x2a *lword* *hword*))
(definst (ld bc (word)) (emit #xed #x4b *lword* *hword*))
(definst (ld de (word)) (emit #xed #x5b *lword* *hword*))
;; (definst (ld hl (word)) (emit #xed #x6b *lword* *hword*))
(definst (ld sp (word)) (emit #xed #x7b *lword* *hword*))
(definst (ld ix (word)) (emit #xdd #x2a *lword* *hword*))
(definst (ld iy (word)) (emit #xfd #x2a *lword* *hword*))
(definst (ld (word) hl) (emit #x22 *lword* *hword*))
(definst (ld (word) bc) (emit #xed #x43 *lword* *hword*))
(definst (ld (word) de) (emit #xed #x53 *lword* *hword*))
;; (definst (ld (word) hl) (emit #xed #x6b *lword* *hword*))
(definst (ld (word) ix) (emit #xdd #x22 *lword* *hword*))
(definst (ld (word) iy) (emit #xdd #x22 *lword* *hword*))
(definst (ld (word) sp) (emit #xed #x73 *lword* *hword*))
(definst (ld sp hl) (emit #xf9))
(definst (ld sp ix) (emit #xdd #xf9))
(definst (ld sp iy) (emit #xfd #xf9))

;; register exchange instructions
(definst (ex de hl) (emit #xeb))
(definst (ex (sp) hl) (emit #xe3))
(definst (ex (sp) ix) (emit #xdd #xe3))
(definst (ex (sp) iy) (emit #xfd #xe3))
(definst (ex af af) (emit #x08))
(definst (exx) (emit #xd9))

;; add byte instructions
(definst (add a a) (emit #x87))
(definst (add a b) (emit #x80))
(definst (add a c) (emit #x81))
(definst (add a d) (emit #x82))
(definst (add a e) (emit #x83))
(definst (add a h) (emit #x84))
(definst (add a l) (emit #x85))
(definst (add a (hl)) (emit #x86))
(definst (add a (ix index)) (emit #xdd #x86 *index*))
(definst (add a (iy index)) (emit #xfd #x86 *index*))
(definst (add a byte) (emit #xc6 *byte*))

;; add byte carry-in instructions
(definst (adc a a) (emit #x8f))
(definst (adc a b) (emit #x88))
(definst (adc a c) (emit #x89))
(definst (adc a d) (emit #x8a))
(definst (adc a e) (emit #x8b))
(definst (adc a h) (emit #x8c))
(definst (adc a l) (emit #x8d))
(definst (adc a (hl)) (emit #x8e))
(definst (adc a (ix index)) (emit #xdd #x8e *index*))
(definst (adc a (iy index)) (emit #xfd #x8e *index*))
(definst (adc a byte) (emit #xce *byte*))

;; substract byte instructions
(definst (sub a) (emit #x97))
(definst (sub b) (emit #x90))
(definst (sub c) (emit #x91))
(definst (sub d) (emit #x92))
(definst (sub e) (emit #x93))
(definst (sub h) (emit #x94))
(definst (sub l) (emit #x95))
(definst (sub (hl)) (emit #x96))
(definst (sub (ix index)) (emit #xdd #x96 *index*))
(definst (sub (iy index)) (emit #xfd #x96 *index*))
(definst (sub byte) (emit #xd6 *byte*))

;; substract byte with borrow-in instructions
(definst (sbc a) (emit #x9f))
(definst (sbc b) (emit #x98))
(definst (sbc c) (emit #x99))
(definst (sbc d) (emit #x9a))
(definst (sbc e) (emit #x9b))
(definst (sbc h) (emit #x9c))
(definst (sbc l) (emit #x9d))
(definst (sbc (hl)) (emit #x9e))
(definst (sbc (ix index)) (emit #xdd #x9e *index*))
(definst (sbc (iy index)) (emit #xfd #x9e *index*))
(definst (sbc byte) (emit #xde *byte*))

;; double byte add instructions
(definst (add hl bc) (emit #x09))
(definst (add hl de) (emit #x19))
(definst (add hl hl) (emit #x29))
(definst (add hl sp) (emit #x39))
(definst (add ix bc) (emit #xdd #x09))
(definst (add ix de) (emit #xdd #x19))
(definst (add ix ix) (emit #xdd #x29))
(definst (add ix sp) (emit #xdd #x39))
(definst (add iy bc) (emit #xfd #x09))
(definst (add iy de) (emit #xfd #x19))
(definst (add iy iy) (emit #xfd #x29))
(definst (add iy sp) (emit #xfd #x39))

;; double byte add with carry-in instructions
(definst (adc) (emit #xed #x4a))
(definst (adc hl de) (emit #xed #x5a))
(definst (adc hl hl) (emit #xed #x6a))
(definst (adc hl sp) (emit #xed #x7a))

;; control instructions
(definst (di) (emit #xf3))
(definst (ei) (emit #xfb))
(definst (im 0) (emit #xed #x46))
(definst (im 1) (emit #xed #x56))
(definst (im 2) (emit #xed #x5e))
(definst (ld a i) (emit #xed #x57))
(definst (ld i a) (emit #xed #x47))
(definst (ld a r) (emit #xed #x5f))
(definst (ld r a) (emit #xed #x4f))
(definst (nop) (emit #x00))
(definst (halt) (emit #x76))

;; increment byte instructions
(definst (inc a) (emit #x3c))
(definst (inc b) (emit #x04))
(definst (inc c) (emit #x0c))
(definst (inc d) (emit #x14))
(definst (inc e) (emit #x1c))
(definst (inc h) (emit #x24))
(definst (inc l) (emit #x2c))
(definst (inc (hl)) (emit #x34))
(definst (inc (ix index)) (emit #xdd #x34 *index*))
(definst (inc (iy index)) (emit #xfd #x34 *index*))

;; decrement byte instructions
(definst (dec a) (emit #x3d))
(definst (dec b) (emit #x05))
(definst (dec c) (emit #x0d))
(definst (dec d) (emit #x15))
(definst (dec e) (emit #x1d))
(definst (dec h) (emit #x25))
(definst (dec l) (emit #x2d))
(definst (dec (hl)) (emit #x35))
(definst (dec (ix index)) (emit #xdd #x35 *index*))
(definst (dec (iy index)) (emit #xfd #x35 *index*))

;; increment register pair instructions
(definst (inc bc) (emit #x03))
(definst (inc de) (emit #x13))
(definst (inc hl) (emit #x23))
(definst (inc sp) (emit #x33))
(definst (inc ix) (emit #xdd #x23))
(definst (inc iy) (emit #xfd #x23))

;; decrement register pair instructions
(definst (dec bc) (emit #x0b))
(definst (dec de) (emit #x1b))
(definst (dec hl) (emit #x2b))
(definst (dec sp) (emit #x3b))
(definst (dec ix) (emit #xdd #x2b))
(definst (dec iy) (emit #xfd #x2b))

;; special accumulator and flag instructions
(definst (daa) (emit #x27))
(definst (cpl) (emit #x2f))
(definst (scf) (emit #x37))
(definst (ccf) (emit #x3f))
(definst (neg) (emit #xed #x44))

;; rotate instructions
(definst (rlca) (emit #x07))
(definst (rrca) (emit #x0f))
(definst (rla) (emit #x17))
(definst (rra) (emit #x1f))
(definst (rld) (emit #xed #x6f))
(definst (rrd) (emit #xed #x67))
(definst (rlc a) (emit #xcb #x07))
(definst (rlc b) (emit #xcb #x00))
(definst (rlc c) (emit #xcb #x01))
(definst (rlc d) (emit #xcb #x02))
(definst (rlc e) (emit #xcb #x03))
(definst (rlc h) (emit #xcb #x04))
(definst (rlc l) (emit #xcb #x05))
(definst (rlc (hl)) (emit #xcb #x06))
(definst (rlc (ix index)) (emit #xdd #xcb *index* #x06))
(definst (rlc (iy index)) (emit #xfd #xcb *index* #x06))
(definst (rl a) (emit #xcb #x17))
(definst (rl b) (emit #xcb #x10))
(definst (rl c) (emit #xcb #x11))
(definst (rl d) (emit #xcb #x12))
(definst (rl e) (emit #xcb #x13))
(definst (rl h) (emit #xcb #x14))
(definst (rl l) (emit #xcb #x15))
(definst (rl (hl)) (emit #xcb #x16))
(definst (rl (ix index)) (emit #xdd #xcb *index* #x16))
(definst (rl (iy index)) (emit #xfd #xcb *index* #x16))
(definst (rrc a) (emit #xcb #x0f))
(definst (rrc b) (emit #xcb #x08))
(definst (rrc c) (emit #xcb #x09))
(definst (rrc d) (emit #xcb #x0a))
(definst (rrc e) (emit #xcb #x0b))
(definst (rrc h) (emit #xcb #x0c))
(definst (rrc l) (emit #xcb #x0d))
(definst (rrc (hl)) (emit #xcb #x0e))
(definst (rrc (ix index)) (emit #xdd #xcb *index* #x0e))
(definst (rrc (iy index)) (emit #xfd #xcb *index* #x0e))
(definst (rl a) (emit #xcb #x1f))
(definst (rl b) (emit #xcb #x18))
(definst (rl c) (emit #xcb #x19))
(definst (rl d) (emit #xcb #x1a))
(definst (rl e) (emit #xcb #x1b))
(definst (rl h) (emit #xcb #x1c))
(definst (rl l) (emit #xcb #x1d))
(definst (rl (hl)) (emit #xcb #x1e))
(definst (rl (ix index)) (emit #xdd #xcb *index* #x1e))
(definst (rl (iy index)) (emit #xfd #xcb *index* #x1e))

;; logical byte instructions
(definst (and a) (emit #xa7))
(definst (and b) (emit #xa0))
(definst (and c) (emit #xa1))
(definst (and d) (emit #xa2))
(definst (and e) (emit #xa3))
(definst (and h) (emit #xa4))
(definst (and l) (emit #xa5))
(definst (and (hl)) (emit #xa6))
(definst (and (ix index)) (emit #xdd #xa6 *index*))
(definst (and (iy index)) (emit #xfd #xa6 *index*))
(definst (and byte) (emit #xe6 *byte*))
(definst (xor a) (emit #xaf))
(definst (xor b) (emit #xa8))
(definst (xor c) (emit #xa9))
(definst (xor d) (emit #xaa))
(definst (xor e) (emit #xab))
(definst (xor h) (emit #xac))
(definst (xor l) (emit #xad))
(definst (xor (hl)) (emit #xae))
(definst (xor (ix index)) (emit #xdd #xae *index*))
(definst (xor (iy index)) (emit #xfd #xae *index*))
(definst (xor byte) (emit #xee *byte*))
(definst (or a) (emit #xb7))
(definst (or b) (emit #xb0))
(definst (or c) (emit #xb1))
(definst (or d) (emit #xb2))
(definst (or e) (emit #xb3))
(definst (or h) (emit #xb4))
(definst (or l) (emit #xb5))
(definst (or (hl)) (emit #xb6))
(definst (or (ix index)) (emit #xdd #xb6 *index*))
(definst (or (iy index)) (emit #xfd #xb6 *index*))
(definst (or byte) (emit #xf6 *byte*))
(definst (cp a) (emit #xbf))
(definst (cp b) (emit #xb8))
(definst (cp c) (emit #xb9))
(definst (cp d) (emit #xba))
(definst (cp e) (emit #xbb))
(definst (cp h) (emit #xbc))
(definst (cp l) (emit #xbd))
(definst (cp (hl)) (emit #xbe))
(definst (cp (ix index)) (emit #xdd #xbe *index*))
(definst (cp (iy index)) (emit #xfd #xbe *index*))
(definst (cp byte) (emit #xfe *byte*))
(definst (cpi) (emit #xed #xa1))
(definst (cpir) (emit #xed #xb1))
(definst (cpd) (emit #xed #xa9))
(definst (cpdr) (emit #xed #xb9))

;; branch control/program counter load instructions
(definst (jp word) (emit #xc3 *lword* *hword*))
(definst (jp nz word) (emit #xc2 *lword* *hword*))
(definst (jp z word) (emit #xca *lword* *hword*))
(definst (jp nc word) (emit #xd2 *lword* *hword*))
(definst (jp c word) (emit #xda *lword* *hword*))
(definst (jp po word) (emit #xe2 *lword* *hword*))
(definst (jp pe word) (emit #xea *lword* *hword*))
(definst (jp p word) (emit #xf2 *lword* *hword*))
(definst (jp m word) (emit #xfa *lword* *hword*))
(definst (jp (hl)) (emit #xe9))
(definst (jp (ix)) (emit #xdd #xe9))
(definst (jp (iy)) (emit #xfd #xe9))
(definst (jr index) (emit #x18 *index*))
(definst (jr nz index) (emit #x20 *index*))
(definst (jr z index) (emit #x28 *index*))
(definst (jr nc index) (emit #x30 *index*))
(definst (jr c index) (emit #x38 *index*))
(definst (djnz index) (emit #x10 *index*))
(definst (call word) (emit #xcd *lword* *hword*))
(definst (call nz word) (emit #xc4 *lword* *hword*))
(definst (call z word) (emit #xcc *lword* *hword*))
(definst (call nc word) (emit #xd4 *lword* *hword*))
(definst (call c word) (emit #xdc *lword* *hword*))
(definst (call po word) (emit #xe4 *lword* *hword*))
(definst (call pe word) (emit #xec *lword* *hword*))
(definst (call p word) (emit #xf4 *lword* *hword*))
(definst (call m word) (emit #xfc *lword* *hword*))
(definst (ret) (emit #xc9))
(definst (ret nz) (emit #xc0))
(definst (ret z) (emit #xc8))
(definst (ret nc) (emit #xd0))
(definst (ret c) (emit #xd8))
(definst (ret po) (emit #xe0))
(definst (ret pe) (emit #xe8))
(definst (ret p) (emit #xf0))
(definst (ret m) (emit #xf8))
(definst (reti) (emit #xed #x4d))
(definst (retn) (emit #xed #x45))
(definst (rst #x0) (emit #xc7))
(definst (rst #x8) (emit #xcf))
(definst (rst #x10) (emit #xd7))
(definst (rst #x18) (emit #xdf))
(definst (rst #x20) (emit #xe7))
(definst (rst #x28) (emit #xef))
(definst (rst #x30) (emit #xf7))
(definst (rst #x38) (emit #xff))

;; stack operation instructions
(definst (push bc) (emit #xc5))
(definst (push de) (emit #xd5))
(definst (push hl) (emit #xe5))
(definst (push af) (emit #xf5))
(definst (push ix) (emit #xdd #xe5))
(definst (push iy) (emit #xfd #xe5))
(definst (pop bc) (emit #xc1))
(definst (pop de) (emit #xd1))
(definst (pop hl) (emit #xe1))
(definst (pop af) (emit #xf1))
(definst (pop ix) (emit #xdd #xe1))
(definst (pop iy) (emit #xfd #xe1))

;; input/output instructions
(definst (in a (byte)) (emit #xdb *byte*))
(definst (in a (c)) (emit #xed #x78))
(definst (in b (c)) (emit #xed #x40))
(definst (in c (c)) (emit #xed #x48))
(definst (in d (c)) (emit #xed #x50))
(definst (in) (emit #xed #x58))
(definst (in h (c)) (emit #xed #x60))
(definst (in l (c)) (emit #xed #x68))
(definst (ini) (emit #xed #xa2))
(definst (inir) (emit #xed #xb2))
(definst (ind) (emit #xed #xaa))
(definst (indr) (emit #xed #xba))
(definst (out (byte) a) (emit #xd3 #x20))
(definst (out (c) a) (emit #xed #x79))
(definst (out (c) b) (emit #xed #x41))
(definst (out (c) c) (emit #xed #x49))
(definst (out (c) d) (emit #xed #x51))
(definst (out (c) e) (emit #xed #x59))
(definst (out (c) h) (emit #xed #x61))
(definst (out (c) l) (emit #xed #x69))
(definst (outi) (emit #xed #xa3))
(definst (otir) (emit #xed #xb3))
(definst (outd) (emit #xed #xab))
(definst (otdr) (emit #xed #xbb))

;; data transfer instructions
(definst (ldi) (emit #xed #xa0))
(definst (ldir) (emit #xed #xb0))
(definst (ldd) (emit #xed #xa8))
(definst (lddr) (emit #xed #xb8))

;; bit manipulation instructions
(definst (bit 0 a) (emit #xcb #x47))
(definst (bit 0 b) (emit #xcb #x40))
(definst (bit 0 c) (emit #xcb #x41))
(definst (bit 0 d) (emit #xcb #x42))
(definst (bit 0 e) (emit #xcb #x43))
(definst (bit 0 h) (emit #xcb #x44))
(definst (bit 0 l) (emit #xcb #x45))
(definst (bit 0 (hl)) (emit #xcb #x46))
(definst (bit 0 (ix index)) (emit #xdd #xcb *index* #x46))
(definst (bit 0 (iy index)) (emit #xfd #xcb *index* #x46))
(definst (bit 1 a) (emit #xcb #x4f))
(definst (bit 1 b) (emit #xcb #x48))
(definst (bit 1 c) (emit #xcb #x49))
(definst (bit 1 d) (emit #xcb #x4a))
(definst (bit 1 e) (emit #xcb #x4b))
(definst (bit 1 h) (emit #xcb #x4c))
(definst (bit 1 l) (emit #xcb #x4d))
(definst (bit 1 (hl)) (emit #xcb #x4e))
(definst (bit 1 (ix index)) (emit #xdd #xcb *index* #x4e))
(definst (bit 1 (iy index)) (emit #xfd #xcb *index* #x4e))
(definst (bit 2 a) (emit #xcb #x57))
(definst (bit 2 b) (emit #xcb #x50))
(definst (bit 2 c) (emit #xcb #x51))
(definst (bit 2 d) (emit #xcb #x52))
(definst (bit 2 e) (emit #xcb #x53))
(definst (bit 2 h) (emit #xcb #x54))
(definst (bit 2 l) (emit #xcb #x55))
(definst (bit 2 (hl)) (emit #xcb #x56))
(definst (bit 2 (ix index)) (emit #xdd #xcb *index* #x56))
(definst (bit 2 (iy index)) (emit #xfd #xcb *index* #x56))
(definst (bit 3 a) (emit #xcb #x5f))
(definst (bit 3 b) (emit #xcb #x58))
(definst (bit 3 c) (emit #xcb #x59))
(definst (bit 3 d) (emit #xcb #x5a))
(definst (bit 3 e) (emit #xcb #x5b))
(definst (bit 3 h) (emit #xcb #x5c))
(definst (bit 3 l) (emit #xcb #x5d))
(definst (bit 3 (hl)) (emit #xcb #x5e))
(definst (bit 3 (ix index)) (emit #xdd #xcb *index* #x5e))
(definst (bit 3 (iy index)) (emit #xfd #xcb *index* #x5e))
(definst (bit 4 a) (emit #xcb #x67))
(definst (bit 4 b) (emit #xcb #x60))
(definst (bit 4 c) (emit #xcb #x61))
(definst (bit 4 d) (emit #xcb #x62))
(definst (bit 4 e) (emit #xcb #x63))
(definst (bit 4 h) (emit #xcb #x64))
(definst (bit 4 l) (emit #xcb #x65))
(definst (bit 4 (hl)) (emit #xcb #x66))
(definst (bit 4 (ix index)) (emit #xdd #xcb *index* #x66))
(definst (bit 4 (iy index)) (emit #xfd #xcb *index* #x66))
(definst (bit 5 a) (emit #xcb #x6f))
(definst (bit 5 b) (emit #xcb #x68))
(definst (bit 5 c) (emit #xcb #x69))
(definst (bit 5 d) (emit #xcb #x6a))
(definst (bit 5 e) (emit #xcb #x6b))
(definst (bit 5 h) (emit #xcb #x6c))
(definst (bit 5 l) (emit #xcb #x6d))
(definst (bit 5 (hl)) (emit #xcb #x6e))
(definst (bit 5 (ix index)) (emit #xdd #xcb *index* #x6e))
(definst (bit 5 (iy index)) (emit #xfd #xcb *index* #x6e))
(definst (bit 6 a) (emit #xcb #x77))
(definst (bit 6 b) (emit #xcb #x70))
(definst (bit 6 c) (emit #xcb #x71))
(definst (bit 6 d) (emit #xcb #x72))
(definst (bit 6 e) (emit #xcb #x73))
(definst (bit 6 h) (emit #xcb #x74))
(definst (bit 6 l) (emit #xcb #x75))
(definst (bit 6 (hl)) (emit #xcb #x76))
(definst (bit 6 (ix index)) (emit #xdd #xcb *index* #x76))
(definst (bit 6 (iy index)) (emit #xfd #xcb *index* #x76))
(definst (bit 7 a) (emit #xcb #x7f))
(definst (bit 7 b) (emit #xcb #x78))
(definst (bit 7 c) (emit #xcb #x79))
(definst (bit 7 d) (emit #xcb #x7a))
(definst (bit 7 e) (emit #xcb #x7b))
(definst (bit 7 h) (emit #xcb #x7c))
(definst (bit 7 l) (emit #xcb #x7d))
(definst (bit 7 (hl)) (emit #xcb #x7e))
(definst (bit 7 (ix index)) (emit #xdd #xcb *index* #x7e))
(definst (bit 7 (iy index)) (emit #xfd #xcb *index* #x7e))
(definst (res 0 a) (emit #xcb #x87))
(definst (res 0 b) (emit #xcb #x80))
(definst (res 0 c) (emit #xcb #x81))
(definst (res 0 d) (emit #xcb #x82))
(definst (res 0 e) (emit #xcb #x83))
(definst (res 0 h) (emit #xcb #x84))
(definst (res 0 l) (emit #xcb #x85))
(definst (res 0 (hl)) (emit #xcb #x86))
(definst (res 0 (ix index)) (emit #xdd #xcb *index* #x86))
(definst (res 0 (iy index)) (emit #xfd #xcb *index* #x86))
(definst (res 1 a) (emit #xcb #x8f))
(definst (res 1 b) (emit #xcb #x88))
(definst (res 1 c) (emit #xcb #x89))
(definst (res 1 d) (emit #xcb #x8a))
(definst (res 1 e) (emit #xcb #x8b))
(definst (res 1 h) (emit #xcb #x8c))
(definst (res 1 l) (emit #xcb #x8d))
(definst (res 1 (hl)) (emit #xcb #x8e))
(definst (res 1 (ix index)) (emit #xdd #xcb *index* #x8e))
(definst (res 1 (iy index)) (emit #xfd #xcb *index* #x8e))
(definst (res 2 a) (emit #xcb #x97))
(definst (res 2 b) (emit #xcb #x90))
(definst (res 2 c) (emit #xcb #x91))
(definst (res 2 d) (emit #xcb #x92))
(definst (res 2 e) (emit #xcb #x93))
(definst (res 2 h) (emit #xcb #x94))
(definst (res 2 l) (emit #xcb #x95))
(definst (res 2 (hl)) (emit #xcb #x96))
(definst (res 2 (ix index)) (emit #xdd #xcb *index* #x96))
(definst (res 2 (iy index)) (emit #xfd #xcb *index* #x96))
(definst (res 3 a) (emit #xcb #x9f))
(definst (res 3 b) (emit #xcb #x98))
(definst (res 3 c) (emit #xcb #x99))
(definst (res 3 d) (emit #xcb #x9a))
(definst (res 3 e) (emit #xcb #x9b))
(definst (res 3 h) (emit #xcb #x9c))
(definst (res 3 l) (emit #xcb #x9d))
(definst (res 3 (hl)) (emit #xcb #x9e))
(definst (res 3 (ix index)) (emit #xdd #xcb *index* #x9e))
(definst (res 3 (iy index)) (emit #xfd #xcb *index* #x9e))
(definst (res 4 a) (emit #xcb #xa7))
(definst (res 4 b) (emit #xcb #xa0))
(definst (res 4 c) (emit #xcb #xa1))
(definst (res 4 d) (emit #xcb #xa2))
(definst (res 4 e) (emit #xcb #xa3))
(definst (res 4 h) (emit #xcb #xa4))
(definst (res 4 l) (emit #xcb #xa5))
(definst (res 4 (hl)) (emit #xcb #xa6))
(definst (res 4 (ix index)) (emit #xdd #xcb *index* #xa6))
(definst (res 4 (iy index)) (emit #xfd #xcb *index* #xa6))
(definst (res 5 a) (emit #xcb #xaf))
(definst (res 5 b) (emit #xcb #xa8))
(definst (res 5 c) (emit #xcb #xa9))
(definst (res 5 d) (emit #xcb #xaa))
(definst (res 5 e) (emit #xcb #xab))
(definst (res 5 h) (emit #xcb #xac))
(definst (res 5 l) (emit #xcb #xad))
(definst (res 5 (hl)) (emit #xcb #xae))
(definst (res 5 (ix index)) (emit #xdd #xcb *index* #xae))
(definst (res 5 (iy index)) (emit #xfd #xcb *index* #xae))
(definst (res 6 a) (emit #xcb #xb7))
(definst (res 6 b) (emit #xcb #xb0))
(definst (res 6 c) (emit #xcb #xb1))
(definst (res 6 d) (emit #xcb #xb2))
(definst (res 6 e) (emit #xcb #xb3))
(definst (res 6 h) (emit #xcb #xb4))
(definst (res 6 l) (emit #xcb #xb5))
(definst (res 6 (hl)) (emit #xcb #xb6))
(definst (res 6 (ix index)) (emit #xdd #xcb *index* #xb6))
(definst (res 6 (iy index)) (emit #xfd #xcb *index* #xb6))
(definst (res 7 a) (emit #xcb #xbf))
(definst (res 7 b) (emit #xcb #xb8))
(definst (res 7 c) (emit #xcb #xb9))
(definst (res 7 d) (emit #xcb #xba))
(definst (res 7 e) (emit #xcb #xbb))
(definst (res 7 h) (emit #xcb #xbc))
(definst (res 7 l) (emit #xcb #xbd))
(definst (res 7 (hl)) (emit #xcb #xbe))
(definst (res 7 (ix index)) (emit #xdd #xcb *index* #xbe))
(definst (res 7 (iy index)) (emit #xfd #xcb *index* #xbe))
(definst (set 0 a) (emit #xcb #xc7))
(definst (set 0 b) (emit #xcb #xc0))
(definst (set 0 c) (emit #xcb #xc1))
(definst (set 0 d) (emit #xcb #xc2))
(definst (set 0 e) (emit #xcb #xc3))
(definst (set 0 h) (emit #xcb #xc4))
(definst (set 0 l) (emit #xcb #xc5))
(definst (set 0 (hl)) (emit #xcb #xc6))
(definst (set 0 (ix index)) (emit #xdd #xcb *index* #xc6))
(definst (set 0 (iy index)) (emit #xfd #xcb *index* #xc6))
(definst (set 1 a) (emit #xcb #xcf))
(definst (set 1 b) (emit #xcb #xc8))
(definst (set 1 c) (emit #xcb #xc9))
(definst (set 1 d) (emit #xcb #xca))
(definst (set 1 e) (emit #xcb #xcb))
(definst (set 1 h) (emit #xcb #xcc))
(definst (set 1 l) (emit #xcb #xcd))
(definst (set 1 (hl)) (emit #xcb #xce))
(definst (set 1 (ix index)) (emit #xdd #xcb *index* #xce))
(definst (set 1 (iy index)) (emit #xfd #xcb *index* #xce))
(definst (set 2 a) (emit #xcb #xd7))
(definst (set 2 b) (emit #xcb #xd0))
(definst (set 2 c) (emit #xcb #xd1))
(definst (set 2 d) (emit #xcb #xd2))
(definst (set 2 e) (emit #xcb #xd3))
(definst (set 2 h) (emit #xcb #xd4))
(definst (set 2 l) (emit #xcb #xd5))
(definst (set 2 (hl)) (emit #xcb #xd6))
(definst (set 2 (ix index)) (emit #xdd #xcb *index* #xd6))
(definst (set 2 (iy index)) (emit #xfd #xcb *index* #xd6))
(definst (set 3 a) (emit #xcb #xdf))
(definst (set 3 b) (emit #xcb #xd8))
(definst (set 3 c) (emit #xcb #xd9))
(definst (set 3 d) (emit #xcb #xda))
(definst (set 3 e) (emit #xcb #xdb))
(definst (set 3 h) (emit #xcb #xdc))
(definst (set 3 l) (emit #xcb #xdd))
(definst (set 3 (hl)) (emit #xcb #xde))
(definst (set 3 (ix index)) (emit #xdd #xcb *index* #xde))
(definst (set 3 (iy index)) (emit #xfd #xcb *index* #xde))
(definst (set 4 a) (emit #xcb #xe7))
(definst (set 4 b) (emit #xcb #xe0))
(definst (set 4 c) (emit #xcb #xe1))
(definst (set 4 d) (emit #xcb #xe2))
(definst (set 4 e) (emit #xcb #xe3))
(definst (set 4 h) (emit #xcb #xe4))
(definst (set 4 l) (emit #xcb #xe5))
(definst (set 4 (hl)) (emit #xcb #xe6))
(definst (set 4 (ix index)) (emit #xdd #xcb *index* #xe6))
(definst (set 4 (iy index)) (emit #xfd #xcb *index* #xe6))
(definst (set 5 a) (emit #xcb #xef))
(definst (set 5 b) (emit #xcb #xe8))
(definst (set 5 c) (emit #xcb #xe9))
(definst (set 5 d) (emit #xcb #xea))
(definst (set 5 e) (emit #xcb #xeb))
(definst (set 5 h) (emit #xcb #xec))
(definst (set 5 l) (emit #xcb #xed))
(definst (set 5 (hl)) (emit #xcb #xee))
(definst (set 5 (ix index)) (emit #xdd #xcb *index* #xee))
(definst (set 5 (iy index)) (emit #xfd #xcb *index* #xee))
(definst (set 6 a) (emit #xcb #xf7))
(definst (set 6 b) (emit #xcb #xf0))
(definst (set 6 c) (emit #xcb #xf1))
(definst (set 6 d) (emit #xcb #xf2))
(definst (set 6 e) (emit #xcb #xf3))
(definst (set 6 h) (emit #xcb #xf4))
(definst (set 6 l) (emit #xcb #xf5))
(definst (set 6 (hl)) (emit #xcb #xf6))
(definst (set 6 (ix index)) (emit #xdd #xcb *index* #xf6))
(definst (set 6 (iy index)) (emit #xfd #xcb *index* #xf6))
(definst (set 7 a) (emit #xcb #xff))
(definst (set 7 b) (emit #xcb #xf8))
(definst (set 7 c) (emit #xcb #xf9))
(definst (set 7 d) (emit #xcb #xfa))
(definst (set 7 e) (emit #xcb #xfb))
(definst (set 7 h) (emit #xcb #xfc))
(definst (set 7 l) (emit #xcb #xfd))
(definst (set 7 (hl)) (emit #xcb #xfe))
(definst (set 7 (ix index)) (emit #xdd #xcb *index* #xfe))
(definst (set 7 (iy index)) (emit #xfd #xcb *index* #xfe))

;; bit shift instructions
(definst (sla a) (emit #xcb #x27))
(definst (sla b) (emit #xcb #x20))
(definst (sla c) (emit #xcb #x21))
(definst (sla d) (emit #xcb #x22))
(definst (sla e) (emit #xcb #x23))
(definst (sla h) (emit #xcb #x24))
(definst (sla l) (emit #xcb #x25))
(definst (sla (hl)) (emit #xcb #x26))
(definst (sla (ix index)) (emit #xdd #xcb *index* #x26))
(definst (sla (iy index)) (emit #xfd #xcb *index* #x26))
(definst (sra a) (emit #xcb #x2f))
(definst (sra b) (emit #xcb #x28))
(definst (sra c) (emit #xcb #x29))
(definst (sra d) (emit #xcb #x2a))
(definst (sra e) (emit #xcb #x2b))
(definst (sra h) (emit #xcb #x2c))
(definst (sra l) (emit #xcb #x2d))
(definst (sra (hl)) (emit #xcb2e))
(definst (sra (ix index)) (emit #xdd #xcb *index* #x2e))
(definst (sra (iy index)) (emit #xfd #xcb *index* #x2e))
(definst (srl a) (emit #xcb #x3f))
(definst (srl b) (emit #xcb #x38))
(definst (srl c) (emit #xcb #x39))
(definst (srl d) (emit #xcb #x3a))
(definst (srl e) (emit #xcb #x3b))
(definst (srl h) (emit #xcb #x3c))
(definst (srl l) (emit #xcb #x3d))
(definst (srl (hl)) (emit #xcb #x3e))
(definst (srl (ix index)) (emit #xdd #xcb *index* #x3e))
(definst (srl (iy index)) (emit #xfd #xcb *index* #x3e))
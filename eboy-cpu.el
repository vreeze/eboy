;;; eboy-cpu.el --- CPU instructions  -*- lexical-binding: t; -*-
;;; package -- Summary:
;;; Commentary:
;;; Code:
;;(require 'eboy)
(eval-when-compile (require 'cl))
(require 'eboy-macros)

(defvar eboy-cpu nil "All the cpu instruction functions.")
(defvar eboy-cpu-cb nil "All the CB opcode cpu instruction functions.")
(defvar eboy-cpu-halted nil "Bool to indicate if the CPU is halted")

(setq eboy-cpu
      (vector
       (lambda nil "0x00 NOP"
         (incf eboy-clock-cycles 4))
       (lambda nil "0x01 LD BC, $%04x"
         (eboy-set-rBC (eboy-get-short))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x02 LD (BC),A"
         (eboy-mem-write-byte (eboy-get-rBC) eboy-rA)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x03 INC BC "
         (eboy-set-rBC (1+ (eboy-get-rBC)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x04 INC B "
         (eboy-add-byte eboy-rB 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rB 15) (logand (1- eboy-rB) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x05 DEC B (before dec %d)"
         (eboy-dec eboy-rB eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x06 LD B, #0x%02x"
         (setq eboy-rB (eboy-get-byte)) (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x07 RLCA -/- "
         (let ((c (lsh eboy-rA -7)))
           (setq eboy-rA (logior (lsh eboy-rA 1) c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil)
           (eboy-set-flag eboy-flags :C c))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x08 LD (nn),SP"
         (eboy-mem-write-short (eboy-get-short) eboy-sp)
         (incf eboy-pc 2)
         (incf eboy-clock-cycles 20))
       (lambda nil "0x09 ADD HL,BC "
         (let ((hl (eboy-get-rHL)))
           (eboy-set-rHL (+ hl (eboy-get-rBC)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand (eboy-get-rHL) 4095) (logand hl 4095)))
           (eboy-set-flag eboy-flags :C (< (logand (eboy-get-rHL) 65535) (logand hl 65535))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x0A LD A,(BC)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rBC)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x0B DEC BC "
         (eboy-set-rBC (1- (eboy-get-rBC)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x0C INC C "
         (eboy-add-byte eboy-rC 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rC 15) (logand (1- eboy-rC) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x0D DEC C "
         (eboy-dec eboy-rC eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x0E LD C, #0x%02x"
         (setq eboy-rC (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x0F RRCA -/- "
         (let ((c (logand eboy-rA 1)))
           (setq eboy-rA (logand (logior (lsh eboy-rA -1) (lsh c 7)) #xFF))
           (eboy-set-flag eboy-flags :C (= c 1))
           (eboy-set-flag eboy-flags :Z (= eboy-rA 0))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x10 STOP -/- 10 "
         (setq eboy-cpu-halted t)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x11 LD DE, $%04x"
         (eboy-set-rDE (eboy-get-short))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x12 LD (DE),A"
         (eboy-mem-write-byte (eboy-get-rDE) eboy-rA)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x13 INC DE "
         (eboy-set-rDE (1+ (eboy-get-rDE)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x14 INC D "
         (eboy-add-byte eboy-rD 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rD #xf) (logand (1- eboy-rD) #xf)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x15 DEC D "
         (eboy-dec eboy-rD eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x16 LD D, #0x%02x"
         (setq eboy-rD (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x17 RLA -/- "
         (let ((c (= (lsh eboy-rA -7) 1)))
           (setq eboy-rA (logior (lsh eboy-rA 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil)
           (eboy-set-flag eboy-flags :C c))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x18 JR %02d "
         (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte)) 1))
         (incf eboy-clock-cycles 12))
       (lambda nil "0x19 ADD HL,DE "
         (let ((hl (eboy-get-rHL)))
           (eboy-set-rHL (+ hl (eboy-get-rDE)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand (eboy-get-rHL) 4095) (logand hl 4095)))
           (eboy-set-flag eboy-flags :C (< (logand (eboy-get-rHL) 65535) (logand hl 65535))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x1A LD A,(DE)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rDE)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x1B DEC DE "
         (eboy-set-rDE (1- (eboy-get-rDE)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x1C INC E "
         (eboy-add-byte eboy-rE 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rE 15) (logand (1- eboy-rE) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x1D DEC E "
         (eboy-dec eboy-rE eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x1E LD E, #0x%02x"
         (setq eboy-rE (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x1F RRA -/- "
         (let ((c (= (logand eboy-rA #x01) 1)))
           (setq eboy-rA (logand (logior (lsh eboy-rA -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)) #xff))
           (eboy-set-flag eboy-flags :C c)
           (eboy-set-flag eboy-flags :Z nil)
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x20 JR NZ,*"
         (if (null (eboy-get-flag eboy-flags :Z))
             (progn
               (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
               (incf eboy-clock-cycles 4)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x21 LD HL, $%04x"
         (eboy-set-rHL (eboy-get-short))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x22 LD (HLI),A"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
         (eboy-set-rHL (1+ (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x23 INC HL "
         (eboy-set-rHL (1+ (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x24 INC H "
         (eboy-add-byte eboy-rH 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rH 15) (logand (1- eboy-rH) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x25 DEC H "
         (eboy-dec eboy-rH eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x26 LD H, #0x%02x"
         (setq eboy-rH (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x27 DAA -/- "
         (let ((a eboy-rA))
           (if (eboy-get-flag eboy-flags :N)
               (progn (if (eboy-get-flag eboy-flags :H)
                          (progn (setq a (+ a -6))
                                 (setq a (logand a #xFF))))
                      (if (eboy-get-flag eboy-flags :C)
                          (setq a (+  a -96))))
             (progn (if (or (eboy-get-flag eboy-flags :H) (> (logand a #xF) 9))
                        (setq a (+  a #x06)))
                    (if (or (eboy-get-flag eboy-flags :C) (> a #x9F))
                        (setq a (+  a #x60)))))
           (eboy-set-flag eboy-flags :C (or (>= a #x100) (< a 0)))
           (eboy-add-byte a 0)
           (setq eboy-rA a)
           (eboy-set-flag eboy-flags :H nil)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (incf eboy-clock-cycles 4)))
       (lambda nil "0x28 JR Z,* "
         (if (eboy-get-flag eboy-flags :Z)
             (progn
               (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
               (incf eboy-clock-cycles 4)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x29 ADD HL,HL "
         (let ((hl (eboy-get-rHL)))
           (eboy-set-rHL (* 2 hl))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand (eboy-get-rHL) #x7ff) (logand hl #x7ff)))
           (eboy-set-flag eboy-flags :C (< (logand (eboy-get-rHL) #xffff) (logand hl #xffff))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2A LD A,(HLI)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
         (eboy-set-rHL (1+ (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2B DEC HL "
         (eboy-set-rHL (1- (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2C INC L "
         (eboy-add-byte eboy-rL 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rL 15) (logand (1- eboy-rL) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x2D DEC L "
         (eboy-dec eboy-rL eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x2E LD L, #0x%02x"
         (setq eboy-rL (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2F CPL -/- "
         (setq eboy-rA (logand 255 (lognot eboy-rA)))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H t)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x30 JR NC,* "
         (if (null (eboy-get-flag eboy-flags :C))
             (progn
               (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
               (incf eboy-clock-cycles 4)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x31 LD SP, $%04x"
         (setq eboy-sp (eboy-get-short))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x32 LD (HLD),A"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
         (eboy-set-rHL (1- (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x33 INC SP "
         (incf eboy-sp)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x34 INC (HL) "
         (let* ((hl (eboy-get-rHL))
                (data (eboy-mem-read-byte hl)))
           (eboy-add-byte data 1)
           (eboy-mem-write-byte hl data)
           (eboy-set-flag eboy-flags :Z (zerop data))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand data 15) (logand (1- data) 15))))
         (incf eboy-clock-cycles 12))
       (lambda nil "0x35 DEC (HL) "
         (let ((data (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-dec data eboy-flags)
           (eboy-mem-write-byte (eboy-get-rHL) data))
         (incf eboy-clock-cycles 12))
       (lambda nil "0x36 LD (HL),0x%02x"
         (eboy-mem-write-byte (eboy-get-rHL) (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x37 SCF -/- "
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C t)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x38 JR C,* "
         (if (eboy-get-flag eboy-flags :C)
             (progn
               (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
               (incf eboy-clock-cycles 4)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x39 ADD HL,SP "
         (let ((hl (eboy-get-rHL)))
           (eboy-set-rHL (+ hl eboy-sp))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand (eboy-get-rHL) 4095) (logand hl 4095)))
           (eboy-set-flag eboy-flags :C (< (logand (eboy-get-rHL) 65535) (logand hl 65535))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3A LD A,(HLD)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
         (eboy-set-rHL (1- (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3B DEC SP "
         (eboy-add-to-short eboy-sp -1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3C INC A "
         (eboy-add-byte eboy-rA 1)
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H (< (logand eboy-rA 15) (logand (1- eboy-rA) 15)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x3D DEC A "
         (eboy-dec eboy-rA eboy-flags)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x3E LD A,#0x%02x"
         (setq eboy-rA (eboy-get-byte))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3F CCF -/- "
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :Z (if (eboy-get-flag eboy-flags :C) nil t))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x40 LD B,B"
         (setq eboy-rB eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x41 LD B,C"
         (setq eboy-rB eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x42 LD B,D"
         (setq eboy-rB eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x43 LD B,E"
         (setq eboy-rB eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x44 LD B,H"
         (setq eboy-rB eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x45 LD B,L"
         (setq eboy-rB eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x46 LD B,(HL)"
         (setq eboy-rB (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x47 LD B,A"
         (setq eboy-rB eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x48 LD C,B"
         (setq eboy-rC eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x49 LD C,C"
         (setq eboy-rC eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x4A LD C,D"
         (setq eboy-rC eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x4B LD C,E"
         (setq eboy-rC eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x4C LD C,H"
         (setq eboy-rC eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x4D LD C,L"
         (setq eboy-rC eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x4E LD C,(HL)"
         (setq eboy-rC (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x4F LD C,A (0x%02x)"
         (setq eboy-rC eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x50 LD D,B"
         (setq eboy-rD eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x51 LD D,C"
         (setq eboy-rD eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x52 LD D,D"
         (setq eboy-rD eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x53 LD D,E"
         (setq eboy-rD eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x54 LD D,H"
         (setq eboy-rD eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x55 LD D,L"
         (setq eboy-rD eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x56 LD D,(HL)"
         (setq eboy-rD (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x57 LD D,A"
         (setq eboy-rD eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x58 LD E,B"
         (setq eboy-rE eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x59 LD E,C"
         (setq eboy-rE eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x5A LD E,D"
         (setq eboy-rE eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x5B LD E,E"
         (setq eboy-rE eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x5C LD E,H"
         (setq eboy-rE eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x5D LD E,L"
         (setq eboy-rE eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x5E LD E,(HL)"
         (setq eboy-rE (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x5F LD E,A"
         (setq eboy-rE eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x60 LD H,B"
         (setq eboy-rH eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x61 LD H,C"
         (setq eboy-rH eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x62 LD H,D"
         (setq eboy-rH eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x63 LD H,E"
         (setq eboy-rH eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x64 LD H,H"
         (setq eboy-rH eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x65 LD H,L"
         (setq eboy-rH eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x66 LD H,(HL)"
         (setq eboy-rH (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x67 LD H,A"
         (setq eboy-rH eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x68 LD L,B"
         (setq eboy-rL eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x69 LD L,C"
         (setq eboy-rL eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x6A LD L,D"
         (setq eboy-rL eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x6B LD L,E"
         (setq eboy-rL eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x6C LD L,H"
         (setq eboy-rL eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x6D LD L,L"
         (setq eboy-rL eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x6E LD L,(HL)"
         (setq eboy-rL (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x6F LD L,A"
         (setq eboy-rL eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x70 LD (HL),B"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rB)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x71 LD (HL),C"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rC)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x72 LD (HL),D"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rD)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x73 LD (HL),E"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rE)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x74 LD (HL),H"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rH)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x75 LD (HL),L"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rL)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x76 HALT -/- "
         (setq eboy-cpu-halted t)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x77 LD (HL),A"
         (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x78 LD A,B"
         (setq eboy-rA eboy-rB)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x79 LD A,C"
         (setq eboy-rA eboy-rC)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x7A LD A,D"
         (setq eboy-rA eboy-rD)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x7B LD A,E"
         (setq eboy-rA eboy-rE)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x7C LD A,H"
         (setq eboy-rA eboy-rH)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x7D LD A,L"
         (setq eboy-rA eboy-rL)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x7E LD A,(HL)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x7F LD A,A"
         (setq eboy-rA eboy-rA)
         (incf eboy-clock-cycles 4))
       (lambda nil "0x80 ADD A,B"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rB)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x81 ADD A,C"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rC)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x82 ADD A,D"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rD)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x83 ADD A,E"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rE)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x84 ADD A,H"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rH)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x85 ADD A,L"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rL)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x86 ADD A,(HL)"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x87 ADD A,A"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA eboy-rA)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x88 ADC A,B"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rB #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rB c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rB c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x89 ADC A,C"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rC #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rC c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rC c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8A ADC A,D"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rD #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rD c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rD c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8B ADC A,E"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rE #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rE c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rE c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8C ADC A,H"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rH #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rH c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rH c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8D ADC A,L"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rL #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rL c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rL c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8E ADC A,(HL)"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0))
               (hl (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand hl #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA hl c) #x100))
           (eboy-add-byte eboy-rA (+ hl c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x8F ADC A,A"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rA #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rA c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x90 SUB B"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rB))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x91 SUB C"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rC))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x92 SUB D"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rD))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x93 SUB E"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rE))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x94 SUB H"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rH))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x95 SUB L"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rL))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x96 SUB (HL)"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x97 SUB A"
         (let
             ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 eboy-rA))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA 15) (logand oldval 15)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA 255) (logand oldval 255))))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x98 SBC A,B"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rB #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rB c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rB c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x99 SBC A,C"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rC #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rC c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rC c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9A SBC A,D"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rD #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rD c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rD c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9B SBC A,E"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rE #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rE c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rE c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9C SBC A,H"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rH #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rH c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rH c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9D SBC A,L"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand eboy-rL #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA eboy-rL c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ eboy-rL c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9E SBC A,(HL)"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0))
               (hld (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :H (< (- (logand eboy-rA #xF) (logand hld #xF) c) 0))
           (eboy-set-flag eboy-flags :C (< (- eboy-rA hld c) 0))
           (eboy-set-flag eboy-flags :N t)
           (eboy-add-byte eboy-rA (* -1 (+ hld c)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x9F SBC A,A"
         (let ((c (if (eboy-get-flag eboy-flags :C) 255 0)))
           (eboy-set-flag eboy-flags :H (= c 255))
           (eboy-set-flag eboy-flags :C (= c 255))
           (eboy-set-flag eboy-flags :N t)
           (setq eboy-rA c)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA0 AND B"
         (setq eboy-rA (logand eboy-rA eboy-rB))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA1 AND C"
         (setq eboy-rA (logand eboy-rA eboy-rC))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA2 AND D"
         (setq eboy-rA (logand eboy-rA eboy-rD))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA3 AND E"
         (setq eboy-rA (logand eboy-rA eboy-rE))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA4 AND H"
         (setq eboy-rA (logand eboy-rA eboy-rH))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA5 AND L"
         (setq eboy-rA (logand eboy-rA eboy-rL))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA6 AND (HL)"
         (setq eboy-rA (logand eboy-rA (eboy-mem-read-byte (eboy-get-rHL))))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xA7 AND A"
         (setq eboy-rA (logand eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA8 XOR B "
         (setq eboy-rA (logxor eboy-rB eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA9 XOR C "
         (setq eboy-rA (logxor eboy-rC eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAA XOR D "
         (setq eboy-rA (logxor eboy-rD eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAB XOR E "
         (setq eboy-rA (logxor eboy-rE eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAC XOR H "
         (setq eboy-rA (logxor eboy-rH eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAD XOR L "
         (setq eboy-rA (logxor eboy-rL eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAE XOR (HL) "
         (setq eboy-rA (logxor eboy-rA (eboy-mem-read-byte (eboy-get-rHL))))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xAF XOR A "
         (setq eboy-rA (logxor eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB0 OR B"
         (setq eboy-rA (logior eboy-rB eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB1 OR C"
         (setq eboy-rA (logior eboy-rC eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB2 OR D"
         (setq eboy-rA (logior eboy-rD eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB3 OR E"
         (setq eboy-rA (logior eboy-rE eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB4 OR H"
         (setq eboy-rA (logior eboy-rH eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB5 OR L"
         (setq eboy-rA (logior eboy-rL eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB6 OR (HL)"
         (setq eboy-rA (logior (eboy-mem-read-byte (eboy-get-rHL)) eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xB7 OR A"
         (setq eboy-rA (logior eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB8 CP B "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rB))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rB) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rB))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB9 CP C "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rC))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rC) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rC))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBA CP D "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rD))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rD) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rD))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBB CP E "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rE))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rE) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rE))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBC CP H "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rH))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rH) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rH))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBD CP L "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rL))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rL) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rL))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBE CP (HL) "
         (let ((n (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :Z (= eboy-rA n))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA n) 15) (logand eboy-rA 15)))
           (eboy-set-flag eboy-flags :C (< eboy-rA n)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xBF CP A "
         (eboy-set-flag eboy-flags :Z (= eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :N t)
         (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA eboy-rA) 15) (logand eboy-rA 15)))
         (eboy-set-flag eboy-flags :C (< eboy-rA eboy-rA))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xC0 RET NZ "
         (if (null (eboy-get-flag eboy-flags :Z))
             (progn
               (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
               (incf eboy-sp 2)
               (incf eboy-clock-cycles 12)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xC1 POP BC"
         (eboy-set-rBC (eboy-mem-read-short eboy-sp))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xC2 JP NZ,nn  "
         (if (null (eboy-get-flag eboy-flags :Z))
             (progn
               (setq eboy-pc (1- (eboy-get-short)))
               (incf eboy-clock-cycles 4))
           (eboy-inc-pc 2))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xC3 JP $%04x"
         (setq eboy-pc (1- (eboy-get-short)))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xC4 CALL NZ,nn "
         (if (null (eboy-get-flag eboy-flags :Z))
             (progn
               (decf eboy-sp 2)
               (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
               (setq eboy-pc (- (eboy-get-short) 1))
               (incf eboy-clock-cycles 24))
           (incf eboy-pc 2)
           (incf eboy-clock-cycles 12)))
       (lambda nil "0xC5 PUSH BC"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rBC))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xC6 ADD A,#"
         (let ((rAnew (+ eboy-rA (eboy-get-byte))))
           (eboy-set-flag eboy-flags :Z (zerop rAnew))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand rAnew 15) (logand eboy-rA 15)))
           (eboy-set-flag eboy-flags :C (< (logand rAnew 255) (logand eboy-rA 255)))
           (eboy-add-byte rAnew 0) ; Convert to byte
           (setq eboy-rA rAnew))
         (incf eboy-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xC7 RST 00H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x00))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xC8 RET Z "
         (if (eboy-get-flag eboy-flags :Z)
             (progn
               (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
               (incf eboy-sp 2)
               (incf eboy-clock-cycles 12)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xC9 RET -/- (%0004x)"
         (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xCA JP Z,$%04x"
         (if (eboy-get-flag eboy-flags :Z)
             (progn
               (setq eboy-pc (1- (eboy-get-short)))
               (incf eboy-clock-cycles 4))
           (eboy-inc-pc 2))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xCB 2byte opcode CB:"
         (let ((cb-opcode (eboy-get-byte)))
           (eboy-inc-pc 1)
           (funcall (nth cb-opcode eboy-cpu-cb))))
       (lambda nil "0xCC CALL Z,nn "
         (if (eboy-get-flag eboy-flags :Z)
             (progn
               (decf eboy-sp 2)
               (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
               (setq eboy-pc (- (eboy-get-short) 1))
               (incf eboy-clock-cycles 24))
           (incf eboy-pc 2)
           (incf eboy-clock-cycles 12)))
       (lambda nil "0xCD CALL 0x%0004x "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
         (setq eboy-pc (1- (eboy-get-short)))
         (incf eboy-clock-cycles 24))
       (lambda nil "0xCE ADC A,#"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0))
               (b (eboy-get-byte)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand b #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA b c) #x100))
           (eboy-add-byte eboy-rA (+ b c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xCF RST 08H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x08))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD0 RET NC "
         (if (null (eboy-get-flag eboy-flags :C))
             (progn
               (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
               (incf eboy-sp 2)
               (incf eboy-clock-cycles 12)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xD1 POP DE"
         (eboy-set-rDE (eboy-mem-read-short eboy-sp))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xD2 JP NC,nn "
         (if (null (eboy-get-flag eboy-flags :C))
             (progn
               (setq eboy-pc (1- (eboy-get-short)))
               (incf eboy-clock-cycles 4))
           (eboy-inc-pc 2))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xD3"
         (assert nil t "0xD3 Non existant opcode"))
       (lambda nil "0xD4 CALL NC,nn "
         (if (null (eboy-get-flag eboy-flags :C))
             (progn
               (decf eboy-sp 2)
               (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
               (setq eboy-pc (- (eboy-get-short) 1))
               (incf eboy-clock-cycles 24))
           (incf eboy-pc 2)
           (incf eboy-clock-cycles 12)))
       (lambda nil "0xD5 PUSH DE"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rDE))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xD6 SUB #"
         (let ((newVal (- eboy-rA (eboy-get-byte))))
           (eboy-set-flag eboy-flags :Z (zerop newVal))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand newVal #x0F) (logand eboy-rA #x0F)))
           (eboy-set-flag eboy-flags :C (> (logand newVal #xFF) (logand eboy-rA #xFF)))
           (eboy-add-byte newVal 0) ; Convert to byte
           (setq eboy-rA newVal))
         (incf eboy-pc 1)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD7 RST 10H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x10))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD8 RET C "
         (if (eboy-get-flag eboy-flags :C)
             (progn
               (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
               (incf eboy-sp 2)
               (incf eboy-clock-cycles 12)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xD9 RETI -/- "
         (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
         (incf eboy-sp 2)
         (setq eboy-interrupt-master-enbl t)
         (incf eboy-clock-cycles 16)
         (setq eboy-delay-enabling-interrupt-p t))
       (lambda nil "0xDA JP C,nn "
         (if (eboy-get-flag eboy-flags :C)
             (progn
               (setq eboy-pc (1- (eboy-get-short)))
               (incf eboy-clock-cycles 4))
           (eboy-inc-pc 2))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xDB"
         (assert nil t "0xDB Non existant opcode"))
       (lambda nil "0xDC CALL C,nn "
         (if (eboy-get-flag eboy-flags :C)
             (progn
               (decf eboy-sp 2)
               (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
               (setq eboy-pc (- (eboy-get-short) 1))
               (incf eboy-clock-cycles 24))
           (incf eboy-pc 2)
           (incf eboy-clock-cycles 12)))
       (lambda nil "0xDD"
         (assert nil t "0xDD Non existant opcode"))
       (lambda nil "0xDE SBC A,#"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0))
               (oldrA eboy-rA))
           (eboy-add-byte eboy-rA (* -1 (+ (eboy-get-byte) c)))
           (eboy-set-flag eboy-flags :H (< (logand oldrA #x0F) (logand eboy-rA #x0F)))
           (eboy-set-flag eboy-flags :C (< (logand oldrA #xFF) (logand eboy-rA #xFF))))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (incf eboy-pc 1)
         (incf eboy-clock-cycles))
       (lambda nil "0xDF RST 18H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x18))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE0 LD ($FF00+),A "
         (eboy-mem-write-byte (+ 65280 (eboy-get-byte)) eboy-rA)
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xE1 POP HL"
         (eboy-set-rHL (eboy-mem-read-short eboy-sp))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xE2 LD ($FF00+C),A"
         (eboy-mem-write-byte (+ 65280 eboy-rC)
          eboy-rA)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xE3"
         (assert nil t "0xE3 Non existant opcode"))
       (lambda nil "0xE4"
         (assert nil t "0xE4 Non existant opcode"))
       (lambda nil "0xE5 PUSH HL"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rHL))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE6 AND # (#%02x) "
         (setq eboy-rA (logand eboy-rA (eboy-get-byte)))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xE7 RST 20H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x20))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE8 ADD SP,#  "
         (let ((old-sp eboy-sp))
           (setq eboy-sp (+ eboy-sp (eboy-byte-to-signed (eboy-get-byte))))
           (eboy-set-flag eboy-flags :Z nil)
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-sp #x0F) (logand old-sp #x0F)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-sp #xFF) (logand old-sp #xFF))))
         (incf eboy-pc 1)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE9 JP (HL) "
         (setq eboy-pc (1- (eboy-get-rHL)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xEA LD (0x%0004x),A"
         (eboy-mem-write-byte (eboy-get-short) eboy-rA)
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xEB"
         (assert nil t "0xEB Non existant opcode"))
       (lambda nil "0xEC"
         (assert nil t "0xEC Non existant opcode"))
       (lambda nil "0xED"
         (assert nil t "0xED Non existant opcode"))
       (lambda nil "0xEE XOR * "
         (setq eboy-rA (logxor (eboy-get-byte) eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xEF RST 28H "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x28))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF0 LD A,($FF00+)"
         (setq eboy-rA (eboy-mem-read-byte (+ #xFF00 (eboy-get-byte))))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF1 POP AF"
         (let ((data (eboy-mem-read-short eboy-sp)))
           (setq eboy-rA (lsh data -8))
           (setq eboy-flags (eboy-byte-to-flags (logand data 255))))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF2 LD A,($FF00+C)"
         (setq eboy-rA (eboy-mem-read-byte (+ #xFF00 eboy-rC)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xF3 DI -/- "
         (setq eboy-interrupt-master-enbl nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xF4"
         (assert nil t "0xF4 Non existant opcode"))
       (lambda nil "0xF5 PUSH AF"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rpair eboy-rA (eboy-flags-to-byte eboy-flags)))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF6 OR #"
         (setq eboy-rA (logior eboy-rA (eboy-get-byte)))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xF7 RST 30H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x30))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF8 LDHL SP,n"
         (let ((b (eboy-get-byte)))
           (eboy-set-rHL (+ eboy-sp (eboy-byte-to-signed b)))
           (eboy-add-byte b eboy-sp)
           (eboy-set-flag eboy-flags :Z nil)
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand b #xFF) (logand eboy-sp #xFF)))
           (eboy-set-flag eboy-flags :C (< (logand b #x0F) (logand eboy-sp #x0F))))
         (incf eboy-pc)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF9 LD SP,HL"
         (setq eboy-sp (eboy-get-rHL))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xFA LD A,(0x%02x)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-short)))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xFB EI -/- "
         (setq eboy-interrupt-master-enbl t)
         (incf eboy-clock-cycles 4)
         (setq eboy-delay-enabling-interrupt-p t))
       (lambda nil "0xFC"
         (assert nil t "0xFC Non existant opcode"))
       (lambda nil "0xFD"
         (assert nil t "0xFD Non existant opcode"))
       (lambda nil "0xFE CP # (0x%02x)"
         (let ((n (eboy-get-byte)))
           (eboy-set-flag eboy-flags :Z (= eboy-rA n))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA n) 15) (logand eboy-rA 15)))
           (eboy-set-flag eboy-flags :C (< eboy-rA n))
           (eboy-inc-pc 1))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xFF RST 38H  "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
         (setq eboy-pc (1- #x38))
         (incf eboy-clock-cycles 16))))


(setq eboy-cpu-cb
      (list
       (lambda nil "0x00 RLC B"
         (let ((b (lsh eboy-rB -7)))
           (setq eboy-rB (logior (lsh eboy-rB 1) b))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil)
           (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x01 RLC C"
          (let ((b (lsh eboy-rC -7)))
            (setq eboy-rC (logior (lsh eboy-rC 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x02 RLC D"
          (let ((b (lsh eboy-rD -7)))
            (setq eboy-rD (logior (lsh eboy-rD 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x03 RLC E"
          (let ((b (lsh eboy-rE -7)))
            (setq eboy-rE (logior (lsh eboy-rE 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x04 RLC H"
          (let ((b (lsh eboy-rH -7)))
            (setq eboy-rH (logior (lsh eboy-rH 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x05 RLC L"
          (let ((b (lsh eboy-rL -7)))
            (setq eboy-rL (logior (lsh eboy-rL 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x06 RLC (HL) "
          (let* ((hleboy-mem-read-byteb ( (eboy-get-rHL)))
                 (b (lsh hlb -7))
                 (hlbn (logior (lsh hlb 1) b)))
            (eboy-mem-write-byte (eboy-get-rHL) hlbn)
            (eboy-set-flag eboy-flags :Z (zerop hlbn))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x07 RLC A"
          (let ((b (lsh eboy-rA -7)))
            (setq eboy-rA (logior (lsh eboy-rA 1) b))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C (= b 1)))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x08 RRC B"
          (let ((b (logand eboy-rB #x01)))
            (setq eboy-rB (logior (lsh eboy-rB -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x09 RRC C"
          (let ((b (logand eboy-rC #x01)))
            (setq eboy-rC (logior (lsh eboy-rC -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0A RRC D"
          (let ((b (logand eboy-rD #x01)))
            (setq eboy-rD (logior (lsh eboy-rD -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0B RRC E"
          (let ((b (logand eboy-rE #x01)))
            (setq eboy-rE (logior (lsh eboy-rE -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0C RRC H"
          (let ((b (logand eboy-rH #x01)))
            (setq eboy-rH (logior (lsh eboy-rH -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0D RRC L"
          (let ((b (logand eboy-rL #x01)))
            (setq eboy-rL (logior (lsh eboy-rL -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0E RRC (HL) "
          (let* ((hlb (eboy-mem-read-byte (eboy-get-rHL)))
                 (b (logand hlb #x01))
                 (hlbn (logior (lsh hlb -1) (lsh b 7))))
            (eboy-mem-write-byte (eboy-get-rHL) hlbn)
            (eboy-set-flag eboy-flags :Z (zerop hlbn))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x0F RRC A"
          (let ((b (logand eboy-rA #x01)))
            (setq eboy-rA (logior (lsh eboy-rA -1) (lsh b 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C b))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x10 RL B"
          (let ((c (= (lsh eboy-rB -7) 1)))
            (setq eboy-rB (logior (lsh eboy-rB 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x11 RL C"
          (let ((c (= (lsh eboy-rC -7) 1)))
            (setq eboy-rC (logior (lsh eboy-rC 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x12 RL D"
          (let ((c (= (lsh eboy-rD -7) 1)))
            (setq eboy-rD (logior (lsh eboy-rD 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x13 RL E"
          (let ((c (= (lsh eboy-rE -7) 1)))
            (setq eboy-rE (logior (lsh eboy-rE 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x14 RL H"
          (let ((c (= (lsh eboy-rH -7) 1)))
            (setq eboy-rH (logior (lsh eboy-rH 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x15 RL L"
          (let ((c (= (lsh eboy-rL -7) 1)))
            (setq eboy-rL (logior (lsh eboy-rL 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x16 RL (HL) "
          (let* ((hl (eboy-mem-read-byte (eboy-get-rHL)))
                 (c (= (lsh hl -7) 1)))
            (eboy-mem-write-byte (eboy-get-rHL) (logior (lsh hl 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x17 RL A"
          (let ((c (= (lsh eboy-rA -7) 1)))
            (setq eboy-rA (logior (lsh eboy-rA 1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x18 RR B"
          (let ((c (= (logand eboy-rB #x01) #x01)))
            (setq eboy-rB (logior (lsh eboy-rB -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x19 RR C"
          (let ((c (= (logand eboy-rC #x01) #x01)))
            (setq eboy-rC (logior (lsh eboy-rC -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1A RR D"
          (let ((c (= (logand eboy-rD #x01) #x01)))
            (setq eboy-rD (logior (lsh eboy-rD -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1B RR E"
          (let ((c (= (logand eboy-rE #x01) #x01)))
            (setq eboy-rE (logior (lsh eboy-rE -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1C RR H"
          (let ((c (= (logand eboy-rH #x01) #x01)))
            (setq eboy-rH (logior (lsh eboy-rH -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1D RR L"
          (let ((c (= (logand eboy-rL #x01) #x01)))
            (setq eboy-rL (logior (lsh eboy-rL -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1E RR (HL) "
          (let* ((hl (eboy-mem-read-byte (eboy-get-rHL)))
                (c (= (logand hl #x01) #x01)))
            (eboy-mem-write-byte (eboy-get-rHL) (logior (lsh hl -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x1F RR A"
          (let ((c (= (logand eboy-rA #x01) #x01)))
            (setq eboy-rA (logior (lsh eboy-rA -1) (lsh (if (eboy-get-flag eboy-flags :C) 1 0) 7)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x20 SLA B"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rB -7) 1))
          (setq eboy-rB (lsh eboy-rB 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x21 SLA C"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rC -7) 1))
          (setq eboy-rC (lsh eboy-rC 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x22 SLA D"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rD -7) 1))
          (setq eboy-rD (lsh eboy-rD 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x23 SLA E"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rE -7) 1))
          (setq eboy-rE (lsh eboy-rE 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x24 SLA H"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rH -7) 1))
          (setq eboy-rH (lsh eboy-rH 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x25 SLA L"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rL -7) 1))
          (setq eboy-rL (lsh eboy-rL 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x26 SLA (HL) "
          (let ((hl (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :C (= (lsh hl -7) 1))
            (eboy-mem-write-byte (eboy-get-rHL) (lsh hl 1))
            (eboy-set-flag eboy-flags :Z (zerop (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x27 SLA A"
          (eboy-set-flag eboy-flags :C (= (lsh eboy-rA -7) 1))
          (setq eboy-rA (lsh eboy-rA 1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x28 SRA B"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rB #x01) 1))
          (setq eboy-rB (logior (lsh eboy-rB -1) (logand eboy-rB #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x29 SRA C"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rC #x01) 1))
          (setq eboy-rC (logior (lsh eboy-rC -1) (logand eboy-rC #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2A SRA D"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rD #x01) 1))
          (setq eboy-rD (logior (lsh eboy-rD -1) (logand eboy-rD #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2B SRA E"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rE #x01) 1))
          (setq eboy-rE (logior (lsh eboy-rE -1) (logand eboy-rE #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2C SRA H"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rH #x01) 1))
          (setq eboy-rH (logior (lsh eboy-rH -1) (logand eboy-rH #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2D SRA L"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rL #x01) 1))
          (setq eboy-rL (logior (lsh eboy-rL -1) (logand eboy-rL #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2E SRA (HL) "
          (let* ((hlb (eboy-mem-read-byte (eboy-get-rHL)))
                 (hlbn (logior (lsh hlb -1) (logand hlb #x80))))
            (eboy-set-flag eboy-flags :C (= (logand hlb #x01) 1))
            (eboy-mem-write-byte (eboy-get-rHL) hlb)
            (eboy-set-flag eboy-flags :Z (zerop hlb))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x2F SRA A"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rA #x01) 1))
          (setq eboy-rA (logior (lsh eboy-rA -1) (logand eboy-rA #x80)))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x30 SWAP B"
          (setq eboy-rB (logand (logior (lsh eboy-rB 4) (lsh eboy-rB -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x31 SWAP C"
          (setq eboy-rC (logand (logior (lsh eboy-rC 4) (lsh eboy-rC -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x32 SWAP D"
          (setq eboy-rD (logand (logior (lsh eboy-rD 4) (lsh eboy-rD -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x33 SWAP E"
          (setq eboy-rE (logand (logior (lsh eboy-rE 4) (lsh eboy-rE -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x34 SWAP H"
          (setq eboy-rH (logand (logior (lsh eboy-rH 4) (lsh eboy-rH -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x35 SWAP L"
          (setq eboy-rL (logand (logior (lsh eboy-rL 4) (lsh eboy-rL -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x36 SWAP (HL) "
          (let ((hl (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-mem-write-byte (eboy-get-rHL) (logand (logior (lsh hl 4) (lsh hl -4)) 255) )
            (eboy-set-flag eboy-flags :Z (zerop (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C nil))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x37 SWAP A"
          (setq eboy-rA (logand (logior (lsh eboy-rA 4) (lsh eboy-rA -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x38 SRL B"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rB #x01) #x01))
          (setq eboy-rB (lsh eboy-rB -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x39 SRL C"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rC #x01) #x01))
          (setq eboy-rC (lsh eboy-rC -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3A SRL D"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rD #x01) #x01))
          (setq eboy-rD (lsh eboy-rD -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3B SRL E"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rE #x01) #x01))
          (setq eboy-rE (lsh eboy-rE -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rE))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3C SRL H"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rH #x01) #x01))
          (setq eboy-rH (lsh eboy-rH -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rH))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3D SRL L"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rL #x01) #x01))
          (setq eboy-rL (lsh eboy-rL -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rL))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3E SRL (HL) "
          (let ((hl (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag eboy-flags :C (= (logand hl #x01) #x01))
            (setq hl (lsh hl -1))
            (eboy-set-flag eboy-flags :Z (zerop hl))
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :N nil))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x3F SRL A"
          (eboy-set-flag eboy-flags :C (= (logand eboy-rA #x01) #x01))
          (setq eboy-rA (lsh eboy-rA -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x40 BIT b,B"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x41 BIT b,C"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x42 BIT b,D"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x43 BIT b,E"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x44 BIT b,H"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x45 BIT b,L"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x46 BIT b,(HL) "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x47 BIT b,A"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x01) #x01))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x48 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x49 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x4A "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x4B "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x4C "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x4D "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x4E "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x4F "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x02) #x02))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x50 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x51 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x52 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x53 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x54 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x55 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x56 "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x57 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x04) #x04))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x58 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x59 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x5A "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x5B "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x5C "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x5D "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x5E "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x5F "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x08) #x08))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x60 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x61 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x62 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x63 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x64 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x65 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x66 "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x67 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x10) #x10))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x68 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x69 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x6A "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x6B "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x6C "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x6D "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x6E "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x6F "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x20) #x20))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x70 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x71 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x72 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x73 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x74 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x75 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x76 "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x77 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x40) #x40))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x78 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rB #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x79 "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rC #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7A "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rD #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7B "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rE #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7C BIT 7,H"
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rH #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7D "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rL #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7E "
          (eboy-set-flag eboy-flags :Z (/= (logand (eboy-get-rHL) #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 16))
        (lambda nil "0x7F "
          (eboy-set-flag eboy-flags :Z (/= (logand eboy-rA #x80) #x80))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x80 RES b,B"
          (setq eboy-rB (logand eboy-rB #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x81 RES b,C"
          (setq eboy-rC (logand eboy-rC #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x82 RES b,D"
          (setq eboy-rD (logand eboy-rD #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x83 RES b,E"
          (setq eboy-rE (logand eboy-rE #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x84 RES b,H"
          (setq eboy-rH (logand eboy-rH #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x85 RES b,L"
          (setq eboy-rL (logand eboy-rL #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x86 RES b,(HL) "
          (eboy-set-rHL (logand (eboy-get-rHL) #xFE))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x87 RES b,A"
          (setq eboy-rA (logand eboy-rA #xFE))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x88 "
          (setq eboy-rB (logand eboy-rB #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x89 "
          (setq eboy-rC (logand eboy-rC #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x8A "
          (setq eboy-rD (logand eboy-rD #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x8B "
          (setq eboy-rE (logand eboy-rE #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x8C "
          (setq eboy-rH (logand eboy-rH #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x8D "
          (setq eboy-rL (logand eboy-rL #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x8E "
          (eboy-set-rHL (logand (eboy-get-rHL) #xFD))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x8F "
          (setq eboy-rA (logand eboy-rA #xFD))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x90 "
          (setq eboy-rB (logand eboy-rB #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x91 "
          (setq eboy-rC(logand eboy-rC #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x92 "
          (setq eboy-rD (logand eboy-rD #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x93 "
          (setq eboy-rE (logand eboy-rE #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x94 "
          (setq eboy-rH (logand eboy-rH #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x95 "
          (setq eboy-rL (logand eboy-rL #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x96 "
          (eboy-set-rHL (logand (eboy-get-rHL) #xFB))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x97 "
          (setq eboy-rA (logand eboy-rA #xFB))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x98 "
          (setq eboy-rB (logand eboy-rB #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x99 "
          (setq eboy-rC (logand eboy-rC #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x9A "
          (setq eboy-rD (logand eboy-rD #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x9B "
          (setq eboy-rE (logand eboy-rE #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x9C "
          (setq eboy-rH (logand eboy-rH #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x9D "
          (setq eboy-rL (logand eboy-rL #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x9E "
          (eboy-set-rHL (logand (eboy-get-rHL) #xF7))
          (incf eboy-clock-cycles 16))
        (lambda nil "0x9F "
          (setq eboy-rA (logand eboy-rA #xF7))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA0 "
          (setq eboy-rB (logand eboy-rB #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA1 "
          (setq eboy-rC (logand eboy-rC #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA2 "
          (setq eboy-rD (logand eboy-rD #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA3 "
          (setq eboy-rE (logand eboy-rE #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA4 "
          (setq eboy-rH (logand eboy-rH #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA5 "
          (setq eboy-rL (logand eboy-rL #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA6 "
          (eboy-set-rHL (logand (eboy-get-rHL) #xEF))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xA7 "
          (setq eboy-rA (logand eboy-rA #xEF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA8 "
          (setq eboy-rB (logand eboy-rB #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xA9 "
          (setq eboy-rC (logand eboy-rC #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xAA "
          (setq eboy-rD (logand eboy-rD #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xAB "
          (setq eboy-rE (logand eboy-rE #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xAC "
          (setq eboy-rH (logand eboy-rH #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xAD "
          (setq eboy-rL (logand eboy-rL #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xAE "
          (eboy-set-rHL (logand (eboy-get-rHL) #xDF))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xAF "
          (setq eboy-rA (logand eboy-rA #xDF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB0 "
          (setq eboy-rB (logand eboy-rB #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB1 "
          (setq eboy-rC (logand eboy-rC #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB2 "
          (setq eboy-rD (logand eboy-rD #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB3 "
          (setq eboy-rE (logand eboy-rE #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB4 "
          (setq eboy-rH (logand eboy-rH #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB5 "
          (setq eboy-rL (logand eboy-rL #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB6 "
          (eboy-set-rHL (logand (eboy-get-rHL) #xBF))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xB7 "
          (setq eboy-rA (logand eboy-rA #xBF))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB8 "
          (setq eboy-rB (logand eboy-rB #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xB9 "
          (setq eboy-rC (logand eboy-rC #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xBA "
          (setq eboy-rD (logand eboy-rD #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xBB "
          (setq eboy-rE (logand eboy-rE #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xBC "
          (setq eboy-rH (logand eboy-rH #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xBD "
          (setq eboy-rL (logand eboy-rL #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xBE "
          (eboy-set-rHL (logand (eboy-get-rHL) #x7F))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xBF "
          (setq eboy-rA (logand eboy-rA #x7F))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC0 SET b,B"
          (setq eboy-rB (logior eboy-rB #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC1 SET b,C"
          (setq eboy-rC (logior eboy-rC #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC2 SET b,D"
          (setq eboy-rD (logior eboy-rD #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC3 SET b,E"
          (setq eboy-rE (logior eboy-rE #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC4 SET b,H"
          (setq eboy-rH (logior eboy-rH #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC5 SET b,L"
          (setq eboy-rL (logior eboy-rL #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC6 SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x01))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xC7 SET b,A"
          (setq eboy-rA (logior eboy-rA #x01))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC8 SET b,B"
          (setq eboy-rB (logior eboy-rB #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC9 SET b,C"
          (setq eboy-rC (logior eboy-rC #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xCA SET b,D"
          (setq eboy-rD (logior eboy-rD #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xCB SET b,E"
          (setq eboy-rE (logior eboy-rE #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xCC SET b,H"
          (setq eboy-rH (logior eboy-rH #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xCD SET b,L"
          (setq eboy-rL (logior eboy-rL #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xCE SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x02))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xCF SET b,A"
          (setq eboy-rA (logior eboy-rA #x02))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD0 SET b,B"
          (setq eboy-rB (logior eboy-rB #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD1 SET b,C"
          (setq eboy-rC (logior eboy-rC #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD2 SET b,D"
          (setq eboy-rD (logior eboy-rD #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD3 SET b,E"
          (setq eboy-rE (logior eboy-rE #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD4 SET b,H"
          (setq eboy-rH (logior eboy-rH #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD5 SET b,L"
          (setq eboy-rL (logior eboy-rL #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD6 SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x04))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xD7 SET b,A"
          (setq eboy-rA (logior eboy-rA #x04))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD8 SET b,B"
          (setq eboy-rB (logior eboy-rB #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xD9 SET b,C"
          (setq eboy-rC (logior eboy-rC #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xDA SET b,D"
          (setq eboy-rD (logior eboy-rD #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xDB SET b,E"
          (setq eboy-rE (logior eboy-rE #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xDC SET b,H"
          (setq eboy-rH (logior eboy-rH #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xDD SET b,L"
          (setq eboy-rL (logior eboy-rL #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xDE SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x08))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xDF SET b,A"
          (setq eboy-rA (logior eboy-rA #x08))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE0 SET b,B"
          (setq eboy-rB (logior eboy-rB #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE1 SET b,C"
          (setq eboy-rC (logior eboy-rC #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE2 SET b,D"
          (setq eboy-rD (logior eboy-rD #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE3 SET b,E"
          (setq eboy-rE (logior eboy-rE #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE4 SET b,H"
          (setq eboy-rH (logior eboy-rH #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE5 SET b,L"
          (setq eboy-rL (logior eboy-rL #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE6 SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x10))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xE7 SET b,A"
          (setq eboy-rA (logior eboy-rA #x10))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE8 SET b,B"
          (setq eboy-rB (logior eboy-rB #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xE9 SET b,C"
          (setq eboy-rC (logior eboy-rC #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xEA SET b,D"
          (setq eboy-rD (logior eboy-rD #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xEB SET b,E"
          (setq eboy-rE (logior eboy-rE #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xEC SET b,H"
          (setq eboy-rH (logior eboy-rH #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xED SET b,L"
          (setq eboy-rL (logior eboy-rL #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xEE SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x20))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xEF SET b,A"
          (setq eboy-rA (logior eboy-rA #x20))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF0 SET b,B"
          (setq eboy-rB (logior eboy-rB #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF1 SET b,C"
          (setq eboy-rC (logior eboy-rC #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF2 SET b,D"
          (setq eboy-rD (logior eboy-rD #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF3 SET b,E"
          (setq eboy-rE (logior eboy-rE #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF4 SET b,H"
          (setq eboy-rH (logior eboy-rH #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF5 SET b,L"
          (setq eboy-rL (logior eboy-rL #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF6 SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x40))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xF7 SET b,A"
          (setq eboy-rA (logior eboy-rA #x40))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF8 SET b,B"
          (setq eboy-rB (logior eboy-rB #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xF9 SET b,C"
          (setq eboy-rC (logior eboy-rC #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xFA SET b,D"
          (setq eboy-rD (logior eboy-rD #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xFB SET b,E"
          (setq eboy-rE (logior eboy-rE #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xFC SET b,H"
          (setq eboy-rH (logior eboy-rH #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xFD SET b,L"
          (setq eboy-rL (logior eboy-rL #x80))
          (incf eboy-clock-cycles 8))
        (lambda nil "0xFE SET b,(HL) "
          (eboy-set-rHL (logior (eboy-get-rHL) #x80))
          (incf eboy-clock-cycles 16))
        (lambda nil "0xFF SET b,A"
          (setq eboy-rA (logior eboy-rA #x80))
          (incf eboy-clock-cycles 8))
        )
      )

(provide 'eboy-cpu)
;;; eboy-cpu.el ends here

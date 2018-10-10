;;; eboy-cpu.el --- CPU instructions  -*- lexical-binding: t; -*-
;;; package -- Summary:
;;; Commentary:
;;; Code:
;;(require 'eboy)

(defvar eboy-cpu nil "All the cpu instruction functions.")
(defvar eboy-cpu-cb nil "All the CB opcode cpu instruction functions.")
(defvar eboy-cpu-halted nil "Bool to indicate if the CPU is halted")

(setq eboy-cpu
      (list
       (lambda nil "0x00 NOP"
         (incf eboy-clock-cycles 4))
       (lambda nil "0x01 LD BC, $%04x"
         (eboy-set-rBC (eboy-get-short))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0x02 LD (BC),A"
         (assert nil t "unimplemented opcode")
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
           (eboy-set-flag eboy-flags :C c)
           )
         (incf eboy-clock-cycles 4))
       (lambda nil "0x08 LD (nn),SP"
         (eboy-mem-write-short (eboy-mem-read-short (eboy-get-byte)) eboy-sp)
         (incf eboy-pc 2)
         (incf eboy-clock-cycles 20))
       (lambda nil "0x09 ADD HL,BC "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0x0A LD A,(BC)"
         (assert nil t "unimplemented opcode")
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
         (let ((c (= (logand eboy-rA 1) 1)))
           (setq eboy-rA (logand (logior (lsh eboy-rA -1) (lsh c 7)) #xFF))
           (eboy-set-flag eboy-flags :C c)
           (eboy-set-flag eboy-flags :Z (= eboy-rA 0))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H nil))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x10 STOP -/- 10 "
         (setq eboy-halted t)
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
         (assert nil t "unimplemented opcode")
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
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
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
           (eboy-set-flag eboy-flags :H (< (logand (eboy-get-rHL) #xfff) (logand hl #xfff)))
           (eboy-set-flag eboy-flags :C (< (logand (eboy-get-rHL) #xffff) (logand hl #xffff))))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2A LD A,(HLI)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
         (eboy-set-rHL (1+ (eboy-get-rHL)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x2B DEC HL "
         (assert nil t "unimplemented opcode")
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
         (assert nil t "unimplemented opcode")
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
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x38 JR C,* "
         (if (eboy-get-flag eboy-flags :C)
             (progn
               (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
               (incf eboy-clock-cycles 4)))
         (incf eboy-pc)
         (incf eboy-clock-cycles 8))
       (lambda nil "0x39 ADD HL,SP "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3A LD A,(HLD)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0x3B DEC SP "
         (assert nil t "unimplemented opcode")
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
         (assert nil t "unimplemented opcode")
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
         (setq eboy-halted t)
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
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rB c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x89 ADC A,C"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rC #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rC c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rC c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8A ADC A,D"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rD #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rD c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rD c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8B ADC A,E"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rE #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rE c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rE c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8C ADC A,H"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rH #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rH c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rH c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8D ADC A,L"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rL #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rL c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rL c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0x8E ADC A,(HL)"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0))
               (hl (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand hl #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA hl c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA hl c))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0x8F ADC A,A"
         (let ((c (if (eboy-get-flag eboy-flags :C) 1 0)))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (>= (+ (logand eboy-rA #xF) (logand eboy-rA #xF) c) #x10))
           (eboy-set-flag eboy-flags :C (>= (+ eboy-rA eboy-rA c) #x100))
           (eboy-add-byte eboy-rA (+ eboy-rA eboy-rA c))
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
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x92 SUB D"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x93 SUB E"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x94 SUB H"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x95 SUB L"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x96 SUB (HL)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0x97 SUB A"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x98 SBC A,B"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x99 SBC A,C"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9A SBC A,D"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9B SBC A,E"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9C SBC A,H"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9D SBC A,L"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0x9E SBC A,(HL)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0x9F SBC A,A"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA0 AND B"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA1 AND C"
         (setq eboy-rA (logand eboy-rA eboy-rC))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA2 AND D"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA3 AND E"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA4 AND H"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA5 AND L"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA6 AND (HL)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xA7 AND A"
         (setq eboy-rA (logand eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H t)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA8 XOR B "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xA9 XOR C "
         (setq eboy-rA (logxor eboy-rC eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAA XOR D "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAB XOR E "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAC XOR H "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xAD XOR L "
         (assert nil t "unimplemented opcode")
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
         (setq eboy-rA
               (logior eboy-rB eboy-rA))
         (eboy-set-flag eboy-flags :Z
                        (zerop eboy-rA))
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
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB3 OR E"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB4 OR H"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB5 OR L"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB6 OR (HL)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xB7 OR A"
         (setq eboy-rA (logior eboy-rA eboy-rA))
         (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
         (eboy-set-flag eboy-flags :N nil)
         (eboy-set-flag eboy-flags :H nil)
         (eboy-set-flag eboy-flags :C nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB8 CP B "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xB9 CP C "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBA CP D "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBB CP E "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBC CP H "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBD CP L "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xBE CP (HL) "
         (let ((n (eboy-mem-read-byte (eboy-get-rHL))))
           (eboy-set-flag eboy-flags :Z (= eboy-rA n))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA n) 15) (logand eboy-rA 15)))
           (eboy-set-flag eboy-flags :C (< eboy-rA n)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xBF CP A "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 4))
       (lambda nil "0xC0 RET NZ "
         (if (null (eboy-get-flag eboy-flags :Z))
             (progn
               (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
               (incf eboy-sp 2)
               (incf eboy-clock-cycles 12)))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xC1 POP BC"
         (eboy-set-rBC
          (eboy-mem-read-short eboy-sp))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xC2 JP NZ,nn  "
         (assert nil t "unimplemented opcode")
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
               (incf eboy-clock-cycles 24)
               )
           (incf eboy-pc 2)
           (incf eboy-clock-cycles 12)
           ))
       (lambda nil "0xC5 PUSH BC"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rBC))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xC6 ADD A,#"
         (let ((rAold eboy-rA))
           (eboy-add-byte eboy-rA (eboy-get-byte))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N nil)
           (eboy-set-flag eboy-flags :H (< (logand eboy-rA 15) (logand rAold 15)))
           (eboy-set-flag eboy-flags :C (< (logand eboy-rA 255) (logand rAold 255))))
         (incf eboy-pc 1)
         (incf eboy-clock-cycles 8))
       (lambda nil "0xC7 RST 00H  "
         (assert nil t "unimplemented opcode")
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
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xCD CALL 0x%0004x "
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
         (setq eboy-pc (1- (eboy-get-short)))
         (incf eboy-clock-cycles 24))
       (lambda nil "0xCE ADC A,#"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xCF RST 08H  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD0 RET NC "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xD1 POP DE"
         (eboy-set-rDE
          (eboy-mem-read-short eboy-sp))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xD2 JP NC,nn "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xD3 Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xD4 CALL NC,nn "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xD5 PUSH DE"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rDE))
         (incf eboy-clock-cycles 12))
       (lambda nil "0xD6 SUB #"
         (let ((oldval eboy-rA))
           (eboy-add-byte eboy-rA (* -1 (eboy-get-byte)))
           (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand eboy-rA #x0F) (logand oldval #x0F)))
           (eboy-set-flag eboy-flags :C (> (logand eboy-rA #xFF) (logand oldval #xFF)))
           )
         (incf eboy-pc 1)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD7 RST 10H  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xD8 RET C "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xD9 RETI -/- "
         (setq eboy-pc (1- (eboy-mem-read-short eboy-sp)))
         (incf eboy-sp 2)
         (setq eboy-interrupt-master-enbl t)
         (incf eboy-clock-cycles 16)
         (setq eboy-delay-enabling-interrupt-p t))
       (lambda nil "0xDA JP C,nn "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xDB Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xDC CALL C,nn "
         (assert nil t "unimplemented opcode"))
       (lambda nil "0xDD Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xDE SBC A,#"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles))
       (lambda nil "0xDF RST 18H  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE0 LD ($FF00+),A "
         (eboy-mem-write-byte (+ 65280 (eboy-get-byte))
          eboy-rA)
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
       (lambda nil "0xE3 Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xE4 Non existant opcode: 0x%02x"
         (assert nil t))
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
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE8 ADD SP,#  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xE9 JP (HL) "
         (setq eboy-pc (1- (eboy-get-rHL)))
         (incf eboy-clock-cycles 4))
       (lambda nil "0xEA LD (0x%0004x),A"
         (eboy-mem-write-byte (eboy-get-short) eboy-rA)
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xEB Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xEC Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xED Non existant opcode: 0x%02x"
         (assert nil t))
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
         (setq eboy-pc (1- 40))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF0 LD A,($FF00+)"
         (setq eboy-rA (eboy-mem-read-byte (+ 65280 (eboy-get-byte))))
         (eboy-inc-pc 1)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF1 POP AF"
         (let ((data (eboy-mem-read-short eboy-sp)))
           (setq eboy-rA (lsh data -8))
           (setq eboy-flags (eboy-byte-to-flags (logand data 255))))
         (incf eboy-sp 2)
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF2 LD A,($FF00+C)"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xF3 DI -/- "
         (setq eboy-interrupt-master-enbl nil)
         (incf eboy-clock-cycles 4))
       (lambda nil "0xF4 Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xF5 PUSH AF"
         (decf eboy-sp 2)
         (eboy-mem-write-short eboy-sp (eboy-get-rpair eboy-rA (eboy-flags-to-byte eboy-flags)))
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF6 OR #"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xF7 RST 30H  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       (lambda nil "0xF8 LDHL SP,n"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 12))
       (lambda nil "0xF9 LD SP,HL"
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 8))
       (lambda nil "0xFA LD A,(0x%02x)"
         (setq eboy-rA (eboy-mem-read-byte (eboy-get-short)))
         (eboy-inc-pc 2)
         (incf eboy-clock-cycles 16))
       (lambda nil "0xFB EI -/- "
         (setq eboy-interrupt-master-enbl t)
         (incf eboy-clock-cycles 4)
         (setq eboy-delay-enabling-interrupt-p t))
       (lambda nil "0xFC Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xFD Non existant opcode: 0x%02x"
         (assert nil t))
       (lambda nil "0xFE CP # (0x%02x)"
         (let ((n (eboy-get-byte)))
           (eboy-set-flag eboy-flags :Z (= eboy-rA n))
           (eboy-set-flag eboy-flags :N t)
           (eboy-set-flag eboy-flags :H (> (logand (- eboy-rA n) 15) (logand eboy-rA 15)))
           (eboy-set-flag eboy-flags :C (< eboy-rA n))
           (eboy-inc-pc 1))
         (incf eboy-clock-cycles 8))
       (lambda nil "0xFF RST 38H  "
         (assert nil t "unimplemented opcode")
         (incf eboy-clock-cycles 16))
       ))



(setq eboy-cpu-cb
      (list
       (lambda nil "0x00 RLC B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x01 RLC C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x02 RLC D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x03 RLC E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x04 RLC H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x05 RLC L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x06 RLC (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x07 RLC A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x08 RRC B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x09 RRC C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0A RRC D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0B RRC E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0C RRC H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0D RRC L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x0E RRC (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x0F RRC A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x10 RL B"
          (assert nil t "unimplemented opcode")
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
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x13 RL E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x14 RL H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x15 RL L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x16 RL (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x17 RL A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x18 RR B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x19 RR C"
          (let ((c (logand eboy-rC #x01)))
            (setq eboy-rC (logior (lsh eboy-rC -1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rC))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1A RR D"
          (let ((c (logand eboy-rD #x01)))
            (setq eboy-rD (logior (lsh eboy-rD -1) (if (eboy-get-flag eboy-flags :C) 1 0)))
            (eboy-set-flag eboy-flags :Z (zerop eboy-rD))
            (eboy-set-flag eboy-flags :N nil)
            (eboy-set-flag eboy-flags :H nil)
            (eboy-set-flag eboy-flags :C c))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1B RR E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1C RR H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1D RR L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x1E RR (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x1F RR A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x20 SLA B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x21 SLA C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x22 SLA D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x23 SLA E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x24 SLA H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x25 SLA L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x26 SLA (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x27 SLA A"
          (eboy-set-flag eboy-flags :C (lsh eboy-rA -7))
          (setq eboy-rA (lsh eboy-rA -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x28 SRA B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x29 SRA C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2A SRA D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2B SRA E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2C SRA H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2D SRA L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x2E SRA (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x2F SRA A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x30 SWAP B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x31 SWAP C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x32 SWAP D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x33 SWAP E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x34 SWAP H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x35 SWAP L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x36 SWAP (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x37 SWAP A"
          (setq eboy-rA (logand (logior (lsh eboy-rA 4) (lsh eboy-rA -4)) 255))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rA))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :C nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x38 SRL B"
          (eboy-set-flag eboy-flags :C (logand eboy-rB #x01))
          (setq eboy-rB (lsh eboy-rB -1))
          (eboy-set-flag eboy-flags :Z (zerop eboy-rB))
          (eboy-set-flag eboy-flags :H nil)
          (eboy-set-flag eboy-flags :N nil)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x39 SRL C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3A SRL D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3B SRL E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3C SRL H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3D SRL L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x3E SRL (HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x3F SRL A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x40 BIT b,B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x41 BIT b,C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x42 BIT b,D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x43 BIT b,E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x44 BIT b,H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x45 BIT b,L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x46 BIT b,(HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x47 BIT b,A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x48 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x49 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4C "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x4F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x50 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x51 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x52 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x53 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x54 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x55 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x56 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x57 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x58 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x59 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5C "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x5F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x60 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x61 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x62 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x63 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x64 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x65 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x66 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x67 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x68 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x69 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6C "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x6F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x70 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x71 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x72 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x73 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x74 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x75 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x76 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x77 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x78 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x79 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x7A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x7B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x7C BIT 7,H"
          (eboy-set-flag eboy-flags :Z (not (= (logand eboy-rH 128) 128)))
          (eboy-set-flag eboy-flags :N nil)
          (eboy-set-flag eboy-flags :H t)
          (incf eboy-clock-cycles 8))
        (lambda nil "0x7D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x7E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x7F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x80 RES b,B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x81 RES b,C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x82 RES b,D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x83 RES b,E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x84 RES b,H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x85 RES b,L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0x86 RES b,(HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0x87 RES b,A"
          (setq eboy-rA (logand eboy-rA 254))
          (incf eboy-clock-cycles 8))
        (lambda nil "0x88 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x89 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8C "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x8F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x90 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x91 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x92 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x93 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x94 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x95 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x96 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x97 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x98 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x99 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9A "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9B "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9C "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9D "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9E "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0x9F "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA0 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA1 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA2 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA3 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA4 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA5 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA6 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA7 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA8 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xA9 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAA "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAB "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAC "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAD "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAE "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xAF "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB0 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB1 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB2 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB3 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB4 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB5 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB6 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB7 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB8 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xB9 "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBA "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBB "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBC "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBD "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBE "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xBF "
          (assert nil t "Unimplemented BC opcode"))
        (lambda nil "0xC0 SET b,B"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC1 SET b,C"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC2 SET b,D"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC3 SET b,E"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC4 SET b,H"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC5 SET b,L"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8))
        (lambda nil "0xC6 SET b,(HL) "
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 16))
        (lambda nil "0xC7 SET b,A"
          (assert nil t "unimplemented opcode")
          (incf eboy-clock-cycles 8)))
      )

(provide 'eboy-cpu)
;;; eboy-cpu.el ends here

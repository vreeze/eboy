;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;;; Commentary:
;; (defcont foo #b00001)
;; (setf *flag* (logior *flag* #b00001))
;;
;; (defun foobar (keyword)
;;   "docstring."
;;   (ecase keyword
;;     (:foo #b00001)
;;     (:bar #b00010)
;;     (:baz #b00100))
;;   )
;; bool-vector?
(require 'cl)

;;; Code:
(defun eboy-read-bytes (path)
  "Read binary data from PATH.
Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defconst eboy-pc-start-address #x100 "The start address of the program counter.")

(defvar eboy-rom-filename nil "The file name of the loaded rom.")
(defvar eboy-rom nil "The binary vector of the rom.")
(defvar eboy-rom-size nil "The size of the rom in bytes.")
(defvar eboy-pc nil "The program counter.")
(defvar eboy-flags nil "The flags of the CPU.")
(defvar eboy-debugging-nr-instructions nil "Number of instructions executed.")

(defun eboy-reset-CPU-flags ()
  "Reset the CPU flags."
  (setq eboy-flags '((Z . nil)
                     (N . nil)
                     (H . nil)
                     (C . nil)))
  )

(defun eboy-flag-Z-set-p ()
  "Is the flag Z set?."
  (cdr (assoc 'Z eboy-flags))
  )
(defun eboy-set-Z-flag ()
  "Set flag Z."
  )

(defun eboy-rom-title ()
  "Retrieve the title of the game from the loaded rom."
  (let ((title ""))
    (dotimes (i 16)
      (let ((c (aref eboy-rom (+ i #x134))))
        (unless (equal c 0)
            (setq title (concat title (format "%c" c))))
        ))
    title))

(defun eboy-get-short ()
  "Skip opcode and get next two bytes."
  (logior (lsh (aref eboy-rom (+ eboy-pc 2)) 8) (aref eboy-rom (+ eboy-pc 1)))
  )

(defun eboy-get-byte ()
  "Skip opcode and get next byte."
  (aref eboy-rom (+ eboy-pc 1))
  )

(defun eboy-inc-pc (nr-bytes)
  "Increment program counter with NR-BYTES."
  (setq eboy-pc (+ eboy-pc nr-bytes))
  )

(defun eboy-debug-unimplemented-opcode (opcode)
  "Print OPCODE is unimplemented."
  (insert (format "Unimplemented opcode 0x%02x" opcode))
  )

(defun eboy-process-opcode (opcode)
  "Process OPCODE."
  (insert (format "pc: 0x%x  " eboy-pc))
    (cl-case opcode
      (#x00 (insert "NOP\n"))

      ;; LD nn,n
      (#x06
       ;;(eboy-debug-unimplemented-opcode 6)
       (insert (format "LD B, #0x%02x\n" (eboy-get-byte)))
       (insert )
       (eboy-inc-pc 1))
      (#x0E (insert (format "LD C, #0x%02x\n" (eboy-get-byte)))
           (eboy-inc-pc 1))
      (#x16 (insert (format "LD D, #0x%02x\n" (eboy-get-byte)))
            (eboy-inc-pc 1))
      (#x1E (insert (format "LD E, #0x%02x\n" (eboy-get-byte)))
            (eboy-inc-pc 1))
      (#x26 (insert (format "LD H, #0x%02x\n" (eboy-get-byte)))
            (eboy-inc-pc 1))
      (#x2E (insert (format "LD L, #0x%02x\n" (eboy-get-byte)))
            (eboy-inc-pc 1))

      ;; ;; LD r1,r2
      (#x7F (insert (format "LD A,A\n")))
      (#x78 (insert (format "LD A,B\n")))
      (#x79 (insert (format "LD A,C\n")))
      (#x7A (insert (format "LD A,D\n")))
      (#x7B (insert (format "LD A,E\n")))
      (#x7C (insert (format "LD A,H\n")))
      (#x7D (insert (format "LD A,L\n")))
      (#x7E (insert (format "LD A,(HL)\n")))
      (#x40 (insert (format "LD B,B\n")))
      (#x41 (insert (format "LD B,C\n")))
      (#x42 (insert (format "LD B,D\n")))
      (#x43 (insert (format "LD B,E\n")))
      (#x44 (insert (format "LD B,H\n")))
      (#x45 (insert (format "LD B,L\n")))
      (#x46 (insert (format "LD B,(HL)\n")))
      (#x48 (insert (format "LD C,B\n")))
      (#x49 (insert (format "LD C,C\n")))
      (#x4A (insert (format "LD C,D\n")))
      (#x4B (insert (format "LD C,E\n")))
      (#x4C (insert (format "LD C,H\n")))
      (#x4D (insert (format "LD C,L\n")))
      (#x4E (insert (format "LD C,(HL)\n")))
      (#x50 (insert (format "LD D,B\n")))
      (#x51 (insert (format "LD D,C\n")))
      (#x52 (insert (format "LD D,D\n")))
      (#x53 (insert (format "LD D,E\n")))
      (#x54 (insert (format "LD D,H\n")))
      (#x55 (insert (format "LD D,L\n")))
      (#x56 (insert (format "LD D,(HL)\n")))
      (#x58 (insert (format "LD E,B\n")))
      (#x59 (insert (format "LD E,C\n")))
      (#x5A (insert (format "LD E,D\n")))
      (#x5B (insert (format "LD E,E\n")))
      (#x5C (insert (format "LD E,H\n")))
      (#x5D (insert (format "LD E,L\n")))
      (#x5E (insert (format "LD E,(HL)\n")))
      (#x60 (insert (format "LD H,B\n")))
      (#x61 (insert (format "LD H,C\n")))
      (#x62 (insert (format "LD H,D\n")))
      (#x63 (insert (format "LD H,E\n")))
      (#x64 (insert (format "LD H,H\n")))
      (#x65 (insert (format "LD H,L\n")))
      (#x66 (insert (format "LD H,(HL)\n")))
      (#x68 (insert (format "LD L,B\n")))
      (#x69 (insert (format "LD L,C\n")))
      (#x6A (insert (format "LD L,D\n")))
      (#x6B (insert (format "LD L,E\n")))
      (#x6C (insert (format "LD L,H\n")))
      (#x6D (insert (format "LD L,L\n")))
      (#x6E (insert (format "LD L,(HL)\n")))
      (#x70 (insert (format "LD (HL),B\n")))
      (#x71 (insert (format "LD (HL),C\n")))
      (#x72 (insert (format "LD (HL),D\n")))
      (#x73 (insert (format "LD (HL),E\n")))
      (#x74 (insert (format "LD (HL),H\n")))
      (#x75 (insert (format "LD (HL),L\n")))
      (#x36 (insert (format "LD (HL),n\n")))

      ;; LD A,n
      (#x0A (insert (format "LD A,(BC)\n")))
      (#x1A (insert (format "LD A,(DE)\n")))
      (#xFA (insert (format "LD A,(nn)\n")))
      (#x3E (insert (format "LD A,#0x%02x\n" (eboy-get-byte))) (eboy-inc-pc 1))

      ;; LD n,A - Put value A into n.
      ;; n = A,B,C,D,E,H,L,(BC),(DE),(HL),(nn)
      ;; nn = two byte immediate value. (LS byte first.)
      (#x47 (insert (format "LD B,A")))                      ;; 4
      (#x4F (insert (format "LD C,A")))                      ;; 4
      (#x57 (insert (format "LD D,A")))                      ;; 4
      (#x5F (insert (format "LD E,A")))                      ;; 4
      (#x67 (insert (format "LD H,A")))                      ;; 4
      (#x6F (insert (format "LD L,A")))                      ;; 4
      (#x02 (insert (format "LD (BC),A\n")))                   ;; 8
      (#x12 (insert (format "LD (DE),A\n")))                   ;; 8
      (#x77 (insert (format "LD (HL),A\n")))                   ;; 8
      (#xEA (insert (format "LD (nn),A\n")))                   ;; 16

      ;; Put value at address $FF00 + register C into A.
      (#xF2 (insert (format "LD A,($FF00+C)")))
      ;; Put A into address $FF00 + register C.
      (#xE2 (insert (format "LD ($FF00+C),A\n")))
      ;; Put value at address HL into A. Decrement HL.
      (#x3A (insert (format "LD A,(HLD)\n")))
      ;; Put A into memory address HL. Decrement HL.
      (#x32 (insert (format "LD (HLD),A\n")))
      ;; Put value at address HL into A. Increment HL.
      (#x2A (insert (format "LD A,(HLI)\n")))
      ;; Put A into memory address HL. Increment HL.
      (#x22 (insert (format "LD (HLI),A\n")))
      ;; Put A into memory address $FF00+n
      (#xE0 (insert (format "LD ($FF00+n),A\n")))
      ;; Put memory address $FF00+n into A.
      (#xF0 (insert (format "LD A,($FF00+n)\n")))

      ;; 16 bit loads, nn = 16 bit immediate value
      (#x01 (insert (format "LD BC, $%04x\n" (eboy-get-short)))(eboy-inc-pc 2))
      (#x11 (insert (format "LD DE,nn\n")))
      (#x21 (insert (format "LD HL,nn\n")))
      (#x31 (insert (format "LD SP,nn\n")))

      (#xF9 (insert (format "LD SP,HL\n")))

      ;; Put SP + n effective address into HL, n = one byte signed immediate value.
      ;; Flags affected:
      ;; Z - Reset.
      ;; N - Reset.
      ;; H - Set or reset according to operation.
      ;; C - Set or reset according to operation.
      (#xF8 (insert (format "LDHL SP,n\n")))
      ;; Put Stack Pointer (SP) at address n. n = two byte immediate address
      (#x08 (insert (format "LD (nn),SP\n")))

      ;; Push register pair nn onto stack.
      ;; Decrement Stack Pointer (SP) twice.
      (#xF5 (insert (format "PUSH AF\n")))                      ;; 16
      (#xC5 (insert (format "PUSH BC\n")))                      ;; 16
      (#xD5 (insert (format "PUSH DE\n")))                      ;; 16
      (#xE5 (insert (format "PUSH HL\n")))                      ;; 16

      ;; Pop two bytes off stack into register pair nn.
      ;; Increment Stack Pointer (SP) twice.
      (#xF1 (insert (format "POP AF\n")))                      ;; 12
      (#xC1 (insert (format "POP BC\n")))                      ;; 12
      (#xD1 (insert (format "POP DE\n")))                      ;; 12
      (#xE1 (insert (format "POP HL\n")))                      ;; 12


      ;; ADD A,n    -    Add n to A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Set if carry from bit 3.
      ;; C - Set if carry from bit 7.
      (#x87 (insert (format "ADD A,A\n")))                      ;; 4
      (#x80 (insert (format "ADD A,B\n")))                      ;; 4
      (#x81 (insert (format "ADD A,C\n")))                      ;; 4
      (#x82 (insert (format "ADD A,D\n")))                      ;; 4
      (#x83 (insert (format "ADD A,E\n")))                      ;; 4
      (#x84 (insert (format "ADD A,H\n")))                      ;; 4
      (#x85 (insert (format "ADD A,L\n")))                      ;; 4
      (#x86 (insert (format "ADD A,(HL)\n")))                   ;; 8
      (#xC6 (insert (format "ADD A,#\n")))                      ;; 8

      ;;ADC A,n    -   Add n + Carry flag to A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Set if carry from bit 3.
      ;; C - Set if carry from bit 7.
      (#x8F (insert (format "ADC A,A\n")))                      ;; 4
      (#x88 (insert (format "ADC A,B\n")))                      ;; 4
      (#x89 (insert (format "ADC A,C\n")))                      ;; 4
      (#x8A (insert (format "ADC A,D\n")))                      ;; 4
      (#x8B (insert (format "ADC A,E\n")))                      ;; 4
      (#x8C (insert (format "ADC A,H\n")))                      ;; 4
      (#x8D (insert (format "ADC A,L\n")))                      ;; 4
      (#x8E (insert (format "ADC A,(HL)\n")))                   ;; 8
      (#xCE (insert (format "ADC A,#\n")))                      ;; 8

      ;; SUB n - Subtract n from A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Set.
      ;; H - Set if no borrow from bit 4.
      ;; C - Set if no borrow.
      (#x97 (insert (format "SUB A\n")))                        ;; 4
      (#x90 (insert (format "SUB B\n")))                        ;; 4
      (#x91 (insert (format "SUB C\n")))                        ;; 4
      (#x92 (insert (format "SUB D\n")))                        ;; 4
      (#x93 (insert (format "SUB E\n")))                        ;; 4
      (#x94 (insert (format "SUB H\n")))                        ;; 4
      (#x95 (insert (format "SUB L\n")))                        ;; 4
      (#x96 (insert (format "SUB (HL)\n")))                     ;; 8
      (#xD6 (insert (format "SUB #\n")))                        ;; 8

      ;; SBC A,n - Subtract n + Carry flag from A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Set.
      ;; H - Set if no borrow from bit 4.
      ;; C - Set if no borrow.
      (#x9F (insert (format "SBC A,A\n")))                      ;; 4
      (#x98 (insert (format "SBC A,B\n")))                      ;; 4
      (#x99 (insert (format "SBC A,C\n")))                      ;; 4
      (#x9A (insert (format "SBC A,D\n")))                      ;; 4
      (#x9B (insert (format "SBC A,E\n")))                      ;; 4
      (#x9C (insert (format "SBC A,H\n")))                      ;; 4
      (#x9D (insert (format "SBC A,L\n")))                      ;; 4
      (#x9E (insert (format "SBC A,(HL)\n")))                   ;; 8
      ;;(?? (insert (format "SBC A,#\n")))                      ;; ?

      ;; AND n - Logically AND n with A, result in A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Set.
      ;; C - Reset.
      (#xA7 (insert (format "AND A\n")))                        ;; 4
      (#xA0 (insert (format "AND B\n")))                        ;; 4
      (#xA1 (insert (format "AND C\n")))                        ;; 4
      (#xA2 (insert (format "AND D\n")))                        ;; 4
      (#xA3 (insert (format "AND E\n")))                        ;; 4
      (#xA4 (insert (format "AND H\n")))                        ;; 4
      (#xA5 (insert (format "AND L\n")))                        ;; 4
      (#xA6 (insert (format "AND (HL)\n")))                     ;; 8
      (#xE6 (insert (format "AND #\n")))                        ;; 8

      ;; OR n - Logical OR n with register A, result in A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Reset.
      (#xB7 (insert (format "OR A\n")))                        ;; 4
      (#xB0 (insert (format "OR B\n")))                        ;; 4
      (#xB1 (insert (format "OR C\n")))                        ;; 4
      (#xB2 (insert (format "OR D\n")))                        ;; 4
      (#xB3 (insert (format "OR E\n")))                        ;; 4
      (#xB4 (insert (format "OR H\n")))                        ;; 4
      (#xB5 (insert (format "OR L\n")))                        ;; 4
      (#xB6 (insert (format "OR (HL)\n")))                     ;; 8
      (#xF6 (insert (format "OR #\n")))                        ;; 8

      ;; XOR n - Logical exclusive OR n with register A, result in A.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Reset.
      (#xAF (insert (format "XOR A \n"))) ;; 4
      (#xA8 (insert (format "XOR B \n"))) ;; 4
      (#xA9 (insert (format "XOR C \n"))) ;; 4
      (#xAA (insert (format "XOR D \n"))) ;; 4
      (#xAB (insert (format "XOR E \n"))) ;; 4
      (#xAC (insert (format "XOR H \n"))) ;; 4
      (#xAD (insert (format "XOR L \n"))) ;; 4
      (#xAE (insert (format "XOR (HL) \n"))) ;; 8
      (#xEE (insert (format "XOR * \n"))) ;; 8

      ;; CP n - Compare A with n. This is basically an A - n subtraction instruction but the results are thrown away.
      ;; Flags affected:
      ;; Z - Set if result is zero. (Set if A = n.)
      ;; N - Set.
      ;; H - Set if no borrow from bit 4.
      ;; C - Set for no borrow. (Set if A < n.)
      (#xBF (insert (format "CP A \n"))) ;; 4
      (#xB8 (insert (format "CP B \n"))) ;; 4
      (#xB9 (insert (format "CP C \n"))) ;; 4
      (#xBA (insert (format "CP D \n"))) ;; 4
      (#xBB (insert (format "CP E \n"))) ;; 4
      (#xBC (insert (format "CP H \n"))) ;; 4
      (#xBD (insert (format "CP L \n"))) ;; 4
      (#xBE (insert (format "CP (HL) \n"))) ;; 8
      (#xFE (insert (format "CP # \n"))) ;; 8

      ;; INC n - Increment register n.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Set if carry from bit 3.
      ;; C - Not affected.
      (#x3C (insert (format "INC A \n"))) ;; 4
      (#x04 (insert (format "INC B \n"))) ;; 4
      (#x0C (insert (format "INC C \n"))) ;; 4
      (#x14 (insert (format "INC D \n"))) ;; 4
      (#x1C (insert (format "INC E \n"))) ;; 4
      (#x24 (insert (format "INC H \n"))) ;; 4
      (#x2C (insert (format "INC L \n"))) ;; 4
      (#x34 (insert (format "INC (HL)  \n")));; 12

      ;; DEC n - Decrement register n.
      ;; Flags affected:
      ;; Z - Set if reselt is zero.
      ;; N - Set.
      ;; H - Set if no borrow from bit 4.
      ;; C - Not affected.
      (#x3D (insert (format "DEC A \n"))) ;; 4
      (#x05 (insert (format "DEC B \n"))) ;; 4
      (#x0D (insert (format "DEC C \n"))) ;; 4
      (#x15 (insert (format "DEC D \n"))) ;; 4
      (#x1D (insert (format "DEC E \n"))) ;; 4
      (#x25 (insert (format "DEC H \n"))) ;; 4
      (#x2D (insert (format "DEC L \n"))) ;; 4
      (#x35 (insert (format "DEC (HL)  \n")));; 12

      ;;; 16-Bit Arithmetic
      ;;
      ;; ADD HL,n - Add n to HL.
      ;; Flags affected:
      ;; Z - Not affected.
      ;; N - Reset.
      ;; H - Set if carry from bit 11.
      ;; C - Set if carry from bit 15.
      (#x09 (insert (format "ADD HL,BC \n"))) ;; 8
      (#x19 (insert (format "ADD HL,DE \n"))) ;; 8
      (#x29 (insert (format "ADD HL,HL \n"))) ;; 8
      (#x39 (insert (format "ADD HL,SP \n"))) ;; 8

      ;; ADD SP,n - Add n to Stack Pointer (SP).
      ;; Flags affected:
      ;; None.
      ;; Z - Reset.
      ;; N - Reset.
      ;; H - Set or reset according to operation.
      ;; C - Set or reset according to operation.
      (#xE8 (insert (format "ADD SP,#  \n")));; 16

      ;; INC nn - Increment register nn.
      ;; Flags affected:
      (#x03 (insert (format "INC BC \n"))) ;; 8
      (#x13 (insert (format "INC DE \n"))) ;; 8
      (#x23 (insert (format "INC HL \n"))) ;; 8
      (#x33 (insert (format "INC SP \n"))) ;; 8

      ;; DEC nn - Decrement register nn.
      ;; Flags affected:
      ;; None.
      (#x0B (insert (format "DEC BC \n"))) ;; 8
      (#x1B (insert (format "DEC DE \n"))) ;; 8
      (#x2B (insert (format "DEC HL \n"))) ;; 8
      (#x3B (insert (format "DEC SP \n"))) ;; 8

      ;;; Miscellaneous



      ;; DAA - Decimal adjust register A.
      ;; This instruction adjusts register A so that the correct representation of Binary Coded Decimal (BCD) is obtained.
      ;; Flags affected:
      ;; Z - Set if register A is zero.
      ;; N - Not affected.
      ;; H - Reset.
      ;; C - Set or reset according to operation.
      (#x27 (insert (format "DAA -/- \n"))) ;; 4

      ;; CPL - Complement A register. (Flip all bits.)
      ;; Flags affected:
      ;; Z - Not affected.
      ;; N - Set.
      ;; H - Set.
      ;; C - Not affected.
      (#x2F (insert (format "CPL -/- \n"))) ;; 4

      ;; CCF - Complement carry flag.
      ;; If C flag is set, then reset it.
      ;; If C flag is reset, then set it.
      ;; Flags affected:
      ;; Z - Not affected.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Complemented.
      (#x3F (insert (format "CCF -/- \n"))) ;; 4

      ;; SCF - Set Carry flag.
      ;; Flags affected:
      ;; Z - Not affected.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Set.
      (#x37 (insert (format "SCF -/- \n"))) ;; 4

      ;; HALT - Power down CPU until an interrupt occurs. Use this when ever possible to reduce energy consumption.
      (#x76 (insert (format "HALT -/- \n"))) ;; 4

      ;; STOP - Halt CPU & LCD display until button pressed.
      (#x10 (insert (format "STOP -/- 10 \n"))) ;; 4
      ;; This is a 2 byte opcode: 0x10 00

      ;; DI - This instruction disables interrupts but not immediately. Interrupts are disabled after instruction after DI is executed.
      ;; Flags affected:
      ;; None.
      (#xF3 (insert (format "DI -/- \n"))) ;; 4
      ;; EI - Enable interrupts. This intruction enables interrupts but not immediately. Interrupts are enabled after instruction after EI is executed.
      ;; Flags affected:
      ;; None.
      (#xFB (insert (format "EI -/- \n"))) ;; 4

      ;;; Rotates & Shift

      ;; RLCA - Rotate A left. Old bit 7 to Carry flag.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Contains old bit 7 data.
      (#x07 (insert (format "RLCA -/- \n"))) ;; 4

      ;; RLA - Rotate A left through Carry flag.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Contains old bit 7 data.
      (#x17 (insert (format "RLA -/- \n"))) ;; 4

      ;; RRCA - Rotate A right. Old bit 0 to Carry flag.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Contains old bit 0 data.
      (#x0F (insert (format "RRCA -/- \n"))) ;; 4

      ;; RRA - Rotate A right through Carry flag.
      ;; Flags affected:
      ;; Z - Set if result is zero.
      ;; N - Reset.
      ;; H - Reset.
      ;; C - Contains old bit 0 data.
      (#x1F (insert (format "RRA -/- \n"))) ;; 4


      (#xCB (insert (format "2byte opcode CB: " ))
            (let ((bc-opcode (eboy-get-byte)))
              (eboy-inc-pc 1)
              (cl-case bc-opcode
                ;; SWAP n - Swap upper & lower nibles of n.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Reset.
                (#x37 (insert (format "SWAP A\n"))) ;; 8
                (#x30 (insert (format "SWAP B\n"))) ;; 8
                (#x31 (insert (format "SWAP C\n"))) ;; 8
                (#x32 (insert (format "SWAP D\n"))) ;; 8
                (#x33 (insert (format "SWAP E\n"))) ;; 8
                (#x34 (insert (format "SWAP H\n"))) ;; 8
                (#x35 (insert (format "SWAP L\n"))) ;; 8
                (#x36 (insert (format "SWAP (HL) \n")));; 16

                ;; RLC n - Rotate n left. Old bit 7 to Carry flag.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 7 data.
                (#x07 (insert (format "RLC A\n"))) ;; 8
                (#x00 (insert (format "RLC B\n"))) ;; 8
                (#x01 (insert (format "RLC C\n"))) ;; 8
                (#x02 (insert (format "RLC D\n"))) ;; 8
                (#x03 (insert (format "RLC E\n"))) ;; 8
                (#x04 (insert (format "RLC H\n"))) ;; 8
                (#x05 (insert (format "RLC L\n"))) ;; 8
                (#x06 (insert (format "RLC (HL) \n")));; 16

                ;; RL n - Rotate n left through Carry flag.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 7 data.
                (#x17 (insert (format "RL A\n"))) ;; 8
                (#x10 (insert (format "RL B\n"))) ;; 8
                (#x11 (insert (format "RL C\n"))) ;; 8
                (#x12 (insert (format "RL D\n"))) ;; 8
                (#x13 (insert (format "RL E\n"))) ;; 8
                (#x14 (insert (format "RL H\n"))) ;; 8
                (#x15 (insert (format "RL L\n"))) ;; 8
                (#x16 (insert (format "RL (HL) \n")));; 16

                ;; RRC n -  Rotate n right. Old bit 0 to Carry flag.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 0 data.
                (#x0F (insert (format "RRC A\n"))) ;; 8
                (#x08 (insert (format "RRC B\n"))) ;; 8
                (#x09 (insert (format "RRC C\n"))) ;; 8
                (#x0A (insert (format "RRC D\n"))) ;; 8
                (#x0B (insert (format "RRC E\n"))) ;; 8
                (#x0C (insert (format "RRC H\n"))) ;; 8
                (#x0D (insert (format "RRC L\n"))) ;; 8
                (#x0E (insert (format "RRC (HL) \n")));; 16

                ;; RR n - Rotate n right through Carry flag.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 0 data.
                (#x1F (insert (format "RR A\n"))) ;; 8
                (#x18 (insert (format "RR B\n"))) ;; 8
                (#x19 (insert (format "RR C\n"))) ;; 8
                (#x1A (insert (format "RR D\n"))) ;; 8
                (#x1B (insert (format "RR E\n"))) ;; 8
                (#x1C (insert (format "RR H\n"))) ;; 8
                (#x1D (insert (format "RR L\n"))) ;; 8
                (#x1E (insert (format "RR (HL) \n")));; 16

                ;; SLA n - Shift n left into Carry. LSB of n set to 0.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 7 data.
                (#x27 (insert (format "SLA A\n"))) ;; 8
                (#x20 (insert (format "SLA B\n"))) ;; 8
                (#x21 (insert (format "SLA C\n"))) ;; 8
                (#x22 (insert (format "SLA D\n"))) ;; 8
                (#x23 (insert (format "SLA E\n"))) ;; 8
                (#x24 (insert (format "SLA H\n"))) ;; 8
                (#x25 (insert (format "SLA L\n"))) ;; 8
                (#x26 (insert (format "SLA (HL) \n")));; 16

                ;; SRA n - Shift n right into Carry. MSB doesn't change.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 0 data.
                (#x2F (insert (format "SRA A\n"))) ;; 8
                (#x28 (insert (format "SRA B\n"))) ;; 8
                (#x29 (insert (format "SRA C\n"))) ;; 8
                (#x2A (insert (format "SRA D\n"))) ;; 8
                (#x2B (insert (format "SRA E\n"))) ;; 8
                (#x2C (insert (format "SRA H\n"))) ;; 8
                (#x2D (insert (format "SRA L\n"))) ;; 8
                (#x2E (insert (format "SRA (HL) \n")));; 16

                ;; SRL n - Shift n right into Carry. MSB set to 0.
                ;; Flags affected:
                ;; Z - Set if result is zero.
                ;; N - Reset.
                ;; H - Reset.
                ;; C - Contains old bit 0 data.
                (#x3F (insert (format "SRL A\n"))) ;; 8
                (#x38 (insert (format "SRL B\n"))) ;; 8
                (#x39 (insert (format "SRL C\n"))) ;; 8
                (#x3A (insert (format "SRL D\n"))) ;; 8
                (#x3B (insert (format "SRL E\n"))) ;; 8
                (#x3C (insert (format "SRL H\n"))) ;; 8
                (#x3D (insert (format "SRL L\n"))) ;; 8
                (#x3E (insert (format "SRL (HL) \n")));; 16

                ;; Bit Opcodes - Test bit b in register r.
                ;; Flags affected:
                ;; Z - Set if bit b of register r is 0.
                ;; N - Reset.
                ;; H - Set.
                ;; C - Not affected.
                (#x47 (insert (format "BIT b,A\n"))) ;; 8
                (#x40 (insert (format "BIT b,B\n"))) ;; 8
                (#x41 (insert (format "BIT b,C\n"))) ;; 8
                (#x42 (insert (format "BIT b,D\n"))) ;; 8
                (#x43 (insert (format "BIT b,E\n"))) ;; 8
                (#x44 (insert (format "BIT b,H\n"))) ;; 8
                (#x45 (insert (format "BIT b,L\n"))) ;; 8
                (#x46 (insert (format "BIT b,(HL) \n")));; 16

                ;; SET b,r - Set bit b in register r.
                ;; Flags affected:
                ;; None.
                (#xC7 (insert (format "SET b,A\n"))) ;; 8
                (#xC0 (insert (format "SET b,B\n"))) ;; 8
                (#xC1 (insert (format "SET b,C\n"))) ;; 8
                (#xC2 (insert (format "SET b,D\n"))) ;; 8
                (#xC3 (insert (format "SET b,E\n"))) ;; 8
                (#xC4 (insert (format "SET b,H\n"))) ;; 8
                (#xC5 (insert (format "SET b,L\n"))) ;; 8
                (#xC6 (insert (format "SET b,(HL) \n")));; 16

                ;; RES b,r - Reset bit b in register r.
                ;; Flags affected:
                ;; None.
                (#x87 (insert (format "RES b,A\n"))) ;; 8
                (#x80 (insert (format "RES b,B\n"))) ;; 8
                (#x81 (insert (format "RES b,C\n"))) ;; 8
                (#x82 (insert (format "RES b,D\n"))) ;; 8
                (#x83 (insert (format "RES b,E\n"))) ;; 8
                (#x84 (insert (format "RES b,H\n"))) ;; 8
                (#x85 (insert (format "RES b,L\n"))) ;; 8
                (#x86 (insert (format "RES b,(HL) \n")));; 16
                (otherwise (insert (format "Unimplemented BC opcode 0x%x\n" opcode)))
                )))


      ;; JP nn - Jump to address nn.
      (#xC3 (insert (format "JP $%04x\n" (eboy-get-short))) ;; 12
            ;;(setq eboy-pc (logior (lsh (aref eboy-rom (+ eboy-pc 2)) 8) (aref eboy-rom (+ eboy-pc 1))))
            (eboy-inc-pc 2))

      ;; JP cc,nn - Jump to address n if following condition is true:
      ;;   cc = NZ, Jump if Z flag is reset.
      ;;   cc = Z, Jump if Z flag is set.
      ;;   cc = NC, Jump if C flag is reset.
      ;;   cc = C, Jump if C flag is set.
      ;; nn = two byte immediate value. (LS byte first.)
      (#xC2 (insert (format "JP NZ,nn  \n")));; 12
      (#xCA (insert (format "JP Z,nn  \n")));; 12
      (#xD2 (insert (format "JP NC,nn  \n")));; 12
      (#xDA (insert (format "JP C,nn  \n")));; 12

      ;; JP (HL) - Jump to address contained in HL.
      (#xE9 (insert (format "JP (HL) \n"))) ;; 4

      ;; JR n - Add n to current address and jump to it.
      ;; nn = one byte signed immediate value
      (#x18 (insert (format "JR n \n"))) ;; 8

      ;; JR cc,n - If following condition is true then add n to current address and jump to it:
      ;;  n = one byte signed immediate value
      ;;  cc = NZ, Jump if Z flag is reset.
      ;;  cc = Z, Jump if Z flag is set.
      ;;  cc = NC, Jump if C flag is reset.
      ;;  cc = C, Jump if C flag is set.
      (#x20 (insert (format "JR NZ,* \n"))) ;; 8
      (#x28 (insert (format "JR Z,* \n"))) ;; 8
      (#x30 (insert (format "JR NC,* \n"))) ;; 8
      (#x38 (insert (format "JR C,* \n"))) ;; 8


      ;;; Calls

      ;; CALL nn - Push address of next instruction onto stack and then jump to address nn.
      ;;  nn = two byte immediate value. (LS byte first.)
      (#xCD (insert (format "CALL nn  \n")));; 12

      ;; CALL cc,nn - Call address n if following condition is true:
      ;;  cc = NZ, Call if Z flag is reset.
      ;;  cc = Z, Call if Z flag is set.
      ;;  cc = NC, Call if C flag is reset.
      ;;  cc = C, Call if C flag is set.
      ;;  nn = two byte immediate value. (LS byte first.)
      (#xC4 (insert (format "CALL NZ,nn  \n")));; 12
      (#xCC (insert (format "CALL Z,nn  \n")));; 12
      (#xD4 (insert (format "CALL NC,nn  \n")));; 12
      (#xDC (insert (format "CALL C,nn  \n")));; 12


      ;;; Restarts

      ;; RST n - Push present address onto stack. Jump to address $0000 + n.
      ;;  n = $00,$08,$10,$18,$20,$28,$30,$38
      (#xC7 (insert (format "RST 00H  \n")));; 32
      (#xCF (insert (format "RST 08H  \n")));; 32
      (#xD7 (insert (format "RST 10H  \n")));; 32
      (#xDF (insert (format "RST 18H  \n")));; 32
      (#xE7 (insert (format "RST 20H  \n")));; 32
      (#xEF (insert (format "RST 28H  \n")));; 32
      (#xF7 (insert (format "RST 30H  \n")));; 32
      (#xFF (insert (format "RST 38H  \n")));; 32

      ;;; Returns

      ;; RET - Pop two bytes from stack & jump to that address.
      (#xC9 (insert (format "RET -/- \n"))) ;; 8

      ;; RET cc - Return if following condition is true:
      ;;  cc = NZ, Return if Z flag is reset.
      ;;  cc = Z, Return if Z flag is set.
      ;;  cc = NC, Return if C flag is reset.
      ;;  cc = C, Return if C flag is set.
      (#xC0 (insert (format "RET NZ \n"))) ;; 8
      (#xC8 (insert (format "RET Z \n"))) ;; 8
      (#xD0 (insert (format "RET NC \n"))) ;; 8
      (#xD8 (insert (format "RET C \n"))) ;; 8

      ;; RETI - Pop two bytes from stack & jump to that address then enable interrupts.
      (#xD9 (insert (format "RETI -/- \n"))) ;; 8


      (otherwise (insert (format "Unimplemented opcode 0x%x\n" opcode)))
      )
    (eboy-inc-pc 1)
    )

(defun eboy-load-rom ()
  "Load the rom file.  For now just automatically load a test rom."
  (interactive)
  (setq eboy-rom-filename "roms/test_rom.gb")
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))
  (setq eboy-pc eboy-pc-start-address)
  (setq eboy-debugging-nr-instructions 0)
  (eboy-reset-CPU-flags)
  (switch-to-buffer "*eboy*")
  (erase-buffer)
  (insert (format "Load rom: %s\n" eboy-rom-filename))
  (insert (format "Rom size: %d bytes\n" eboy-rom-size))
  (insert (format "Rom title: %s\n" (eboy-rom-title)))
  ;; loop
  (while (and (< eboy-pc eboy-rom-size) (< eboy-debugging-nr-instructions 500))
    (eboy-process-opcode (aref eboy-rom eboy-pc))
    (setq eboy-debugging-nr-instructions (+ eboy-debugging-nr-instructions 1))
    )
  )

(eboy-load-rom)


(provide 'eboy)
;;; eboy.el ends here

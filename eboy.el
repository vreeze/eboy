;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;;; Commentary:
(eval-when-compile (require 'cl))
;;(require 'cl-lib)
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
(defconst eboy-sp-initial-value #xFFFE "Initial value of the stack pointer.")
(defvar eboy-rom-filename nil "The file name of the loaded rom.")
(defvar eboy-rom nil "The binary vector of the rom.")
(defvar eboy-rom-size nil "The size of the rom in bytes.")
(defvar eboy-pc nil "The program counter.")
(defvar eboy-sp nil "The stack pointer.")

;;(defvar eboy-flags (make-bool-vector 4 t) "The flags Z(Zero) S(Negative) H(Halve Carry) and C(Carry).")
(defvar eboy-debug-nr-instructions nil "Number of instructions executed.")


(defvar eboy-r-A 0 "Register A.")
(defvar eboy-r-B 0 "Register B.")
(defvar eboy-r-C 0 "Register C.")
(defvar eboy-r-D 0 "Register D.")
(defvar eboy-r-E 0 "Register E.")
;;(defvar eboy-r-F 0 "Register F.") flags
(defvar eboy-r-H 0 "Register H.")
(defvar eboy-r-L 0 "Register L.")

(defmacro eboy-add-byte (byte value)
  "Simulate byte behavior: add VALUE to BYTE."
  `(progn (setq ,byte (+ ,byte ,value))
          (if (> ,byte 255)
              (setq ,byte (- ,byte 256)))
          (if (< ,byte 0)
              (setq ,byte (+ ,byte 256))))
  )

(defmacro eboy-subtract-byte (byte value)
  "Simulate byte behavior: subtract VALUE from BYTE."
  `(progn (setq ,byte (- ,byte ,value))
          (if (< ,byte 0)
              (setq ,byte (+ ,byte 256))))
  )

(defun eboy-byte-to-signed (byte)
  "Convert BYTE to signed number."
  (* (1+ (logxor byte #xFF)) -1)
  )

(defmacro eboy-dec (reg flags)
  "CPU Instruction: Decrement register REG and update FLAGS."
  `(progn (eboy-add-byte ,reg -1)
          (eboy-set-flag ,flags ,:Z (= ,reg 0))
          (eboy-set-flag ,flags ,:N t)
          (eboy-set-flag ,flags ,:H (= (logand ,reg #xF) #xF))
     )
)



(defun eboy-init-registers ()
  "Initialize all registers."
  (setq eboy-r-A #x01) ;; 0x01:GB/SGB, 0xFF:GBP, 0x11:GBC
  (setq eboy-r-B 0)
  (setq eboy-r-C #x13)
  (setq eboy-r-D 0)
  (setq eboy-r-E #xD8)
  (setq eboy-r-H #x01)
  (setq eboy-r-L #x4d)
  )

(defun eboy-init-memory ()
  "Initialize the RAM to some startup values."
  (eboy-write-byte-to-memory #xFF05 #x00) ; TIMA
  (eboy-write-byte-to-memory #xFF06 #x00) ; TMA
  (eboy-write-byte-to-memory #xFF07 #x00) ; TAC
  (eboy-write-byte-to-memory #xFF10 #x80) ; NR10
  (eboy-write-byte-to-memory #xFF11 #xBF) ; NR11
  (eboy-write-byte-to-memory #xFF12 #xF3) ; NR12
  (eboy-write-byte-to-memory #xFF14 #xBF) ; NR14
  (eboy-write-byte-to-memory #xFF16 #x3F) ; NR21
  (eboy-write-byte-to-memory #xFF17 #x00) ; NR22
  (eboy-write-byte-to-memory #xFF19 #xBF) ; NR24
  (eboy-write-byte-to-memory #xFF1A #x7F) ; NR30
  (eboy-write-byte-to-memory #xFF1B #xFF) ; NR31
  (eboy-write-byte-to-memory #xFF1C #x9F) ; NR32
  (eboy-write-byte-to-memory #xFF1E #xBF) ; NR33
  (eboy-write-byte-to-memory #xFF20 #xFF) ; NR41
  (eboy-write-byte-to-memory #xFF21 #x00) ; NR42
  (eboy-write-byte-to-memory #xFF22 #x00) ; NR43
  (eboy-write-byte-to-memory #xFF23 #xBF) ; NR30
  (eboy-write-byte-to-memory #xFF24 #x77) ; NR50
  (eboy-write-byte-to-memory #xFF25 #xF3) ; NR51
  (eboy-write-byte-to-memory #xFF26 #xF1) ; NR52, F1-GB, $F0-SGB
  (eboy-write-byte-to-memory #xFF40 #x91) ; LCDC
  (eboy-write-byte-to-memory #xFF42 #x00) ; SCY
  (eboy-write-byte-to-memory #xFF43 #x00) ; SCX
  (eboy-write-byte-to-memory #xFF45 #x00) ; LYC
  (eboy-write-byte-to-memory #xFF47 #xFC) ; BGP
  (eboy-write-byte-to-memory #xFF48 #xFF) ; OBP0
  (eboy-write-byte-to-memory #xFF49 #xFF) ; OBP1
  (eboy-write-byte-to-memory #xFF4A #x00) ; WY
  (eboy-write-byte-to-memory #xFF4B #x00) ; WX
  (eboy-write-byte-to-memory #xFFFF #x00) ; IE
  )

(defun eboy-debug-dump-memory (start-address end-address)
  "Dump the memory from START-ADDRESS to END-ADDRESS."
  (insert (format "\nMemory: 0x%04x - 0x%04x\n" start-address end-address))
  (dotimes (i (1+ (- end-address start-address)))
    (let* ((address (+ start-address i))
           (data (eboy-read-byte-from-memory (+ start-address i))))
      (if (= (mod address #x10) 0)
          (insert (format "\n%04x: " address)))
      (insert (format "%02x " data))
      )
    )
  (insert "\n")
  )

(defvar eboy-ram (make-vector (* 8 1024) 0) "The 8 kB interal RAM.")
(defvar eboy-vram (make-vector (* 8 1024) 0) "The 8 kB interal Video RAM.")

;;; Memory:
;; Iterrupt Enable Register
;; --------------------------- FFFF
;; Internal RAM
;; --------------------------- FF80
;; Empty but unusable for I/O
;; --------------------------- FF4C
;; I/O ports
;; --------------------------- FF00
;; Empty but unusable for I/O
;; --------------------------- FEA0
;; Sprite Attrib Memory (OAM)
;; --------------------------- FE00
;; Echo of 8kB Internal RAM
;; --------------------------- E000
;; 8kB Internal RAM
;; --------------------------- C000
;; 8kB switchable RAM bank
;; --------------------------- A000
;; 8kB Video RAM
;; --------------------------- 8000 --
;; 16kB switchable ROM bank         |
;; --------------------------- 4000  |= 32kB Cartrigbe
;; 16kB ROM bank #0                 |
;; --------------------------- 0000 --
;; * NOTE: b = bit, B = byte

(defun eboy-read-byte-from-memory (address)
  "Read byte from ADDRESS."
  (cond
   ;; Iterrupt Enable Register??

   ((and (>= address #xFF80) (<= address #xFFFF))
    ;;(message "Internal RAM")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xFF4C) (< address #xFF80))
    ;;(message "Empty but unusable for I/O")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xFF00) (< address #xFF4C))
    ;;(message "I/O ports")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xFEA0) (< address #xFF00))
    ;;(message "Empty but unusable for I/O")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xFE00) (< address #xFEA0))
    ;;(message "Sprite Attrib Memory (OAM)")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xE000) (< address #xFE00))
    ;;(message "Echo of 8kB Internal RAM")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xC000) (< address #xE000))
    ;;(message "8kB Internal RAM")
    (aref eboy-ram (- address #xC000))
    )
   ((and (>= address #xA000) (< address #xC000))
    ;;(message "8kB switchable RAM bank")
    )
   ((and (>= address #x8000) (< address #xA000))
    ;;(message "8kB Video RAM")
    (aref eboy-vram (- address #x8000))
    )

   ;; 32kB Cartidge
   ((and (>= address #x4000) (< address #x8000))
    ;;(message "16kB switchable ROM bank")
    (aref eboy-rom address))
   ((and (>= address #x0000) (< address #x4000))
    ;;(message "16kB ROM bank #0")
    (aref eboy-rom address))
   ))

(defun eboy-write-byte-to-memory (address data)
  "Write DATA byte to memory at ADDRESS."
  (cond
   ;; Iterrupt Enable Register??

   ((and (>= address #xFF80) (<= address #xFFFF))
    ;; (message "Write Internal RAM")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFF4C) (< address #xFF80))
    ;; (message "Write Empty but unusable for I/O")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFF00) (< address #xFF4C))
    ;; (message "Write I/O ports")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFEA0) (< address #xFF00))
    ;; (message "Write Empty but unusable for I/O")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFE00) (< address #xFEA0))
    ;; (message "Write Sprite Attrib Memory (OAM)")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xE000) (< address #xFE00))
    ;; (message "Write Echo of 8kB Internal RAM")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xC000) (< address #xE000))
    ;; (message "Write 8kB Internal RAM")
    (aset eboy-ram (- address #xC000) data))
   ((and (>= address #xA000) (< address #xC000))
    ;; (message "Write 8kB switchable RAM bank")
    )
   ((and (>= address #x8000) (< address #xA000))
    ;; (message "Write 8kB Video RAM")
    (aset eboy-vram (- address #x8000) data))

   ;; 32kB Cartidge
   ((and (>= address #x4000) (< address #x8000))
    (assert nil t "Non writable: 16kB switchable ROM bank"))
   ((and (>= address #x0000) (< address #x4000))
    (assert nil t "Non writable: 16kB ROM bank #0"))
   ))

;;(eboy-write-byte-to-memory #x4100 23)
;;(message "message %x" (eboy-read-byte-from-memory #x101))

(defun eboy-debug-print-flags (flags)
  "Print the FLAGS."
  (insert (format "Flags; Z:%s N:%s H:%s C:%s\n" (eboy-get-flag flags :Z) (eboy-get-flag  flags :N) (eboy-get-flag flags :H) (eboy-get-flag flags :C)))
  )

(defun eboy-debug-print-cpu-state (flags)
  "Print the registers and FLAGS."
  (insert (format "\t\t\t\tReg;  A:%3d B:%003d C:%3d D:%3d E:%3d H:%3d L:%3d  Flags; Z:%3s N:%3s H:%3s C:%3s\n" eboy-r-A  eboy-r-B  eboy-r-C  eboy-r-D  eboy-r-E  eboy-r-H  eboy-r-L (eboy-get-flag flags :Z) (eboy-get-flag  flags :N) (eboy-get-flag flags :H) (eboy-get-flag flags :C)))
  )

(defun eboy-set-flags (flags new-flags)
  "Set FLAGS to NEW-FLAGS."
  (eboy-set-flag flags (caar new-flags) (cadar new-flags))
  ;;(insert (format "%s %s" (caar new-flags) (cadar new-flags)) )
  (unless (null (cdr new-flags))
    (eboy-set-flags flags (cdr new-flags)))
  )

(defun eboy-set-flag (flags flag state)
  "Set FLAG in FLAGS to STATE."
  (ecase flag
    (:Z (aset flags 0 state))
    (:N (aset flags 1 state))
    (:H (aset flags 2 state))
    (:C (aset flags 3 state))
    )
  )

(defun eboy-get-flag (flags flag)
  "Get FLAG from FLAGS."
  (ecase flag
    (:Z (aref flags 0))
    (:N (aref flags 1))
    (:H (aref flags 2))
    (:C (aref flags 3))
    )
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

(defun eboy-set-r-BC (value)
  "Put VALUE into registers BC."
  (setq eboy-r-B (logand (lsh value -8) #xff))
  (setq eboy-r-C (logand value #xff))  )

(defun eboy-set-r-DE (value)
  "Put VALUE into registers DE."
  (setq eboy-r-D (logand (lsh value -8) #xff))
  (setq eboy-r-E (logand value #xff))  )

(defun eboy-set-r-HL (value)
  "Put VALUE into registers BC."
  ;;(if (> value #xFFFF)
;;      (setq value (- value #xFFFF)))
  (setq eboy-r-H (logand (lsh value -8) #xff))
  (setq eboy-r-L (logand value #xff)))

(defun eboy-get-r-HL ()
  "Get short by combining byte register H and L."
  (logior (lsh eboy-r-H 8) eboy-r-L) )

(defun eboy-debug-unimplemented-opcode (opcode)
  "Print OPCODE is unimplemented."
  (insert (format "Unimplemented opcode 0x%02x" opcode))
  )


(defun eboy-process-opcode (opcode flags)
  "Process OPCODE, cpu FLAGS state."
  (eboy-debug-print-cpu-state flags)
  (insert (format "pc: 0x%x  " eboy-pc))
  (cl-case opcode
    (#x00 (insert "NOP\n"))

    ;; LD nn,n
    (#x06
     ;;(eboy-debug-unimplemented-opcode 6)
     (insert (format "0x%02x: LD B, #0x%02x\n" opcode (eboy-get-byte)))
     (setq eboy-r-B (eboy-get-byte))
     (eboy-inc-pc 1))
    (#x0E (insert (format "0x%02x: LD C, #0x%02x\n" opcode (eboy-get-byte)))
          (setq eboy-r-C (eboy-get-byte))
          (eboy-inc-pc 1))
    (#x16 (insert (format "0x%02x: LD D, #0x%02x\n" opcode (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x1E (insert (format "0x%02x: LD E, #0x%02x\n" opcode (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x26 (insert (format "0x%02x: LD H, #0x%02x\n" opcode (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x2E (insert (format "0x%02x: LD L, #0x%02x\n" opcode (eboy-get-byte)))
          (eboy-inc-pc 1))

    ;; ;; LD r1,r2
    (#x7F (insert (format "0x%02x: LD A,A\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x78 (insert (format "0x%02x: LD A,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x79 (insert (format "0x%02x: LD A,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x7A (insert (format "0x%02x: LD A,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x7B (insert (format "0x%02x: LD A,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x7C (insert (format "0x%02x: LD A,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x7D (insert (format "0x%02x: LD A,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x7E (insert (format "0x%02x: LD A,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x40 (insert (format "0x%02x: LD B,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x41 (insert (format "0x%02x: LD B,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x42 (insert (format "0x%02x: LD B,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x43 (insert (format "0x%02x: LD B,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x44 (insert (format "0x%02x: LD B,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x45 (insert (format "0x%02x: LD B,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x46 (insert (format "0x%02x: LD B,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x48 (insert (format "0x%02x: LD C,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x49 (insert (format "0x%02x: LD C,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x4A (insert (format "0x%02x: LD C,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x4B (insert (format "0x%02x: LD C,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x4C (insert (format "0x%02x: LD C,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x4D (insert (format "0x%02x: LD C,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x4E (insert (format "0x%02x: LD C,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x50 (insert (format "0x%02x: LD D,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x51 (insert (format "0x%02x: LD D,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x52 (insert (format "0x%02x: LD D,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x53 (insert (format "0x%02x: LD D,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x54 (insert (format "0x%02x: LD D,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x55 (insert (format "0x%02x: LD D,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x56 (insert (format "0x%02x: LD D,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x58 (insert (format "0x%02x: LD E,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x59 (insert (format "0x%02x: LD E,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x5A (insert (format "0x%02x: LD E,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x5B (insert (format "0x%02x: LD E,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x5C (insert (format "0x%02x: LD E,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x5D (insert (format "0x%02x: LD E,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x5E (insert (format "0x%02x: LD E,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x60 (insert (format "0x%02x: LD H,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x61 (insert (format "0x%02x: LD H,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x62 (insert (format "0x%02x: LD H,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x63 (insert (format "0x%02x: LD H,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x64 (insert (format "0x%02x: LD H,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x65 (insert (format "0x%02x: LD H,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x66 (insert (format "0x%02x: LD H,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x68 (insert (format "0x%02x: LD L,B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x69 (insert (format "0x%02x: LD L,C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x6A (insert (format "0x%02x: LD L,D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x6B (insert (format "0x%02x: LD L,E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x6C (insert (format "0x%02x: LD L,H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x6D (insert (format "0x%02x: LD L,L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x6E (insert (format "0x%02x: LD L,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x70 (insert (format "0x%02x: LD (HL),B\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x71 (insert (format "0x%02x: LD (HL),C\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x72 (insert (format "0x%02x: LD (HL),D\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x73 (insert (format "0x%02x: LD (HL),E\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x74 (insert (format "0x%02x: LD (HL),H\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x75 (insert (format "0x%02x: LD (HL),L\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x36 (insert (format "0x%02x: LD (HL),n\n" opcode)) (assert nil t "unimplemented opcode"))

    ;; LD A,n
    (#x0A (insert (format "0x%02x: LD A,(BC)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x1A (insert (format "0x%02x: LD A,(DE)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#xFA (insert (format "0x%02x: LD A,(nn)\n" opcode)) (assert nil t "unimplemented opcode"))
    (#x3E (insert (format "0x%02x: LD A,#0x%02x\n" opcode (eboy-get-byte))) (eboy-inc-pc 1) (assert nil t "unimplemented opcode"))

    ;; LD n,A - Put value A into n.
    ;; n = A,B,C,D,E,H,L,(BC),(DE),(HL),(nn)
    ;; nn = two byte immediate value. (LS byte first.)
    (#x47 (insert (format "0x%02x: LD B,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x4F (insert (format "0x%02x: LD C,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x57 (insert (format "0x%02x: LD D,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x5F (insert (format "0x%02x: LD E,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x67 (insert (format "0x%02x: LD H,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x6F (insert (format "0x%02x: LD L,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x02 (insert (format "0x%02x: LD (BC),A\n" opcode) (assert nil t "unimplemented opcode"))
          ;;(eboy-set-r-BC eboy-r-A) maybe (BC) means memory address?
          )                   ;; 8
    (#x12 (insert (format "0x%02x: LD (DE),A\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 8
    (#x77 (insert (format "0x%02x: LD (HL),A\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xEA (insert (format "0x%02x: LD (nn),A\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 16

    ;; Put value at address $FF00 + register C into A.
    (#xF2 (insert (format "0x%02x: LD A,($FF00+C)" opcode)) (assert nil t "unimplemented opcode") )
    ;; Put A into address $FF00 + register C.
    (#xE2 (insert (format "0x%02x: LD ($FF00+C),A\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put value at address HL into A. Decrement HL.
    (#x3A (insert (format "0x%02x: LD A,(HLD)\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address HL. Decrement HL.
    (#x32 (insert (format "0x%02x: LD (HLD),A\n" opcode))
          (eboy-write-byte-to-memory (eboy-get-r-HL) eboy-r-A)
          (eboy-set-r-HL (1- (eboy-get-r-HL)))
          ) ;; 8
    ;; Put value at address HL into A. Increment HL.
    (#x2A (insert (format "0x%02x: LD A,(HLI)\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address HL. Increment HL.
    (#x22 (insert (format "0x%02x: LD (HLI),A\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address $FF00+n
    (#xE0 (insert (format "0x%02x: LD ($FF00+n),A\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put memory address $FF00+n into A.
    (#xF0 (insert (format "0x%02x: LD A,($FF00+n)\n" opcode)) (assert nil t "unimplemented opcode"))

    ;; 16 bit loads, nn = 16 bit immediate value
    (#x01 (insert (format "0x%02x: LD BC, $%04x\n" opcode (eboy-get-short)))(eboy-inc-pc 2) (assert nil t "unimplemented opcode")) ;; 12
    (#x11 (insert (format "0x%02x: LD DE, $%04x\n" opcode (eboy-get-short))) (assert nil t "unimplemented opcode")) ;; 12
    (#x21 (insert (format "0x%02x: LD HL, $%04x\n" opcode (eboy-get-short)))
          (eboy-set-r-HL (eboy-get-short))
          (eboy-inc-pc 2)
          ) ;; 12
    (#x31 (insert (format "0x%02x: LD SP, $%04x\n" opcode (eboy-get-short))) (assert nil t "unimplemented opcode")) ;; 12

    (#xF9 (insert (format "0x%02x: LD SP,HL\n" opcode)) (assert nil t "unimplemented opcode"))

    ;; Put SP + n effective address into HL, n = one byte signed immediate value.
    ;; Flags affected:
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xF8 (insert (format "0x%02x: LDHL SP,n\n" opcode)) (assert nil t "unimplemented opcode"))
    ;; Put Stack Pointer (SP) at address n. n = two byte immediate address
    (#x08 (insert (format "0x%02x: LD (nn),SP\n" opcode)) (assert nil t "unimplemented opcode"))

    ;; Push register pair nn onto stack.
    ;; Decrement Stack Pointer (SP) twice.
    (#xF5 (insert (format "0x%02x: PUSH AF\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xC5 (insert (format "0x%02x: PUSH BC\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xD5 (insert (format "0x%02x: PUSH DE\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xE5 (insert (format "0x%02x: PUSH HL\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 16

    ;; Pop two bytes off stack into register pair nn.
    ;; Increment Stack Pointer (SP) twice.
    (#xF1 (insert (format "0x%02x: POP AF\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xC1 (insert (format "0x%02x: POP BC\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xD1 (insert (format "0x%02x: POP DE\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xE1 (insert (format "0x%02x: POP HL\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 12


    ;; ADD A,n    -    Add n to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x87 (insert (format "0x%02x: ADD A,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x80 (insert (format "0x%02x: ADD A,B\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x81 (insert (format "0x%02x: ADD A,C\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x82 (insert (format "0x%02x: ADD A,D\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x83 (insert (format "0x%02x: ADD A,E\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x84 (insert (format "0x%02x: ADD A,H\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x85 (insert (format "0x%02x: ADD A,L\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x86 (insert (format "0x%02x: ADD A,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xC6 (insert (format "0x%02x: ADD A,#\n" opcode))
          (setq eboy-r-A (+ eboy-r-A (eboy-get-byte))))                      ;; 8

    ;;ADC A,n    -   Add n + Carry flag to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x8F (insert (format "0x%02x: ADC A,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x88 (insert (format "0x%02x: ADC A,B\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x89 (insert (format "0x%02x: ADC A,C\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8A (insert (format "0x%02x: ADC A,D\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8B (insert (format "0x%02x: ADC A,E\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8C (insert (format "0x%02x: ADC A,H\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8D (insert (format "0x%02x: ADC A,L\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8E (insert (format "0x%02x: ADC A,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xCE (insert (format "0x%02x: ADC A,#\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 8

    ;; SUB n - Subtract n from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x97 (insert (format "0x%02x: SUB A\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x90 (insert (format "0x%02x: SUB B\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x91 (insert (format "0x%02x: SUB C\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x92 (insert (format "0x%02x: SUB D\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x93 (insert (format "0x%02x: SUB E\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x94 (insert (format "0x%02x: SUB H\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x95 (insert (format "0x%02x: SUB L\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x96 (insert (format "0x%02x: SUB (HL)\n" opcode)) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xD6 (insert (format "0x%02x: SUB #\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; SBC A,n - Subtract n + Carry flag from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x9F (insert (format "0x%02x: SBC A,A\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x98 (insert (format "0x%02x: SBC A,B\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x99 (insert (format "0x%02x: SBC A,C\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9A (insert (format "0x%02x: SBC A,D\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9B (insert (format "0x%02x: SBC A,E\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9C (insert (format "0x%02x: SBC A,H\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9D (insert (format "0x%02x: SBC A,L\n" opcode)) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9E (insert (format "0x%02x: SBC A,(HL)\n" opcode)) (assert nil t "unimplemented opcode"))                   ;; 8
    ;;(?? (insert (format "0x%02x: SBC A,#\n" opcode)))                      ;; ?

    ;; AND n - Logically AND n with A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set.
    ;; C - Reset.
    (#xA7 (insert (format "0x%02x: AND A\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA0 (insert (format "0x%02x: AND B\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA1 (insert (format "0x%02x: AND C\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA2 (insert (format "0x%02x: AND D\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA3 (insert (format "0x%02x: AND E\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA4 (insert (format "0x%02x: AND H\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA5 (insert (format "0x%02x: AND L\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA6 (insert (format "0x%02x: AND (HL)\n" opcode)) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xE6 (insert (format "0x%02x: AND #\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; OR n - Logical OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xB7 (insert (format "0x%02x: OR A\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB0 (insert (format "0x%02x: OR B\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB1 (insert (format "0x%02x: OR C\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB2 (insert (format "0x%02x: OR D\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB3 (insert (format "0x%02x: OR E\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB4 (insert (format "0x%02x: OR H\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB5 (insert (format "0x%02x: OR L\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB6 (insert (format "0x%02x: OR (HL)\n" opcode)) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xF6 (insert (format "0x%02x: OR #\n" opcode)) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; XOR n - Logical exclusive OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xAF (insert (format "0x%02x: XOR A \n" opcode))
          (setq eboy-r-A (logxor eboy-r-A eboy-r-A))
          (if (zerop eboy-r-A)
              (eboy-set-flag flags :Z t))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil)
          ) ;; 4
    (#xA8 (insert (format "0x%02x: XOR B \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xA9 (insert (format "0x%02x: XOR C \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xAA (insert (format "0x%02x: XOR D \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xAB (insert (format "0x%02x: XOR E \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xAC (insert (format "0x%02x: XOR H \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xAD (insert (format "0x%02x: XOR L \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xAE (insert (format "0x%02x: XOR (HL) \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#xEE (insert (format "0x%02x: XOR * \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; CP n - Compare A with n. This is basically an A - n subtraction instruction but the results are thrown away.
    ;; Flags affected:
    ;; Z - Set if result is zero. (Set if A = n.)
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set for no borrow. (Set if A < n.)
    (#xBF (insert (format "0x%02x: CP A \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xB8 (insert (format "0x%02x: CP B \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xB9 (insert (format "0x%02x: CP C \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xBA (insert (format "0x%02x: CP D \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xBB (insert (format "0x%02x: CP E \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xBC (insert (format "0x%02x: CP H \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xBD (insert (format "0x%02x: CP L \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#xBE (insert (format "0x%02x: CP (HL) \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#xFE (insert (format "0x%02x: CP # \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; INC n - Increment register n.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Not affected.
    (#x3C (insert (format "0x%02x: INC A \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x04 (insert (format "0x%02x: INC B \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x0C (insert (format "0x%02x: INC C \n" opcode))
          (eboy-add-byte eboy-r-C 1)
          (if (= (logand eboy-r-C #xff) 0)
              (progn (eboy-set-flag flags :Z t)
                     (setq eboy-r-C 0)))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-r-C #xf) (logand (1- eboy-r-C) #xf)))
          ) ;; 4
    (#x14 (insert (format "0x%02x: INC D \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x1C (insert (format "0x%02x: INC E \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x24 (insert (format "0x%02x: INC H \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x2C (insert (format "0x%02x: INC L \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x34 (insert (format "0x%02x: INC (HL)  \n" opcode)) (assert nil t "unimplemented opcode"));; 12

    ;; DEC n - Decrement register n.
    ;; Flags affected:
    ;; Z - Set if reselt is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Not affected.
    (#x3D (insert (format "0x%02x: DEC A \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    (#x05 (insert (format "0x%02x: DEC B \n" opcode))
          ;; (eboy-add-byte eboy-r-B -1)
          ;; (eboy-set-flag flags :Z (= eboy-r-B 0))
          ;; (eboy-set-flag flags :N t)
          ;; (eboy-set-flag flags :H (= (logand eboy-r-B #xF) #xF))
          (eboy-dec eboy-r-B flags)
          ) ;; 4
    (#x0D (insert (format "0x%02x: DEC C \n" opcode)) (eboy-dec eboy-r-C flags)) ;; 4
    (#x15 (insert (format "0x%02x: DEC D \n" opcode)) (eboy-dec eboy-r-D flags)) ;; 4
    (#x1D (insert (format "0x%02x: DEC E \n" opcode)) (eboy-dec eboy-r-E flags)) ;; 4
    (#x25 (insert (format "0x%02x: DEC H \n" opcode)) (eboy-dec eboy-r-H flags)) ;; 4
    (#x2D (insert (format "0x%02x: DEC L \n" opcode)) (eboy-dec eboy-r-L flags)) ;; 4
    (#x35 (insert (format "0x%02x: DEC (HL)  \n" opcode)) (assert nil t "unimplemented opcode"));; 12

      ;;; 16-Bit Arithmetic
    ;;
    ;; ADD HL,n - Add n to HL.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Set if carry from bit 11.
    ;; C - Set if carry from bit 15.
    (#x09 (insert (format "0x%02x: ADD HL,BC \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x19 (insert (format "0x%02x: ADD HL,DE \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x29 (insert (format "0x%02x: ADD HL,HL \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x39 (insert (format "0x%02x: ADD HL,SP \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; ADD SP,n - Add n to Stack Pointer (SP).
    ;; Flags affected:
    ;; None.
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xE8 (insert (format "0x%02x: ADD SP,#  \n" opcode)) (assert nil t "unimplemented opcode"));; 16

    ;; INC nn - Increment register nn.
    ;; Flags affected:
    (#x03 (insert (format "0x%02x: INC BC \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x13 (insert (format "0x%02x: INC DE \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x23 (insert (format "0x%02x: INC HL \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x33 (insert (format "0x%02x: INC SP \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; DEC nn - Decrement register nn.
    ;; Flags affected:
    ;; None.
    (#x0B (insert (format "0x%02x: DEC BC \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x1B (insert (format "0x%02x: DEC DE \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x2B (insert (format "0x%02x: DEC HL \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x3B (insert (format "0x%02x: DEC SP \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

      ;;; Miscellaneous



    ;; DAA - Decimal adjust register A.
    ;; This instruction adjusts register A so that the correct representation of Binary Coded Decimal (BCD) is obtained.
    ;; Flags affected:
    ;; Z - Set if register A is zero.
    ;; N - Not affected.
    ;; H - Reset.
    ;; C - Set or reset according to operation.
    (#x27 (insert (format "0x%02x: DAA -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; CPL - Complement A register. (Flip all bits.)
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Set.
    ;; H - Set.
    ;; C - Not affected.
    (#x2F (insert (format "0x%02x: CPL -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; CCF - Complement carry flag.
    ;; If C flag is set, then reset it.
    ;; If C flag is reset, then set it.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Complemented.
    (#x3F (insert (format "0x%02x: CCF -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; SCF - Set Carry flag.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Set.
    (#x37 (insert (format "0x%02x: SCF -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; HALT - Power down CPU until an interrupt occurs. Use this when ever possible to reduce energy consumption.
    (#x76 (insert (format "0x%02x: HALT -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; STOP - Halt CPU & LCD display until button pressed.
    (#x10 (insert (format "0x%02x: STOP -/- 10 \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    ;; This is a 2 byte opcode: 0x10 00

    ;; DI - This instruction disables interrupts but not immediately. Interrupts are disabled after instruction after DI is executed.
    ;; Flags affected:
    ;; None.
    (#xF3 (insert (format "0x%02x: DI -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4
    ;; EI - Enable interrupts. This intruction enables interrupts but not immediately. Interrupts are enabled after instruction after EI is executed.
    ;; Flags affected:
    ;; None.
    (#xFB (insert (format "0x%02x: EI -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

      ;;; Rotates & Shift

    ;; RLCA - Rotate A left. Old bit 7 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x07 (insert (format "0x%02x: RLCA -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; RLA - Rotate A left through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x17 (insert (format "0x%02x: RLA -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; RRCA - Rotate A right. Old bit 0 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x0F (insert (format "0x%02x: RRCA -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; RRA - Rotate A right through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x1F (insert (format "0x%02x: RRA -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4


    (#xCB (insert (format "0x%02x: 2byte opcode CB: " opcode ) (assert nil t "unimplemented opcode"))
          (let ((bc-opcode (eboy-get-byte)))
            (eboy-inc-pc 1)
            (cl-case bc-opcode
              ;; SWAP n - Swap upper & lower nibles of n.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Reset.
              (#x37 (insert (format "0x%02x: SWAP A\n" opcode))) ;; 8
              (#x30 (insert (format "0x%02x: SWAP B\n" opcode))) ;; 8
              (#x31 (insert (format "0x%02x: SWAP C\n" opcode))) ;; 8
              (#x32 (insert (format "0x%02x: SWAP D\n" opcode))) ;; 8
              (#x33 (insert (format "0x%02x: SWAP E\n" opcode))) ;; 8
              (#x34 (insert (format "0x%02x: SWAP H\n" opcode))) ;; 8
              (#x35 (insert (format "0x%02x: SWAP L\n" opcode))) ;; 8
              (#x36 (insert (format "0x%02x: SWAP (HL) \n" opcode)));; 16

              ;; RLC n - Rotate n left. Old bit 7 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x07 (insert (format "0x%02x: RLC A\n" opcode))) ;; 8
              (#x00 (insert (format "0x%02x: RLC B\n" opcode))) ;; 8
              (#x01 (insert (format "0x%02x: RLC C\n" opcode))) ;; 8
              (#x02 (insert (format "0x%02x: RLC D\n" opcode))) ;; 8
              (#x03 (insert (format "0x%02x: RLC E\n" opcode))) ;; 8
              (#x04 (insert (format "0x%02x: RLC H\n" opcode))) ;; 8
              (#x05 (insert (format "0x%02x: RLC L\n" opcode))) ;; 8
              (#x06 (insert (format "0x%02x: RLC (HL) \n" opcode)));; 16

              ;; RL n - Rotate n left through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x17 (insert (format "0x%02x: RL A\n" opcode))) ;; 8
              (#x10 (insert (format "0x%02x: RL B\n" opcode))) ;; 8
              (#x11 (insert (format "0x%02x: RL C\n" opcode))) ;; 8
              (#x12 (insert (format "0x%02x: RL D\n" opcode))) ;; 8
              (#x13 (insert (format "0x%02x: RL E\n" opcode))) ;; 8
              (#x14 (insert (format "0x%02x: RL H\n" opcode))) ;; 8
              (#x15 (insert (format "0x%02x: RL L\n" opcode))) ;; 8
              (#x16 (insert (format "0x%02x: RL (HL) \n" opcode)));; 16

              ;; RRC n -  Rotate n right. Old bit 0 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x0F (insert (format "0x%02x: RRC A\n" opcode))) ;; 8
              (#x08 (insert (format "0x%02x: RRC B\n" opcode))) ;; 8
              (#x09 (insert (format "0x%02x: RRC C\n" opcode))) ;; 8
              (#x0A (insert (format "0x%02x: RRC D\n" opcode))) ;; 8
              (#x0B (insert (format "0x%02x: RRC E\n" opcode))) ;; 8
              (#x0C (insert (format "0x%02x: RRC H\n" opcode))) ;; 8
              (#x0D (insert (format "0x%02x: RRC L\n" opcode))) ;; 8
              (#x0E (insert (format "0x%02x: RRC (HL) \n" opcode)));; 16

              ;; RR n - Rotate n right through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x1F (insert (format "0x%02x: RR A\n" opcode))) ;; 8
              (#x18 (insert (format "0x%02x: RR B\n" opcode))) ;; 8
              (#x19 (insert (format "0x%02x: RR C\n" opcode))) ;; 8
              (#x1A (insert (format "0x%02x: RR D\n" opcode))) ;; 8
              (#x1B (insert (format "0x%02x: RR E\n" opcode))) ;; 8
              (#x1C (insert (format "0x%02x: RR H\n" opcode))) ;; 8
              (#x1D (insert (format "0x%02x: RR L\n" opcode))) ;; 8
              (#x1E (insert (format "0x%02x: RR (HL) \n" opcode)));; 16

              ;; SLA n - Shift n left into Carry. LSB of n set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x27 (insert (format "0x%02x: SLA A\n" opcode))) ;; 8
              (#x20 (insert (format "0x%02x: SLA B\n" opcode))) ;; 8
              (#x21 (insert (format "0x%02x: SLA C\n" opcode))) ;; 8
              (#x22 (insert (format "0x%02x: SLA D\n" opcode))) ;; 8
              (#x23 (insert (format "0x%02x: SLA E\n" opcode))) ;; 8
              (#x24 (insert (format "0x%02x: SLA H\n" opcode))) ;; 8
              (#x25 (insert (format "0x%02x: SLA L\n" opcode))) ;; 8
              (#x26 (insert (format "0x%02x: SLA (HL) \n" opcode)));; 16

              ;; SRA n - Shift n right into Carry. MSB doesn't change.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x2F (insert (format "0x%02x: SRA A\n" opcode))) ;; 8
              (#x28 (insert (format "0x%02x: SRA B\n" opcode))) ;; 8
              (#x29 (insert (format "0x%02x: SRA C\n" opcode))) ;; 8
              (#x2A (insert (format "0x%02x: SRA D\n" opcode))) ;; 8
              (#x2B (insert (format "0x%02x: SRA E\n" opcode))) ;; 8
              (#x2C (insert (format "0x%02x: SRA H\n" opcode))) ;; 8
              (#x2D (insert (format "0x%02x: SRA L\n" opcode))) ;; 8
              (#x2E (insert (format "0x%02x: SRA (HL) \n" opcode)));; 16

              ;; SRL n - Shift n right into Carry. MSB set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x3F (insert (format "0x%02x: SRL A\n" opcode))) ;; 8
              (#x38 (insert (format "0x%02x: SRL B\n" opcode))) ;; 8
              (#x39 (insert (format "0x%02x: SRL C\n" opcode))) ;; 8
              (#x3A (insert (format "0x%02x: SRL D\n" opcode))) ;; 8
              (#x3B (insert (format "0x%02x: SRL E\n" opcode))) ;; 8
              (#x3C (insert (format "0x%02x: SRL H\n" opcode))) ;; 8
              (#x3D (insert (format "0x%02x: SRL L\n" opcode))) ;; 8
              (#x3E (insert (format "0x%02x: SRL (HL) \n" opcode)));; 16

              ;; Bit Opcodes - Test bit b in register r.
              ;; Flags affected:
              ;; Z - Set if bit b of register r is 0.
              ;; N - Reset.
              ;; H - Set.
              ;; C - Not affected.
              (#x47 (insert (format "0x%02x: BIT b,A\n" opcode))) ;; 8
              (#x40 (insert (format "0x%02x: BIT b,B\n" opcode))) ;; 8
              (#x41 (insert (format "0x%02x: BIT b,C\n" opcode))) ;; 8
              (#x42 (insert (format "0x%02x: BIT b,D\n" opcode))) ;; 8
              (#x43 (insert (format "0x%02x: BIT b,E\n" opcode))) ;; 8
              (#x44 (insert (format "0x%02x: BIT b,H\n" opcode))) ;; 8
              (#x45 (insert (format "0x%02x: BIT b,L\n" opcode))) ;; 8
              (#x46 (insert (format "0x%02x: BIT b,(HL) \n" opcode)));; 16

              ;; SET b,r - Set bit b in register r.
              ;; Flags affected:
              ;; None.
              (#xC7 (insert (format "0x%02x: SET b,A\n" opcode))) ;; 8
              (#xC0 (insert (format "0x%02x: SET b,B\n" opcode))) ;; 8
              (#xC1 (insert (format "0x%02x: SET b,C\n" opcode))) ;; 8
              (#xC2 (insert (format "0x%02x: SET b,D\n" opcode))) ;; 8
              (#xC3 (insert (format "0x%02x: SET b,E\n" opcode))) ;; 8
              (#xC4 (insert (format "0x%02x: SET b,H\n" opcode))) ;; 8
              (#xC5 (insert (format "0x%02x: SET b,L\n" opcode))) ;; 8
              (#xC6 (insert (format "0x%02x: SET b,(HL) \n" opcode)));; 16

              ;; RES b,r - Reset bit b in register r.
              ;; Flags affected:
              ;; None.
              (#x87 (insert (format "0x%02x: RES b,A\n" opcode))) ;; 8
              (#x80 (insert (format "0x%02x: RES b,B\n" opcode))) ;; 8
              (#x81 (insert (format "0x%02x: RES b,C\n" opcode))) ;; 8
              (#x82 (insert (format "0x%02x: RES b,D\n" opcode))) ;; 8
              (#x83 (insert (format "0x%02x: RES b,E\n" opcode))) ;; 8
              (#x84 (insert (format "0x%02x: RES b,H\n" opcode))) ;; 8
              (#x85 (insert (format "0x%02x: RES b,L\n" opcode))) ;; 8
              (#x86 (insert (format "0x%02x: RES b,(HL) \n" opcode)));; 16
              (otherwise (insert (format "Unimplemented BC opcode 0x%x\n" opcode)))
              )))


    ;; JP nn - Jump to address nn.
    (#xC3 (insert (format "0x%02x: JP $%04x\n" opcode (eboy-get-short))) ;; 12
          (setq eboy-pc (1- (eboy-get-short))) ;; Compensate for the pc +1 that is done with each instruction.
          ;;(eboy-inc-pc 2)
          )

    ;; JP cc,nn - Jump to address n if following condition is true:
    ;;   cc = NZ, Jump if Z flag is reset.
    ;;   cc = Z, Jump if Z flag is set.
    ;;   cc = NC, Jump if C flag is reset.
    ;;   cc = C, Jump if C flag is set.
    ;; nn = two byte immediate value. (LS byte first.)
    (#xC2 (insert (format "0x%02x: JP NZ,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xCA (insert (format "0x%02x: JP Z,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xD2 (insert (format "0x%02x: JP NC,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xDA (insert (format "0x%02x: JP C,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12

    ;; JP (HL) - Jump to address contained in HL.
    (#xE9 (insert (format "0x%02x: JP (HL) \n" opcode)) (assert nil t "unimplemented opcode")) ;; 4

    ;; JR n - Add n to current address and jump to it.
    ;; nn = one byte signed immediate value
    (#x18 (insert (format "0x%02x: JR n \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; JR cc,n - If following condition is true then add n to current address and jump to it:
    ;;  n = one byte signed immediate value
    ;;  cc = NZ, Jump if Z flag is reset.
    ;;  cc = Z, Jump if Z flag is set.
    ;;  cc = NC, Jump if C flag is reset.
    ;;  cc = C, Jump if C flag is set.
    (#x20 (insert (format "0x%02x: JR NZ,* (%02d)\n" opcode (eboy-byte-to-signed (eboy-get-byte))))
          (if (null (eboy-get-flag flags :Z))
              (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte)))))
          (incf eboy-pc)) ;; 8
    (#x28 (insert (format "0x%02x: JR Z,* \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x30 (insert (format "0x%02x: JR NC,* \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#x38 (insert (format "0x%02x: JR C,* \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8


      ;;; Calls

    ;; CALL nn - Push address of next instruction onto stack and then jump to address nn.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xCD (insert (format "0x%02x: CALL nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12

    ;; CALL cc,nn - Call address n if following condition is true:
    ;;  cc = NZ, Call if Z flag is reset.
    ;;  cc = Z, Call if Z flag is set.
    ;;  cc = NC, Call if C flag is reset.
    ;;  cc = C, Call if C flag is set.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xC4 (insert (format "0x%02x: CALL NZ,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xCC (insert (format "0x%02x: CALL Z,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xD4 (insert (format "0x%02x: CALL NC,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12
    (#xDC (insert (format "0x%02x: CALL C,nn  \n" opcode)) (assert nil t "unimplemented opcode"));; 12


      ;;; Restarts

    ;; RST n - Push present address onto stack. Jump to address $0000 + n.
    ;;  n = $00,$08,$10,$18,$20,$28,$30,$38
    (#xC7 (insert (format "0x%02x: RST 00H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xCF (insert (format "0x%02x: RST 08H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xD7 (insert (format "0x%02x: RST 10H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xDF (insert (format "0x%02x: RST 18H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xE7 (insert (format "0x%02x: RST 20H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xEF (insert (format "0x%02x: RST 28H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xF7 (insert (format "0x%02x: RST 30H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32
    (#xFF (insert (format "0x%02x: RST 38H  \n" opcode)) (assert nil t "unimplemented opcode"));; 32

      ;;; Returns

    ;; RET - Pop two bytes from stack & jump to that address.
    (#xC9 (insert (format "0x%02x: RET -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; RET cc - Return if following condition is true:
    ;;  cc = NZ, Return if Z flag is reset.
    ;;  cc = Z, Return if Z flag is set.
    ;;  cc = NC, Return if C flag is reset.
    ;;  cc = C, Return if C flag is set.
    (#xC0 (insert (format "0x%02x: RET NZ \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#xC8 (insert (format "0x%02x: RET Z \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#xD0 (insert (format "0x%02x: RET NC \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8
    (#xD8 (insert (format "0x%02x: RET C \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; RETI - Pop two bytes from stack & jump to that address then enable interrupts.
    (#xD9 (insert (format "0x%02x: RETI -/- \n" opcode)) (assert nil t "unimplemented opcode")) ;; 8

    ;; Non existant opcodes
    (#xD3 (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xDB (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xDD (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xE3 (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xE4 (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xEB (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xEC (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xED (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xF4 (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xFC (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (#xFD (insert (format "Non existant opcode: 0x%02x\n" opcode)) (assert nil t))
    (otherwise (insert (format "Unimplemented opcode 0x%x\n" opcode)) (assert nil t))
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
  (setq eboy-sp eboy-sp-initial-value)
  (setq eboy-debug-nr-instructions 0)
  ;;(eboy-reset-CPU-flags)
  (eboy-init-registers)
  (eboy-init-memory)
  (switch-to-buffer "*eboy*")
  (erase-buffer)
  (insert (format "Load rom: %s\n" eboy-rom-filename))
  (insert (format "Rom size: %d bytes\n" eboy-rom-size))
  (insert (format "Rom title: %s\n" (eboy-rom-title)))
  (eboy-debug-dump-memory #xFF00 #xFFFF)

  ;; loop
  (let ((flags (make-bool-vector 4 nil)))
     ;; init flags 0xB0
    (eboy-set-flag flags :Z t)
    (eboy-set-flag flags :N nil)
    (eboy-set-flag flags :H t)
    (eboy-set-flag flags :C t)

    (while (and (< eboy-pc eboy-rom-size) (< eboy-debug-nr-instructions 50000))
      (insert (format "%2d: " eboy-debug-nr-instructions))
      (eboy-process-opcode (aref eboy-rom eboy-pc) flags)
      (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1))
      )
    )
  )

(eboy-load-rom)


(provide 'eboy)
;;; eboy.el ends here

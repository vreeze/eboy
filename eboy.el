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
(defvar eboy-interrupt-enabled nil "Interupts enabled flag.")

;;(defvar eboy-flags (make-bool-vector 4 t) "The flags Z(Zero) S(Negative) H(Halve Carry) and C(Carry).")
(defvar eboy-debug-nr-instructions nil "Number of instructions executed.")
(defvar eboy-debug-1 nil "Enable debugging info.")
(defvar eboy-debug-2 t "Enable debugging info.")


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

(defun eboy-log (logstring)
  "Log string LOGSTRING."
  (if eboy-debug-2 (insert logstring))
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

;; (defun eboy-set-flags (flags new-flags)
;;   "Set FLAGS to NEW-FLAGS."
;;   (eboy-set-flag flags (caar new-flags) (cadar new-flags))
;;   ;;(insert (format "%s %s" (caar new-flags) (cadar new-flags)) )
;;   (unless (null (cdr new-flags))
;;     (eboy-set-flags flags (cdr new-flags)))
;;   )

(defun eboy-set-flag (flags flag state)
  "Set FLAG in FLAGS to STATE."
  (ecase flag
    (:Z (aset flags 0 state))
    (:N (aset flags 1 state))
    (:H (aset flags 2 state))
    (:C (aset flags 3 state))
    )
  )

;; (let ((flags  (make-vector 4 t)))
;;   (eboy-debug-print-flags flags)
;;   (eboy-set-flag flags :C (logand #x00 #x01))
;;   (eboy-debug-print-flags flags))
;Flags; Z:t N:t H:t C:t
;Flags; Z:t N:t H:t C:0


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
  (if eboy-debug-1 (eboy-debug-print-cpu-state flags))
  (insert (format "\npc: 0x%x, opcode: 0x%02x" eboy-pc opcode))
  (cl-case opcode
    (#x00 (eboy-log " NOP"))

    ;; LD nn,n
    (#x06
     ;;(eboy-debug-unimplemented-opcode 6)
     (eboy-log (format " LD B, #0x%02x" (eboy-get-byte)))
     (setq eboy-r-B (eboy-get-byte))
     (eboy-inc-pc 1))
    (#x0E (eboy-log (format " LD C, #0x%02x" (eboy-get-byte)))
          (setq eboy-r-C (eboy-get-byte))
          (eboy-inc-pc 1))
    (#x16 (eboy-log (format " LD D, #0x%02x" (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x1E (eboy-log (format " LD E, #0x%02x" (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x26 (eboy-log (format " LD H, #0x%02x" (eboy-get-byte)))
          (eboy-inc-pc 1))
    (#x2E (eboy-log (format " LD L, #0x%02x" (eboy-get-byte)))
          (eboy-inc-pc 1))

    ;; ;; LD r1,r2
    (#x7F (eboy-log (format " LD A,A")) (assert nil t "unimplemented opcode"))
    (#x78 (eboy-log (format " LD A,B")) (assert nil t "unimplemented opcode"))
    (#x79 (eboy-log (format " LD A,C")) (assert nil t "unimplemented opcode"))
    (#x7A (eboy-log (format " LD A,D")) (assert nil t "unimplemented opcode"))
    (#x7B (eboy-log (format " LD A,E")) (assert nil t "unimplemented opcode"))
    (#x7C (eboy-log (format " LD A,H")) (assert nil t "unimplemented opcode"))
    (#x7D (eboy-log (format " LD A,L")) (assert nil t "unimplemented opcode"))
    (#x7E (eboy-log (format " LD A,(HL)")) (assert nil t "unimplemented opcode"))
    (#x40 (eboy-log (format " LD B,B")) (assert nil t "unimplemented opcode"))
    (#x41 (eboy-log (format " LD B,C")) (assert nil t "unimplemented opcode"))
    (#x42 (eboy-log (format " LD B,D")) (assert nil t "unimplemented opcode"))
    (#x43 (eboy-log (format " LD B,E")) (assert nil t "unimplemented opcode"))
    (#x44 (eboy-log (format " LD B,H")) (assert nil t "unimplemented opcode"))
    (#x45 (eboy-log (format " LD B,L")) (assert nil t "unimplemented opcode"))
    (#x46 (eboy-log (format " LD B,(HL)")) (assert nil t "unimplemented opcode"))
    (#x48 (eboy-log (format " LD C,B")) (assert nil t "unimplemented opcode"))
    (#x49 (eboy-log (format " LD C,C")) (assert nil t "unimplemented opcode"))
    (#x4A (eboy-log (format " LD C,D")) (assert nil t "unimplemented opcode"))
    (#x4B (eboy-log (format " LD C,E")) (assert nil t "unimplemented opcode"))
    (#x4C (eboy-log (format " LD C,H")) (assert nil t "unimplemented opcode"))
    (#x4D (eboy-log (format " LD C,L")) (assert nil t "unimplemented opcode"))
    (#x4E (eboy-log (format " LD C,(HL)")) (assert nil t "unimplemented opcode"))
    (#x50 (eboy-log (format " LD D,B")) (assert nil t "unimplemented opcode"))
    (#x51 (eboy-log (format " LD D,C")) (assert nil t "unimplemented opcode"))
    (#x52 (eboy-log (format " LD D,D")) (assert nil t "unimplemented opcode"))
    (#x53 (eboy-log (format " LD D,E")) (assert nil t "unimplemented opcode"))
    (#x54 (eboy-log (format " LD D,H")) (assert nil t "unimplemented opcode"))
    (#x55 (eboy-log (format " LD D,L")) (assert nil t "unimplemented opcode"))
    (#x56 (eboy-log (format " LD D,(HL)")) (assert nil t "unimplemented opcode"))
    (#x58 (eboy-log (format " LD E,B")) (assert nil t "unimplemented opcode"))
    (#x59 (eboy-log (format " LD E,C")) (assert nil t "unimplemented opcode"))
    (#x5A (eboy-log (format " LD E,D")) (assert nil t "unimplemented opcode"))
    (#x5B (eboy-log (format " LD E,E")) (assert nil t "unimplemented opcode"))
    (#x5C (eboy-log (format " LD E,H")) (assert nil t "unimplemented opcode"))
    (#x5D (eboy-log (format " LD E,L")) (assert nil t "unimplemented opcode"))
    (#x5E (eboy-log (format " LD E,(HL)")) (assert nil t "unimplemented opcode"))
    (#x60 (eboy-log (format " LD H,B")) (assert nil t "unimplemented opcode"))
    (#x61 (eboy-log (format " LD H,C")) (assert nil t "unimplemented opcode"))
    (#x62 (eboy-log (format " LD H,D")) (assert nil t "unimplemented opcode"))
    (#x63 (eboy-log (format " LD H,E")) (assert nil t "unimplemented opcode"))
    (#x64 (eboy-log (format " LD H,H")) (assert nil t "unimplemented opcode"))
    (#x65 (eboy-log (format " LD H,L")) (assert nil t "unimplemented opcode"))
    (#x66 (eboy-log (format " LD H,(HL)")) (assert nil t "unimplemented opcode"))
    (#x68 (eboy-log (format " LD L,B")) (assert nil t "unimplemented opcode"))
    (#x69 (eboy-log (format " LD L,C")) (assert nil t "unimplemented opcode"))
    (#x6A (eboy-log (format " LD L,D")) (assert nil t "unimplemented opcode"))
    (#x6B (eboy-log (format " LD L,E")) (assert nil t "unimplemented opcode"))
    (#x6C (eboy-log (format " LD L,H")) (assert nil t "unimplemented opcode"))
    (#x6D (eboy-log (format " LD L,L")) (assert nil t "unimplemented opcode"))
    (#x6E (eboy-log (format " LD L,(HL)")) (assert nil t "unimplemented opcode"))
    (#x70 (eboy-log (format " LD (HL),B")) (assert nil t "unimplemented opcode"))
    (#x71 (eboy-log (format " LD (HL),C")) (assert nil t "unimplemented opcode"))
    (#x72 (eboy-log (format " LD (HL),D")) (assert nil t "unimplemented opcode"))
    (#x73 (eboy-log (format " LD (HL),E")) (assert nil t "unimplemented opcode"))
    (#x74 (eboy-log (format " LD (HL),H")) (assert nil t "unimplemented opcode"))
    (#x75 (eboy-log (format " LD (HL),L")) (assert nil t "unimplemented opcode"))
    (#x36 (eboy-log (format " LD (HL),n")) (assert nil t "unimplemented opcode"))

    ;; LD A,n
    (#x0A (eboy-log (format " LD A,(BC)")) (assert nil t "unimplemented opcode"))
    (#x1A (eboy-log (format " LD A,(DE)")) (assert nil t "unimplemented opcode"))
    (#xFA (eboy-log (format " LD A,(nn)")) (assert nil t "unimplemented opcode"))
    (#x3E (eboy-log (format " LD A,#0x%02x" (eboy-get-byte)))
          (setq eboy-r-A (eboy-get-byte))
          (eboy-inc-pc 1))

    ;; LD n,A - Put value A into n.
    ;; n = A,B,C,D,E,H,L,(BC),(DE),(HL),(nn)
    ;; nn = two byte immediate value. (LS byte first.)
    (#x47 (eboy-log (format " LD B,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x4F (eboy-log (format " LD C,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x57 (eboy-log (format " LD D,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x5F (eboy-log (format " LD E,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x67 (eboy-log (format " LD H,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x6F (eboy-log (format " LD L,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x02 (eboy-log (format " LD (BC),A") (assert nil t "unimplemented opcode"))
          ;;(eboy-set-r-BC eboy-r-A) maybe (BC) means memory address?
          )                   ;; 8
    (#x12 (eboy-log (format " LD (DE),A")) (assert nil t "unimplemented opcode"))                   ;; 8
    (#x77 (eboy-log (format " LD (HL),A")) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xEA (eboy-log (format " LD (nn),A")) (assert nil t "unimplemented opcode"))                   ;; 16

    ;; Put value at address $FF00 + register C into A.
    (#xF2 (eboy-log (format " LD A,($FF00+C)")) (assert nil t "unimplemented opcode") )
    ;; Put A into address $FF00 + register C.
    (#xE2 (eboy-log (format " LD ($FF00+C),A")) (assert nil t "unimplemented opcode"))
    ;; Put value at address HL into A. Decrement HL.
    (#x3A (eboy-log (format " LD A,(HLD)")) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address HL. Decrement HL.
    (#x32 (eboy-log (format " LD (HLD),A"))
          (eboy-write-byte-to-memory (eboy-get-r-HL) eboy-r-A)
          (eboy-set-r-HL (1- (eboy-get-r-HL)))
          ) ;; 8
    ;; Put value at address HL into A. Increment HL.
    (#x2A (eboy-log (format " LD A,(HLI)")) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address HL. Increment HL.
    (#x22 (eboy-log (format " LD (HLI),A")) (assert nil t "unimplemented opcode"))
    ;; Put A into memory address $FF00+n
    (#xE0 (eboy-log (format " LD ($FF00+0x%02x),A (0x%02x)" (eboy-get-byte) eboy-r-A))
          (eboy-write-byte-to-memory (+ #xFF00 (eboy-get-byte)) eboy-r-A)
          (eboy-inc-pc 1)) ;; 12
    ;; Put memory address $FF00+n into A.
    (#xF0 (eboy-log (format " LD A,($FF00+0x%02x) (0x%02x)" (eboy-get-byte) (eboy-read-byte-from-memory (+ #xFF00 (eboy-get-byte)))))
          (setq eboy-r-A (eboy-read-byte-from-memory (+ #xFF00 (eboy-get-byte))))
          (eboy-inc-pc 1)) ;; 12

    ;; 16 bit loads, nn = 16 bit immediate value
    (#x01 (eboy-log (format " LD BC, $%04x" (eboy-get-short)))(eboy-inc-pc 2) (assert nil t "unimplemented opcode")) ;; 12
    (#x11 (eboy-log (format " LD DE, $%04x" (eboy-get-short))) (assert nil t "unimplemented opcode")) ;; 12
    (#x21 (eboy-log (format " LD HL, $%04x" (eboy-get-short)))
          (eboy-set-r-HL (eboy-get-short))
          (eboy-inc-pc 2)
          ) ;; 12
    (#x31 (eboy-log (format " LD SP, $%04x" (eboy-get-short))) (assert nil t "unimplemented opcode")) ;; 12

    (#xF9 (eboy-log (format " LD SP,HL")) (assert nil t "unimplemented opcode"))

    ;; Put SP + n effective address into HL, n = one byte signed immediate value.
    ;; Flags affected:
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xF8 (eboy-log (format " LDHL SP,n")) (assert nil t "unimplemented opcode"))
    ;; Put Stack Pointer (SP) at address n. n = two byte immediate address
    (#x08 (eboy-log (format " LD (nn),SP")) (assert nil t "unimplemented opcode"))

    ;; Push register pair nn onto stack.
    ;; Decrement Stack Pointer (SP) twice.
    (#xF5 (eboy-log (format " PUSH AF")) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xC5 (eboy-log (format " PUSH BC")) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xD5 (eboy-log (format " PUSH DE")) (assert nil t "unimplemented opcode"))                      ;; 16
    (#xE5 (eboy-log (format " PUSH HL")) (assert nil t "unimplemented opcode"))                      ;; 16

    ;; Pop two bytes off stack into register pair nn.
    ;; Increment Stack Pointer (SP) twice.
    (#xF1 (eboy-log (format " POP AF")) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xC1 (eboy-log (format " POP BC")) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xD1 (eboy-log (format " POP DE")) (assert nil t "unimplemented opcode"))                      ;; 12
    (#xE1 (eboy-log (format " POP HL")) (assert nil t "unimplemented opcode"))                      ;; 12


    ;; ADD A,n    -    Add n to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x87 (eboy-log (format " ADD A,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x80 (eboy-log (format " ADD A,B")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x81 (eboy-log (format " ADD A,C")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x82 (eboy-log (format " ADD A,D")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x83 (eboy-log (format " ADD A,E")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x84 (eboy-log (format " ADD A,H")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x85 (eboy-log (format " ADD A,L")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x86 (eboy-log (format " ADD A,(HL)")) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xC6 (eboy-log (format " ADD A,#"))
          (setq eboy-r-A (+ eboy-r-A (eboy-get-byte))))                      ;; 8

    ;;ADC A,n    -   Add n + Carry flag to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x8F (eboy-log (format " ADC A,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x88 (eboy-log (format " ADC A,B")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x89 (eboy-log (format " ADC A,C")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8A (eboy-log (format " ADC A,D")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8B (eboy-log (format " ADC A,E")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8C (eboy-log (format " ADC A,H")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8D (eboy-log (format " ADC A,L")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x8E (eboy-log (format " ADC A,(HL)")) (assert nil t "unimplemented opcode"))                   ;; 8
    (#xCE (eboy-log (format " ADC A,#")) (assert nil t "unimplemented opcode"))                      ;; 8

    ;; SUB n - Subtract n from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x97 (eboy-log (format " SUB A")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x90 (eboy-log (format " SUB B")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x91 (eboy-log (format " SUB C")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x92 (eboy-log (format " SUB D")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x93 (eboy-log (format " SUB E")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x94 (eboy-log (format " SUB H")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x95 (eboy-log (format " SUB L")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#x96 (eboy-log (format " SUB (HL)")) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xD6 (eboy-log (format " SUB #")) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; SBC A,n - Subtract n + Carry flag from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x9F (eboy-log (format " SBC A,A")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x98 (eboy-log (format " SBC A,B")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x99 (eboy-log (format " SBC A,C")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9A (eboy-log (format " SBC A,D")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9B (eboy-log (format " SBC A,E")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9C (eboy-log (format " SBC A,H")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9D (eboy-log (format " SBC A,L")) (assert nil t "unimplemented opcode"))                      ;; 4
    (#x9E (eboy-log (format " SBC A,(HL)")) (assert nil t "unimplemented opcode"))                   ;; 8
    ;;(?? (eboy-log (format " SBC A,#")))                      ;; ?

    ;; AND n - Logically AND n with A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set.
    ;; C - Reset.
    (#xA7 (eboy-log (format " AND A")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA0 (eboy-log (format " AND B")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA1 (eboy-log (format " AND C")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA2 (eboy-log (format " AND D")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA3 (eboy-log (format " AND E")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA4 (eboy-log (format " AND H")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA5 (eboy-log (format " AND L")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xA6 (eboy-log (format " AND (HL)")) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xE6 (eboy-log (format " AND #")) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; OR n - Logical OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xB7 (eboy-log (format " OR A")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB0 (eboy-log (format " OR B")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB1 (eboy-log (format " OR C")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB2 (eboy-log (format " OR D")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB3 (eboy-log (format " OR E")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB4 (eboy-log (format " OR H")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB5 (eboy-log (format " OR L")) (assert nil t "unimplemented opcode"))                        ;; 4
    (#xB6 (eboy-log (format " OR (HL)")) (assert nil t "unimplemented opcode"))                     ;; 8
    (#xF6 (eboy-log (format " OR #")) (assert nil t "unimplemented opcode"))                        ;; 8

    ;; XOR n - Logical exclusive OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xAF (eboy-log (format " XOR A "))
          (setq eboy-r-A (logxor eboy-r-A eboy-r-A))
          (if (zerop eboy-r-A)
              (eboy-set-flag flags :Z t))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil)
          ) ;; 4
    (#xA8 (eboy-log (format " XOR B ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xA9 (eboy-log (format " XOR C ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xAA (eboy-log (format " XOR D ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xAB (eboy-log (format " XOR E ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xAC (eboy-log (format " XOR H ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xAD (eboy-log (format " XOR L ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xAE (eboy-log (format " XOR (HL) ")) (assert nil t "unimplemented opcode")) ;; 8
    (#xEE (eboy-log (format " XOR * ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; CP n - Compare A with n. This is basically an A - n subtraction instruction but the results are thrown away.
    ;; Flags affected:
    ;; Z - Set if result is zero. (Set if A = n.)
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set for no borrow. (Set if A < n.)
    (#xBF (eboy-log (format " CP A ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xB8 (eboy-log (format " CP B ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xB9 (eboy-log (format " CP C ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xBA (eboy-log (format " CP D ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xBB (eboy-log (format " CP E ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xBC (eboy-log (format " CP H ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xBD (eboy-log (format " CP L ")) (assert nil t "unimplemented opcode")) ;; 4
    (#xBE (eboy-log (format " CP (HL) ")) (assert nil t "unimplemented opcode")) ;; 8
    (#xFE (eboy-log (format " CP # (0x%02x)" (eboy-get-byte)))
          (let ((n (eboy-get-byte)))
            (eboy-set-flag flags :Z (= eboy-r-A n))
            (eboy-set-flag flags :N t)
            (eboy-set-flag flags :H (> (logand (- eboy-r-A n) #x0F) (logand eboy-r-A #x0F)))
            (eboy-set-flag flags :C (< eboy-r-A n))
            (eboy-inc-pc 1)
            )) ;; 8

    ;; INC n - Increment register n.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Not affected.
    (#x3C (eboy-log (format " INC A ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x04 (eboy-log (format " INC B ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x0C (eboy-log (format " INC C "))
          (eboy-add-byte eboy-r-C 1)
          (if (= (logand eboy-r-C #xff) 0)
              (progn (eboy-set-flag flags :Z t)
                     (setq eboy-r-C 0)))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-r-C #xf) (logand (1- eboy-r-C) #xf)))
          ) ;; 4
    (#x14 (eboy-log (format " INC D ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x1C (eboy-log (format " INC E ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x24 (eboy-log (format " INC H ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x2C (eboy-log (format " INC L ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x34 (eboy-log (format " INC (HL)  ")) (assert nil t "unimplemented opcode"));; 12

    ;; DEC n - Decrement register n.
    ;; Flags affected:
    ;; Z - Set if reselt is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Not affected.
    (#x3D (eboy-log (format " DEC A ")) (assert nil t "unimplemented opcode")) ;; 4
    (#x05 (eboy-log (format " DEC B "))
          ;; (eboy-add-byte eboy-r-B -1)
          ;; (eboy-set-flag flags :Z (= eboy-r-B 0))
          ;; (eboy-set-flag flags :N t)
          ;; (eboy-set-flag flags :H (= (logand eboy-r-B #xF) #xF))
          (eboy-dec eboy-r-B flags)
          ) ;; 4
    (#x0D (eboy-log (format " DEC C ")) (eboy-dec eboy-r-C flags)) ;; 4
    (#x15 (eboy-log (format " DEC D ")) (eboy-dec eboy-r-D flags)) ;; 4
    (#x1D (eboy-log (format " DEC E ")) (eboy-dec eboy-r-E flags)) ;; 4
    (#x25 (eboy-log (format " DEC H ")) (eboy-dec eboy-r-H flags)) ;; 4
    (#x2D (eboy-log (format " DEC L ")) (eboy-dec eboy-r-L flags)) ;; 4
    (#x35 (eboy-log (format " DEC (HL)  ")) (assert nil t "unimplemented opcode"));; 12

      ;;; 16-Bit Arithmetic
    ;;
    ;; ADD HL,n - Add n to HL.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Set if carry from bit 11.
    ;; C - Set if carry from bit 15.
    (#x09 (eboy-log (format " ADD HL,BC ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x19 (eboy-log (format " ADD HL,DE ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x29 (eboy-log (format " ADD HL,HL ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x39 (eboy-log (format " ADD HL,SP ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; ADD SP,n - Add n to Stack Pointer (SP).
    ;; Flags affected:
    ;; None.
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xE8 (eboy-log (format " ADD SP,#  ")) (assert nil t "unimplemented opcode"));; 16

    ;; INC nn - Increment register nn.
    ;; Flags affected:
    (#x03 (eboy-log (format " INC BC ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x13 (eboy-log (format " INC DE ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x23 (eboy-log (format " INC HL ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x33 (eboy-log (format " INC SP ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; DEC nn - Decrement register nn.
    ;; Flags affected:
    ;; None.
    (#x0B (eboy-log (format " DEC BC ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x1B (eboy-log (format " DEC DE ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x2B (eboy-log (format " DEC HL ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x3B (eboy-log (format " DEC SP ")) (assert nil t "unimplemented opcode")) ;; 8

      ;;; Miscellaneous



    ;; DAA - Decimal adjust register A.
    ;; This instruction adjusts register A so that the correct representation of Binary Coded Decimal (BCD) is obtained.
    ;; Flags affected:
    ;; Z - Set if register A is zero.
    ;; N - Not affected.
    ;; H - Reset.
    ;; C - Set or reset according to operation.
    (#x27 (eboy-log (format " DAA -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; CPL - Complement A register. (Flip all bits.)
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Set.
    ;; H - Set.
    ;; C - Not affected.
    (#x2F (eboy-log (format " CPL -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; CCF - Complement carry flag.
    ;; If C flag is set, then reset it.
    ;; If C flag is reset, then set it.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Complemented.
    (#x3F (eboy-log (format " CCF -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; SCF - Set Carry flag.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Set.
    (#x37 (eboy-log (format " SCF -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; HALT - Power down CPU until an interrupt occurs. Use this when ever possible to reduce energy consumption.
    (#x76 (eboy-log (format " HALT -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; STOP - Halt CPU & LCD display until button pressed.
    (#x10 (eboy-log (format " STOP -/- 10 ")) (assert nil t "unimplemented opcode")) ;; 4
    ;; This is a 2 byte opcode: 0x10 00

    ;; DI - This instruction disables interrupts but not immediately. Interrupts are disabled after instruction after DI is executed.
    ;; Flags affected:
    ;; None.
    (#xF3 (eboy-log (format " DI -/- ")) (setq eboy-interrupt-enabled nil)) ;; 4
    ;; EI - Enable interrupts. This intruction enables interrupts but not immediately. Interrupts are enabled after instruction after EI is executed.
    ;; Flags affected:
    ;; None.
    (#xFB (eboy-log (format " EI -/- ")) (setq eboy-interrupt-enabled t)) ;; 4

      ;;; Rotates & Shift

    ;; RLCA - Rotate A left. Old bit 7 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x07 (eboy-log (format " RLCA -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; RLA - Rotate A left through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x17 (eboy-log (format " RLA -/- ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; RRCA - Rotate A right. Old bit 0 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x0F (eboy-log (format " RRCA -/- "))
          (insert (format " %s " (logand eboy-r-A #x01)) )
          (eboy-set-flag flags :C (= (logand eboy-r-A #x01) 1))
          (setq eboy-r-A (logand (logior (lsh eboy-r-A 1) (lsh eboy-r-A -7)) #xff))
          (eboy-set-flag flags :Z (= eboy-r-A 0))
          ) ;; 4

    ;; RRA - Rotate A right through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x1F (eboy-log (format " RRA -/- ")) (assert nil t "unimplemented opcode")) ;; 4


    (#xCB (eboy-log (format " 2byte opcode CB: " ) (assert nil t "unimplemented opcode"))
          (let ((bc-opcode (eboy-get-byte)))
            (eboy-inc-pc 1)
            (cl-case bc-opcode
              ;; SWAP n - Swap upper & lower nibles of n.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Reset.
              (#x37 (eboy-log (format " SWAP A"))) ;; 8
              (#x30 (eboy-log (format " SWAP B"))) ;; 8
              (#x31 (eboy-log (format " SWAP C"))) ;; 8
              (#x32 (eboy-log (format " SWAP D"))) ;; 8
              (#x33 (eboy-log (format " SWAP E"))) ;; 8
              (#x34 (eboy-log (format " SWAP H"))) ;; 8
              (#x35 (eboy-log (format " SWAP L"))) ;; 8
              (#x36 (eboy-log (format " SWAP (HL) ")));; 16

              ;; RLC n - Rotate n left. Old bit 7 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x07 (eboy-log (format " RLC A"))) ;; 8
              (#x00 (eboy-log (format " RLC B"))) ;; 8
              (#x01 (eboy-log (format " RLC C"))) ;; 8
              (#x02 (eboy-log (format " RLC D"))) ;; 8
              (#x03 (eboy-log (format " RLC E"))) ;; 8
              (#x04 (eboy-log (format " RLC H"))) ;; 8
              (#x05 (eboy-log (format " RLC L"))) ;; 8
              (#x06 (eboy-log (format " RLC (HL) ")));; 16

              ;; RL n - Rotate n left through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x17 (eboy-log (format " RL A"))) ;; 8
              (#x10 (eboy-log (format " RL B"))) ;; 8
              (#x11 (eboy-log (format " RL C"))) ;; 8
              (#x12 (eboy-log (format " RL D"))) ;; 8
              (#x13 (eboy-log (format " RL E"))) ;; 8
              (#x14 (eboy-log (format " RL H"))) ;; 8
              (#x15 (eboy-log (format " RL L"))) ;; 8
              (#x16 (eboy-log (format " RL (HL) ")));; 16

              ;; RRC n -  Rotate n right. Old bit 0 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x0F (eboy-log (format " RRC A"))) ;; 8
              (#x08 (eboy-log (format " RRC B"))) ;; 8
              (#x09 (eboy-log (format " RRC C"))) ;; 8
              (#x0A (eboy-log (format " RRC D"))) ;; 8
              (#x0B (eboy-log (format " RRC E"))) ;; 8
              (#x0C (eboy-log (format " RRC H"))) ;; 8
              (#x0D (eboy-log (format " RRC L"))) ;; 8
              (#x0E (eboy-log (format " RRC (HL) ")));; 16

              ;; RR n - Rotate n right through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x1F (eboy-log (format " RR A"))) ;; 8
              (#x18 (eboy-log (format " RR B"))) ;; 8
              (#x19 (eboy-log (format " RR C"))) ;; 8
              (#x1A (eboy-log (format " RR D"))) ;; 8
              (#x1B (eboy-log (format " RR E"))) ;; 8
              (#x1C (eboy-log (format " RR H"))) ;; 8
              (#x1D (eboy-log (format " RR L"))) ;; 8
              (#x1E (eboy-log (format " RR (HL) ")));; 16

              ;; SLA n - Shift n left into Carry. LSB of n set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x27 (eboy-log (format " SLA A"))) ;; 8
              (#x20 (eboy-log (format " SLA B"))) ;; 8
              (#x21 (eboy-log (format " SLA C"))) ;; 8
              (#x22 (eboy-log (format " SLA D"))) ;; 8
              (#x23 (eboy-log (format " SLA E"))) ;; 8
              (#x24 (eboy-log (format " SLA H"))) ;; 8
              (#x25 (eboy-log (format " SLA L"))) ;; 8
              (#x26 (eboy-log (format " SLA (HL) ")));; 16

              ;; SRA n - Shift n right into Carry. MSB doesn't change.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x2F (eboy-log (format " SRA A"))) ;; 8
              (#x28 (eboy-log (format " SRA B"))) ;; 8
              (#x29 (eboy-log (format " SRA C"))) ;; 8
              (#x2A (eboy-log (format " SRA D"))) ;; 8
              (#x2B (eboy-log (format " SRA E"))) ;; 8
              (#x2C (eboy-log (format " SRA H"))) ;; 8
              (#x2D (eboy-log (format " SRA L"))) ;; 8
              (#x2E (eboy-log (format " SRA (HL) ")));; 16

              ;; SRL n - Shift n right into Carry. MSB set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x3F (eboy-log (format " SRL A"))) ;; 8
              (#x38 (eboy-log (format " SRL B"))) ;; 8
              (#x39 (eboy-log (format " SRL C"))) ;; 8
              (#x3A (eboy-log (format " SRL D"))) ;; 8
              (#x3B (eboy-log (format " SRL E"))) ;; 8
              (#x3C (eboy-log (format " SRL H"))) ;; 8
              (#x3D (eboy-log (format " SRL L"))) ;; 8
              (#x3E (eboy-log (format " SRL (HL) ")));; 16

              ;; Bit Opcodes - Test bit b in register r.
              ;; Flags affected:
              ;; Z - Set if bit b of register r is 0.
              ;; N - Reset.
              ;; H - Set.
              ;; C - Not affected.
              (#x47 (eboy-log (format " BIT b,A"))) ;; 8
              (#x40 (eboy-log (format " BIT b,B"))) ;; 8
              (#x41 (eboy-log (format " BIT b,C"))) ;; 8
              (#x42 (eboy-log (format " BIT b,D"))) ;; 8
              (#x43 (eboy-log (format " BIT b,E"))) ;; 8
              (#x44 (eboy-log (format " BIT b,H"))) ;; 8
              (#x45 (eboy-log (format " BIT b,L"))) ;; 8
              (#x46 (eboy-log (format " BIT b,(HL) ")));; 16

              ;; SET b,r - Set bit b in register r.
              ;; Flags affected:
              ;; None.
              (#xC7 (eboy-log (format " SET b,A"))) ;; 8
              (#xC0 (eboy-log (format " SET b,B"))) ;; 8
              (#xC1 (eboy-log (format " SET b,C"))) ;; 8
              (#xC2 (eboy-log (format " SET b,D"))) ;; 8
              (#xC3 (eboy-log (format " SET b,E"))) ;; 8
              (#xC4 (eboy-log (format " SET b,H"))) ;; 8
              (#xC5 (eboy-log (format " SET b,L"))) ;; 8
              (#xC6 (eboy-log (format " SET b,(HL) ")));; 16

              ;; RES b,r - Reset bit b in register r.
              ;; Flags affected:
              ;; None.
              (#x87 (eboy-log (format " RES b,A"))) ;; 8
              (#x80 (eboy-log (format " RES b,B"))) ;; 8
              (#x81 (eboy-log (format " RES b,C"))) ;; 8
              (#x82 (eboy-log (format " RES b,D"))) ;; 8
              (#x83 (eboy-log (format " RES b,E"))) ;; 8
              (#x84 (eboy-log (format " RES b,H"))) ;; 8
              (#x85 (eboy-log (format " RES b,L"))) ;; 8
              (#x86 (eboy-log (format " RES b,(HL) ")));; 16
              (otherwise (eboy-log (format "Unimplemented BC opcode 0x%x" opcode)))
              )))


    ;; JP nn - Jump to address nn.
    (#xC3 (eboy-log (format " JP $%04x" (eboy-get-short))) ;; 12
          (setq eboy-pc (1- (eboy-get-short))) ;; Compensate for the pc +1 that is done with each instruction.
          ;;(eboy-inc-pc 2)
          )

    ;; JP cc,nn - Jump to address n if following condition is true:
    ;;   cc = NZ, Jump if Z flag is reset.
    ;;   cc = Z, Jump if Z flag is set.
    ;;   cc = NC, Jump if C flag is reset.
    ;;   cc = C, Jump if C flag is set.
    ;; nn = two byte immediate value. (LS byte first.)
    (#xC2 (eboy-log (format " JP NZ,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xCA (eboy-log (format " JP Z,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xD2 (eboy-log (format " JP NC,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xDA (eboy-log (format " JP C,nn  ")) (assert nil t "unimplemented opcode"));; 12

    ;; JP (HL) - Jump to address contained in HL.
    (#xE9 (eboy-log (format " JP (HL) ")) (assert nil t "unimplemented opcode")) ;; 4

    ;; JR n - Add n to current address and jump to it.
    ;; nn = one byte signed immediate value
    (#x18 (eboy-log (format " JR n ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; JR cc,n - If following condition is true then add n to current address and jump to it:
    ;;  n = one byte signed immediate value
    ;;  cc = NZ, Jump if Z flag is reset.
    ;;  cc = Z, Jump if Z flag is set.
    ;;  cc = NC, Jump if C flag is reset.
    ;;  cc = C, Jump if C flag is set.
    (#x20 (eboy-log (format " JR NZ,* (%02d)" (eboy-byte-to-signed (eboy-get-byte))))
          (if (null (eboy-get-flag flags :Z))
              (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte)))))
          (incf eboy-pc)) ;; 8
    (#x28 (eboy-log (format " JR Z,* ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x30 (eboy-log (format " JR NC,* ")) (assert nil t "unimplemented opcode")) ;; 8
    (#x38 (eboy-log (format " JR C,* ")) (assert nil t "unimplemented opcode")) ;; 8


      ;;; Calls

    ;; CALL nn - Push address of next instruction onto stack and then jump to address nn.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xCD (eboy-log (format " CALL nn  ")) (assert nil t "unimplemented opcode"));; 12

    ;; CALL cc,nn - Call address n if following condition is true:
    ;;  cc = NZ, Call if Z flag is reset.
    ;;  cc = Z, Call if Z flag is set.
    ;;  cc = NC, Call if C flag is reset.
    ;;  cc = C, Call if C flag is set.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xC4 (eboy-log (format " CALL NZ,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xCC (eboy-log (format " CALL Z,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xD4 (eboy-log (format " CALL NC,nn  ")) (assert nil t "unimplemented opcode"));; 12
    (#xDC (eboy-log (format " CALL C,nn  ")) (assert nil t "unimplemented opcode"));; 12


      ;;; Restarts

    ;; RST n - Push present address onto stack. Jump to address $0000 + n.
    ;;  n = $00,$08,$10,$18,$20,$28,$30,$38
    (#xC7 (eboy-log (format " RST 00H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xCF (eboy-log (format " RST 08H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xD7 (eboy-log (format " RST 10H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xDF (eboy-log (format " RST 18H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xE7 (eboy-log (format " RST 20H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xEF (eboy-log (format " RST 28H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xF7 (eboy-log (format " RST 30H  ")) (assert nil t "unimplemented opcode"));; 32
    (#xFF (eboy-log (format " RST 38H  ")) (assert nil t "unimplemented opcode"));; 32

      ;;; Returns

    ;; RET - Pop two bytes from stack & jump to that address.
    (#xC9 (eboy-log (format " RET -/- ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; RET cc - Return if following condition is true:
    ;;  cc = NZ, Return if Z flag is reset.
    ;;  cc = Z, Return if Z flag is set.
    ;;  cc = NC, Return if C flag is reset.
    ;;  cc = C, Return if C flag is set.
    (#xC0 (eboy-log (format " RET NZ ")) (assert nil t "unimplemented opcode")) ;; 8
    (#xC8 (eboy-log (format " RET Z ")) (assert nil t "unimplemented opcode")) ;; 8
    (#xD0 (eboy-log (format " RET NC ")) (assert nil t "unimplemented opcode")) ;; 8
    (#xD8 (eboy-log (format " RET C ")) (assert nil t "unimplemented opcode")) ;; 8

    ;; RETI - Pop two bytes from stack & jump to that address then enable interrupts.
    (#xD9 (eboy-log (format " RETI -/- " )) (assert nil t "unimplemented opcode")) ;; 8

    ;; Non existant opcodes
    (#xD3 (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xDB (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xDD (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xE3 (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xE4 (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xEB (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xEC (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xED (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xF4 (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xFC (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (#xFD (eboy-log (format "Non existant opcode: 0x%02x" opcode)) (assert nil t))
    (otherwise (eboy-log (format "Unimplemented opcode 0x%x" opcode)) (assert nil t))
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
  (if eboy-debug-1 (eboy-log (format "Load rom: %s\n" eboy-rom-filename)))
  (if eboy-debug-1 (eboy-log (format "Rom size: %d bytes\n" eboy-rom-size)))
  (if eboy-debug-1 (eboy-log (format "Rom title: %s\n" (eboy-rom-title))))
  (if eboy-debug-1 (eboy-debug-dump-memory #xFF00 #xFFFF))

  ;; loop
  (let ((flags (make-bool-vector 4 nil)))
     ;; init flags 0xB0
    (eboy-set-flag flags :Z t)
    (eboy-set-flag flags :N nil)
    (eboy-set-flag flags :H t)
    (eboy-set-flag flags :C t)

    (while (and (< eboy-pc eboy-rom-size) (< eboy-debug-nr-instructions 50000))
      (if eboy-debug-1 (insert (format "%2d: " eboy-debug-nr-instructions)))
      (eboy-process-opcode (aref eboy-rom eboy-pc) flags)
      (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1))
      )
    )
  )

(eboy-load-rom)


(provide 'eboy)
;;; eboy.el ends here

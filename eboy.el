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

;; Interrupt masks
(defconst eboy-im-vblank #x01 "The interrupt mask for V-Blank.")
(defconst eboy-im-lcdc #x02 "The interrupt mask for LCDC.")
(defconst eboy-im-tmr-overflow #x04 "The interrupt mask for Timer Overflow.")
(defconst eboy-im-s-trans-compl #x08 "The interrupt mask for Serial I/O transfer complete.")
(defconst eboy-im-h2l-pins #x10 "The interrupt mask for transition from h to l of pin P10-P13.")

(defvar eboy-rom-filename nil "The file name of the loaded rom.")
(defvar eboy-boot-rom-filename nil "The path to the boot rom.")
(defvar eboy-boot-rom nil "The binary vector of the boot rom.")
(defvar eboy-rom nil "The binary vector of the rom.")
(defvar eboy-rom-size nil "The size of the rom in bytes.")
(defvar eboy-pc nil "The program counter.")
(defvar eboy-sp nil "The stack pointer.")
(defvar eboy-interrupt-master-enbl nil "Interupts master enabled flag.")
(defvar eboy-interrupt-pending #x00 "Flags of pending interrupts.")
(defvar eboy-interrupt-enabled #x00 "Flags of enabled interrupts.")
(defvar eboy-boot-rom-disabled-p nil "After boot disable the boot rom.")
(defvar eboy-clock-cycles 0 "Number of elapsed clock cycles.") ;; when to reset?
(defvar eboy-display-write-done nil "Indicate if at line 143 we need to write the display.")
(defvar eboy-lcd-ly nil "The LCDC Y Coordinate.")
(defvar eboy-lcd-scrollx nil "The LCDC Scroll X register.")
(defvar eboy-lcd-scrolly nil "The LCDC Scroll Y register.")
(defvar eboy-delay-enabling-interrupt-p nil "Enabling interrupts is delayd with one instruction.")

;;(defvar eboy-flags (make-bool-vector 4 t) "The flags Z(Zero) S(Negative) H(Halve Carry) and C(Carry).")
(defvar eboy-debug-nr-instructions nil "Number of instructions executed.")
(defvar eboy-debug-1 t "Enable debugging info.")
(defvar eboy-debug-2 nil "Enable debugging info.")
(defvar eboy-debug-pc-max nil "The program counter max.")

(defvar eboy-rA 0 "Register A.")
(defvar eboy-rB 0 "Register B.")
(defvar eboy-rC 0 "Register C.")
(defvar eboy-rD 0 "Register D.")
(defvar eboy-rE 0 "Register E.")
;;(defvar eboy-rF 0 "Register F.") flags
(defvar eboy-rH 0 "Register H.")
(defvar eboy-rL 0 "Register L.")

(defmacro eboy-add-byte (byte value)
  "Simulate byte behavior: add VALUE to BYTE."
  `(progn (setq ,byte (+ ,byte ,value))
          (setq ,byte (logand ,byte #xFF)))
  )

(defmacro eboy-add-to-short (short value)
  "Simulate short behavior: add VALUE to SHORT."
  `(progn (setq ,short (+ ,short ,value))
          (setq ,short (logand ,short #xFFFF)))
  )

;; (defmacro eboy-subtract-byte (byte value)
;;   "Simulate byte behavior: subtract VALUE from BYTE."
;;   `(progn (setq ,byte (- ,byte ,value))
;;           (if (< ,byte 0)
;;               (setq ,byte (+ ,byte 256))))
;;   )

;; (eboy-byte-to-signed #x12)

;; (1+ (logand (lognot #x12) #xFF))

;; (eboy-byte-to-signed #xff)
;; (eboy-byte-to-signed #x8F)

(defun eboy-byte-to-signed (byte)
  "Convert BYTE to signed number."
  ;;(* (1+ (logxor byte #xFF)) -1)
  (if (not (zerop (logand #x80 byte)))
      (setq byte (- byte #x100)))
  byte
  )

(defmacro eboy-dec (reg flags)
  "CPU Instruction: Decrement register REG and update FLAGS."
  `(progn (eboy-add-byte ,reg -1)
          (eboy-set-flag ,flags ,:Z (= ,reg 0))
          (eboy-set-flag ,flags ,:N t)
          (eboy-set-flag ,flags ,:H (= (logand ,reg #xF) #xF))
     )
  )

(defmacro eboy-inc-rpair (r1 r2 value)
  "Increase register pair R1 R2 with VALUE."
  `(let ((rpair (eboy-get-rpair ,r1 ,r2)) )
     (eboy-add-to-short rpair ,value)
     (eboy-set-rpair ,r1 ,r2 rpair)))

(defmacro eboy-set-rpair (r1 r2 value)
  "Put VALUE into register pair R1 and R2."
  `(progn
    (setq ,r1 (logand (lsh ,value -8) #xff))
    (setq ,r2 (logand ,value #xff))))

(defun eboy-get-rpair (r1 r2)
  "Get short by combining byte register R1 and R2."
  (logior (lsh r1 8) r2) )

(defun eboy-init-registers ()
  "Initialize all registers."
  (setq eboy-rA #x01) ;; 0x01:GB/SGB, 0xFF:GBP, 0x11:GBC
  (setq eboy-rB 0)
  (setq eboy-rC #x13)
  (setq eboy-rD 0)
  (setq eboy-rE #xD8)
  (setq eboy-rH #x01)
  (setq eboy-rL #x4d)
  )

(defun eboy-init-memory ()
  "Initialize the RAM to some startup values."
  (eboy-mem-write-byte #xFF05 #x00) ; TIMA
  (eboy-mem-write-byte #xFF06 #x00) ; TMA
  (eboy-mem-write-byte #xFF07 #x00) ; TAC
  (eboy-mem-write-byte #xFF10 #x80) ; NR10
  (eboy-mem-write-byte #xFF11 #xBF) ; NR11
  (eboy-mem-write-byte #xFF12 #xF3) ; NR12
  (eboy-mem-write-byte #xFF14 #xBF) ; NR14
  (eboy-mem-write-byte #xFF16 #x3F) ; NR21
  (eboy-mem-write-byte #xFF17 #x00) ; NR22
  (eboy-mem-write-byte #xFF19 #xBF) ; NR24
  (eboy-mem-write-byte #xFF1A #x7F) ; NR30
  (eboy-mem-write-byte #xFF1B #xFF) ; NR31
  (eboy-mem-write-byte #xFF1C #x9F) ; NR32
  (eboy-mem-write-byte #xFF1E #xBF) ; NR33
  (eboy-mem-write-byte #xFF20 #xFF) ; NR41
  (eboy-mem-write-byte #xFF21 #x00) ; NR42
  (eboy-mem-write-byte #xFF22 #x00) ; NR43
  (eboy-mem-write-byte #xFF23 #xBF) ; NR30
  (eboy-mem-write-byte #xFF24 #x77) ; NR50
  (eboy-mem-write-byte #xFF25 #xF3) ; NR51
  (eboy-mem-write-byte #xFF26 #xF1) ; NR52, F1-GB, $F0-SGB
  (eboy-mem-write-byte #xFF40 #x91) ; LCDC
  (eboy-mem-write-byte #xFF42 #x00) ; SCY
  (eboy-mem-write-byte #xFF43 #x00) ; SCX
  (eboy-mem-write-byte #xFF45 #x00) ; LYC
  (eboy-mem-write-byte #xFF47 #xFC) ; BGP
  (eboy-mem-write-byte #xFF48 #xFF) ; OBP0
  (eboy-mem-write-byte #xFF49 #xFF) ; OBP1
  (eboy-mem-write-byte #xFF4A #x00) ; WY
  (eboy-mem-write-byte #xFF4B #x00) ; WX
  (eboy-mem-write-byte #xFFFF #x00) ; IE
  )

(defun eboy-debug-dump-memory (start-address end-address)
  "Dump the memory from START-ADDRESS to END-ADDRESS."
  (insert (format "\nMemory: 0x%04x - 0x%04x\n" start-address end-address))
  (dotimes (i (1+ (- end-address start-address)))
    (let* ((address (+ start-address i))
           (data (eboy-mem-read-byte (+ start-address i))))
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

(defun eboy-msg (msg)
  "Write a MSG string to the message buffer."
  (if eboy-debug-2 (message "eboy: %s" msg))
  )

(defvar eboy-ram (make-vector (* 8 1024) 0) "The 8 kB interal RAM.")
(defvar eboy-hram (make-vector 128 0) "The 128 bytes high RAM.")
(defvar eboy-vram (make-vector (* 8 1024) 0) "The 8 kB interal Video RAM.")
(defvar eboy-io (make-vector #x4C 0) "The 8 IO registers.")

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
;; --------------------------- 4000  |= 32kB Cartridge
;; 16kB ROM bank #0                 |
;; --------------------------- 0000 --
;; * NOTE: b = bit, B = byte

(defun eboy-mem-read-byte (address)
  "Read byte from ADDRESS."
  (cond
   ((and (>= address #xFF80) (<= address #xFFFF))
    ;;(message "Internal RAM")
    (if (= address #xFFFF)
        (progn (eboy-msg "Read IE: Interrupt Enable.")
               eboy-interrupt-enabled)
      (aref eboy-hram (- address #xFF80))
      )
    )
   ((and (>= address #xFF4C) (< address #xFF80))
    ;;(message "Empty but unusable for I/O")
    (aref eboy-ram (- address #xE000))
    )
   ((and (>= address #xFF00) (< address #xFF4C))
    ;;(message "I/O ports")
    (let ((data (aref eboy-io (- address #xFF00))))
      (cond
       ((= address #xFF4B) (eboy-msg "Read WX: Window X position.") )
       ((= address #xFF4A) (eboy-msg "Read WY: Window Y position.") )
       ((= address #xFF49) (eboy-msg "Read OBP1: Object Palette 1 Data.") )
       ((= address #xFF48) (eboy-msg "Read OBPO: Object Palette 0 Data.") )
       ((= address #xFF47) (eboy-msg "Read BGP: BG & Window Palette DATA.") )
       ;;((= address #xFF46) (eboy-msg "Read DMA: DMA Transfer and Start Address.") )
       ((= address #xFF45) (eboy-msg "Read LYC: LY Compare.") )
       ((= address #xFF44) (eboy-msg "Read LY: LCDC Y Coordinate.")
        (setq data eboy-lcd-ly))
       ((= address #xFF43) (eboy-msg "Read SCX: Scroll X.")
        (setq data eboy-lcd-scrollx))
       ((= address #xFF42) (eboy-msg "Read SCY: Scroll Y.")
        (setq data eboy-lcd-scrolly))
       ((= address #xFF41) (eboy-msg "Read STAT: LCDC Status.") )
       ((= address #xFF40) (eboy-msg "Read LCDC: LCD Control.") )
       ;; in between sound registers, but not consecutive, some unknow address.
       ((= address #xFF0F)
        (eboy-msg "Read IF: Interrupt Flag.")
        (setq data eboy-interrupt-pending)
        )
       ((= address #xFF07) (eboy-msg "Read TAC: Timer Control.") )
       ((= address #xFF06) (eboy-msg "Read TMA: Timer Modulo.") )
       ((= address #xFF05) (eboy-msg "Read TIMA: Timer Counter.") )
       ((= address #xFF04) (eboy-msg "Read DIV: Divider Register.") )
       ((= address #xFF02) (eboy-msg "Read SC: SIO control.") )
       ((= address #xFF01) (eboy-msg "Read SB: Serial transfer data.") )
       ((= address #xFF00) (eboy-msg "Read P1: Joy Pad info and system type register.")
        (cond
         ((= (logand (lognot data) #x10) #x10) (eboy-msg "Direction keys selected") (setq data #x0F))
         ((= (logand (lognot data) #x20) #x20) (eboy-msg "Button keys selected") (setq data #x0F))
         )
        ))
      data
      )
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
   ((and (>= address #x0100) (< address #x4000))
    ;;(message "16kB ROM bank #0")
    (aref eboy-rom address))
   ((and (>= address #x0000) (< address #x0100))
    ;;(message "internal ROM bank #0")
    (if eboy-boot-rom-disabled-p
        (aref eboy-rom address)
      (aref eboy-boot-rom address))
    )
   ))

(defun eboy-mem-write-byte (address data)
  "Write DATA byte to memory at ADDRESS."
  (setq data (logand data #xff))
  (cond
   ((and (>= address #xFF80) (<= address #xFFFF))
    (if (= address #xFFFF)
        (progn (eboy-msg (format "Write IE: Interrupt Enable. #%02x" data))
               (if (= 1 (logand data eboy-im-vblank)) (eboy-msg "ie V-Blank"))
               (if (= 1 (logand data eboy-im-lcdc)) (eboy-msg "ie LCDC"))
               (if (= 1 (logand data eboy-im-tmr-overflow)) (eboy-msg "ie Timer Overflow"))
               (if (= 1 (logand data eboy-im-s-trans-compl)) (eboy-msg "ie Serial I/O transfer complete"))
               (if (= 1 (logand data eboy-im-h2l-pins)) (eboy-msg "ie transition from h to l of pin P10-P13"))
               (setq eboy-interrupt-enabled data)
               )
      ;; (message "Write Internal RAM")
      (aset eboy-hram (- address #xFF80) data)
      )
      )
   ((and (>= address #xFF4C) (< address #xFF80))
    ;; (message "Write Empty but unusable for I/O")
    (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFF00) (< address #xFF4C))
    ;; (message "Write I/O ports")
    (cond
     ((= address #xFF4B) (eboy-msg "Write WX: Window X position.") )
     ((= address #xFF4B) (eboy-msg "Write WX: Window X position.") )
     ((= address #xFF4A) (eboy-msg "Write WY: Window Y position.") )
     ((= address #xFF50) (eboy-msg "Write: Disable boot rom.")
      ;; on boot it writes 0x01, lets disable it always
      (setq eboy-boot-rom-disabled-p t))
     ((= address #xFF49) (eboy-msg "Write OBP1: Object Palette 1 Data.") )
     ((= address #xFF48) (eboy-msg "Write OBPO: Object Palette 0 Data.") )
     ((= address #xFF47) (eboy-msg "Write BGP: BG & Window Palette DATA.") )
     ((= address #xFF46) (eboy-msg "Write DMA: DMA Transfer and Start Address.") )
     ((= address #xFF45) (eboy-msg "Write LYC: LY Compare.") )
     ((= address #xFF44) (eboy-msg "Write LY: Reset the LCDC Y Coordinate.")
      (setq eboy-lcd-ly 0))
     ((= address #xFF43) (eboy-msg "Write SCX: Scroll X.")
      (setq eboy-lcd-scrollx data)
      ;;(message "scroll x %d" data)
      )
     ((= address #xFF42) (eboy-msg "Write SCY: Scroll Y.")
      (setq eboy-lcd-scrolly data)
      ;;(message "scroll y %d" data)
      )
     ((= address #xFF41) (eboy-msg "Write STAT: LCDC Status.") )
     ((= address #xFF40) (eboy-msg "Write LCDC: LCD Control.") )
     ;; in between sound registers, but not consecutive, some unknow address.
     ((= address #xFF0F) (progn (eboy-msg (format "Write IF: Interrupt Flag. #%02x" data))
                                (if (= 1 (logand data eboy-im-vblank)) (eboy-msg "if V-Blank"))
                                (if (= 1 (logand data eboy-im-lcdc)) (eboy-msg "if LCDC"))
                                (if (= 1 (logand data eboy-im-tmr-overflow)) (eboy-msg "if Timer Overflow"))
                                (if (= 1 (logand data eboy-im-s-trans-compl)) (eboy-msg "if Serial I/O transfer complete"))
                                (if (= 1 (logand data eboy-im-h2l-pins)) (eboy-msg "if transition from h to l of pin P10-P13"))
                                (setq eboy-interrupt-pending data)
                                ) )
     ((= address #xFF07) (eboy-msg "Write TAC: Timer Control.") )
     ((= address #xFF06) (eboy-msg "Write TMA: Timer Modulo.") )
     ((= address #xFF05) (eboy-msg "Write TIMA: Timer Counter.") )
     ((= address #xFF04) (eboy-msg "Write DIV: Divider Register.") )
     ((= address #xFF02) (eboy-msg "Write SC: SIO control.") )
     ((= address #xFF01) (eboy-msg "Write SB: Serial transfer data.") )
     ((= address #xFF00) (eboy-msg "Write P1: Joy Pad info and system type register.")
      ))

     (aset eboy-io (- address #xFF00) data))
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
    ;;(assert nil t "Non writable: 16kB switchable ROM bank")
    )
   ((and (>= address #x0000) (< address #x4000))
    ;; Tetris writes to 0x2000... why? memory bank select.
    ;;(assert nil t "Non writable: 16kB ROM bank #0")
    )
   ))

(defun eboy-mem-write-short (address data)
  "Write DATA short to memory at ADDRESS.
Little Endian."
  (eboy-mem-write-byte address (logand data #xFF))
  (eboy-mem-write-byte (1+ address) (logand (lsh data -8) #xFF))
  )

(defun eboy-mem-read-short (address)
  "Read short from memory at ADDRESS.
Little Endian."
  (logior (lsh (eboy-mem-read-byte (1+ address)) 8) (eboy-mem-read-byte address))
  )

;;(eboy-mem-write-byte #x4100 23)
;;(message "message %x" (eboy-mem-read-byte #x101))

(defun eboy-debug-print-flags (flags)
  "Print the FLAGS."
  (insert (format "Flags; Z:%s N:%s H:%s C:%s\n" (eboy-get-flag flags :Z) (eboy-get-flag  flags :N) (eboy-get-flag flags :H) (eboy-get-flag flags :C)))
  )

(defun eboy-print-registers ()
  "Insert the register values."
  (interactive)
  (insert (format "A:0x%02x B:0x%02x C:0x%02x D:0x%02x E:0x%02x H:0x%02x L:0x%02x" eboy-rA  eboy-rB  eboy-rC  eboy-rD  eboy-rE  eboy-rH  eboy-rL)))

(defun eboy-debug-print-cpu-state (flags)
  "Print the registers and FLAGS."
  (insert "\t\t\t\t")
  (eboy-print-registers)
  (insert (format " Z:%3s N:%3s H:%3s C:%3s\n"  (eboy-get-flag flags :Z) (eboy-get-flag  flags :N) (eboy-get-flag flags :H) (eboy-get-flag flags :C))))

(defun eboy-debug-print-stack ()
  "Print the stack."
  (interactive)
  (eboy-debug-dump-memory eboy-sp #xFFFE)
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
    (:C (aset flags 0 state))
    (:H (aset flags 1 state))
    (:N (aset flags 2 state))
    (:Z (aset flags 3 state))
    )
  )

;;(let ((flags  (make-bool-vector 4 nil)))
;;  (eboy-debug-print-flags flags)
;;  ;;(eboy-set-flag flags : nil)
;;  (eboy-debug-print-flags flags)
;;  (insert (format "%x" (eboy-flags-to-byte flags)))
;;  (eboy-debug-print-flags (eboy-byte-to-flags (eboy-flags-to-byte flags))))


(defun eboy-get-flag (flags flag)
  "Get FLAG from FLAGS."
  (ecase flag
    (:C (aref flags 0))
    (:H (aref flags 1))
    (:N (aref flags 2))
    (:Z (aref flags 3))
    )
  )

(defun eboy-flags-to-byte (flags)
  "Convert bitvector FLAGS to byte."
  (let ((byte-flags #x00))
    (if (eboy-get-flag flags :C)
        (setq byte-flags (logior #x10 byte-flags)))
    (if (eboy-get-flag flags :H)
        (setq byte-flags (logior #x20 byte-flags)))
    (if (eboy-get-flag flags :N)
        (setq byte-flags (logior #x40 byte-flags)))
    (if (eboy-get-flag flags :Z)
        (setq byte-flags (logior #x80 byte-flags)))
    byte-flags
    )
  )

(defun eboy-byte-to-flags(byte-flags)
  "Convert BYTE-FLAGS back to bitvector flag."
  (let ((flags (make-bool-vector 4 nil)))
    (if (> (logand #x10 byte-flags) 0)
        (eboy-set-flag flags :C t))
    (if (> (logand #x20 byte-flags) 0)
        (eboy-set-flag flags :H t))
    (if (> (logand #x40 byte-flags) 0)
        (eboy-set-flag flags :N t))
    (if (> (logand #x80 byte-flags) 0)
        (eboy-set-flag flags :Z t))
    flags
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
  (eboy-mem-read-short (+ eboy-pc 1))
  )

(defun eboy-get-byte ()
  "Skip opcode and get next byte."
  (eboy-mem-read-byte (+ eboy-pc 1))
  )

(defun eboy-inc-pc (nr-bytes)
  "Increment program counter with NR-BYTES."
  (setq eboy-pc (+ eboy-pc nr-bytes))
  )

(defun eboy-set-rBC (value)
  "Put VALUE into registers BC."
  (eboy-set-rpair eboy-rB eboy-rC value))
(defun eboy-set-rDE (value)
  "Put VALUE into registers DE."
  (eboy-set-rpair eboy-rD eboy-rE value))
(defun eboy-set-rHL (value)
  "Put VALUE into registers HL."
  (eboy-set-rpair eboy-rH eboy-rL value))

(defun eboy-get-rBC ()
  "Get short by combining byte register B and C."
  (eboy-get-rpair eboy-rB eboy-rC) )
(defun eboy-get-rDE ()
  "Get short by combining byte register D and E."
  (eboy-get-rpair eboy-rD eboy-rE) )
(defun eboy-get-rHL ()
  "Get short by combining byte register H and L."
  (eboy-get-rpair eboy-rH eboy-rL) )

(defun eboy-debug-unimplemented-opcode (opcode)
  "Print OPCODE is unimplemented."
  (insert (format "Unimplemented opcode 0x%02x" opcode)))

;;;;;
;;;; DISPLAY section
;;;;;

(defun eboy-get-color (byte1 byte2 x)
  "Get from line y BYTE1 and BYTE2 the color of coordinate X."
  (let* ((bit (- 7(mod x 8)))
         (mask (lsh 1 bit)))
    (logior (lsh (logand byte2 mask) (+ (* -1 bit) 1))
            (lsh (logand byte1 mask) (* -1 bit))))
  )

 ;; (eboy-get-color #x64 #x28 0) ; ⇒ 0
 ;; (eboy-get-color #x64 #x28 1) ; ⇒ 1
 ;; (eboy-get-color #x64 #x28 2) ; ⇒ 3
 ;; (eboy-get-color #x64 #x28 3) ; ⇒ 0
 ;; (eboy-get-color #x64 #x28 4) ; ⇒ 2
 ;; (eboy-get-color #x64 #x28 5) ; ⇒ 1
 ;; (eboy-get-color #x64 #x28 6) ; ⇒ 0
 ;; (eboy-get-color #x64 #x28 7) ; ⇒ 0

;; ⇒ 1 (x,y) -> tile-number -> tile-id -> 2 bytes for a line y.
(defun eboy-window-tile-map-selected-addr ()
  "docstring"
  (interactive)
  (if (= #x00 (logand (eboy-mem-read-byte #xFF40) #x40))
      #x9800
    #x9c00)
  )
(defun eboy-bg-&-window-tile-data-selected-addr ()
  "docstring"
  (interactive)
  (if (= #x00 (logand (eboy-mem-read-byte #xFF40) #x10))
      #x8800
    #x8000)
  )
(defun eboy-bg-tile-map-selected-addr ()
  "docstring"
  (interactive)
  (if (= #x00 (logand (eboy-mem-read-byte #xFF40) #x08))
      #x9800
    #x9c00)
  )

(defun eboy-get-tile-nr (x y)
  "Given a X Y coordinate, return tile number."
  (+ (/ x 8) (* (/ y 8) 32))
  )
;; (eboy-get-tile-nr 255 255) ; ⇒ 1023

(defun eboy-get-tile-id (tile-nr)
  "Get tile id from Background Tile Map using the TILE-NR."
  (eboy-mem-read-byte (+ (eboy-bg-tile-map-selected-addr) tile-nr)) ; assuming tile map 0 for now
  )
(defun eboy-get-color-xy (x y)
  "Get the color for coordinate X, Y from the bg & window tile data."
  (let ((tile-line-addr (+ (+ (eboy-bg-&-window-tile-data-selected-addr) (* (eboy-get-tile-id(eboy-get-tile-nr x y)) 16)) (* (mod y 8) 2))))
    (eboy-get-color (eboy-mem-read-byte tile-line-addr) (eboy-mem-read-byte (1+ tile-line-addr)) x )
    )
  )

(defvar eboy-display-color-table (make-hash-table :test 'eq) "The hash table with display values.")
(puthash 3 "15 56 15" eboy-display-color-table) ; #0F380F
(puthash 2 "48 98 48" eboy-display-color-table) ; #309830
(puthash 1 "139 172 15" eboy-display-color-table) ; #8BAC0F
(puthash 0 "155 188 15" eboy-display-color-table) ; #9BBC0F

(defvar eboy-display-unicode-table (make-hash-table :test 'eq) "The hash table with unicode display values.")
(puthash 3 #x2588 eboy-display-unicode-table) ; █
(puthash 2 #x2593 eboy-display-unicode-table) ; ▓
(puthash 1 #x2592 eboy-display-unicode-table) ; ▒
(puthash 0 #x0020 eboy-display-unicode-table) ;

(defun eboy-write-display ()
  "Write the colors."
  (interactive)
  (with-current-buffer "*eboy-display*")
  ;(switch-to-buffer "*eboy-display*")
;;  (fundamental-mode)
  ;;(inhibit-read-only t)
  (erase-buffer)
  (insert "P3\n")
  (insert "160 144\n")
  (insert "255\n")
  (dotimes (y 144)
    (dotimes (x 160)
      (insert (format "%s " (gethash (eboy-get-color-xy x y) eboy-display-color-table)))
      )
    (insert "\n")
    )
  (image-mode)
  (deactivate-mark)
  (display-buffer "*eboy-display*")
  ;;(switch-to-buffer "*eboy*")
  )
(defun eboy-get-XPM-string ()
  "Get the XPM image string."
  (let ((xpm "/* XPM */
static char *frame[] = {
\"160  144 4 1\",
\"0 c #9BBC0F \",
\"1 c #8BAC0F \",
\"2 c #309830 \",
\"3 c #0F380F \",
"))
    (dotimes (y 144)
      (setq xpm (concat xpm  "\""))
      (dotimes (x 160)
        (setq xpm (concat xpm (format "%s" (eboy-get-color-xy x y)))))
      (setq xpm (concat xpm  "\",\n"))
      )
    (setq xpm (concat xpm "};"))
    )
  )

(defun eboy-write-display2 ()
  "Write the image to the buffer, in XPM format."
  (interactive)
  (with-current-buffer "*eboy-display*")
  (erase-buffer)
  (insert (propertize " " 'display (create-image (eboy-get-XPM-string) 'xpm t)))
  (deactivate-mark)
  (redisplay)
  )

(defun eboy-write-display-unicode ()
  "Write the display as unicode characters."
  (interactive)
  (with-current-buffer "*eboy-display*")
  (erase-buffer)
  (dotimes (y 144)
    (dotimes (x 160)
      (insert (gethash (eboy-get-color-xy (mod (+ x eboy-lcd-scrollx) 256) (mod (+ y eboy-lcd-scrolly) 256)) eboy-display-unicode-table))
      )
    (insert "\n")
    )
  (goto-char (point-min))
  (redisplay)
  )

(defun eboy-lcd-cycle ()
  "Perform a single lcd cycle."
  (let ((screen-cycle (mod eboy-clock-cycles 70224)))
    (setq eboy-lcd-ly (/ screen-cycle 456))
    (when (and (= eboy-lcd-ly 143) (not eboy-display-write-done))
      (eboy-write-display-unicode)
      (setq eboy-display-write-done t))
    (when (and (= eboy-lcd-ly 144) eboy-display-write-done)
      (setq eboy-interrupt-pending (logior eboy-interrupt-pending eboy-im-vblank))
      (setq eboy-display-write-done nil)
        )
    )
  )

 ;; assuming data table 0 for now
;;(eboy-get-tile-nr 17 11)

;; (mod 754 8)
;;
;; ((lambda (x) (lsh 1 (- 7 (mod x 8))) ) 8)
;;
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 1) ; ⇒ 2
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 2) ; ⇒ 4
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 3) ; ⇒ 6
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 4) ; ⇒ 0
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 5) ; ⇒ 2
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 6) ; ⇒ 4
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 7) ; ⇒ 6
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 8) ; ⇒ 0
;; (getColor (eboy-mem-read-byte #x8010) (eboy-mem-read-byte #x8011) 0) ; ⇒ 0



(defun eboy-process-opcode (opcode flags)
  "Process OPCODE, cpu FLAGS state."
  ;;(if eboy-debug-1 (eboy-debug-print-cpu-state flags))
  (if eboy-debug-1 (insert (format "\npc: 0x%x, opcode: 0x%02x, 0x%x" eboy-pc opcode eboy-clock-cycles)))
  (cl-case opcode
    (#x00 (eboy-log " NOP") (incf eboy-clock-cycles 4))

    ;; LD nn,n
    (#x06 (eboy-log (format " LD B, #0x%02x" (eboy-get-byte)))
          (setq eboy-rB (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))
    (#x0E (eboy-log (format " LD C, #0x%02x" (eboy-get-byte)))
          (setq eboy-rC (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))
    (#x16 (eboy-log (format " LD D, #0x%02x" (eboy-get-byte)))
          (setq eboy-rD (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))
    (#x1E (eboy-log (format " LD E, #0x%02x" (eboy-get-byte)))
          (setq eboy-rE (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))
    (#x26 (eboy-log (format " LD H, #0x%02x" (eboy-get-byte)))
          (setq eboy-rH (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))
    (#x2E (eboy-log (format " LD L, #0x%02x" (eboy-get-byte)))
          (setq eboy-rL (eboy-get-byte))
          (eboy-inc-pc 1)
          (incf eboy-clock-cycles 8))

    ;; ;; LD r1,r2
    (#x7F (eboy-log (format " LD A,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x78 (eboy-log (format " LD A,B"))
          (setq eboy-rA eboy-rB) (incf eboy-clock-cycles 4))
    (#x79 (eboy-log (format " LD A,C"))
          (setq eboy-rA eboy-rC) (incf eboy-clock-cycles 4))
    (#x7A (eboy-log (format " LD A,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x7B (eboy-log (format " LD A,E"))
          (setq eboy-rA eboy-rE) (incf eboy-clock-cycles 4))
    (#x7C (eboy-log (format " LD A,H"))
          (setq eboy-rA eboy-rH) (incf eboy-clock-cycles 4))
    (#x7D (eboy-log (format " LD A,L"))
          (setq eboy-rA eboy-rL) (incf eboy-clock-cycles 4))
    (#x7E (eboy-log (format " LD A,(HL)"))
          (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
          (incf eboy-clock-cycles 8))
    (#x40 (eboy-log (format " LD B,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x41 (eboy-log (format " LD B,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x42 (eboy-log (format " LD B,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x43 (eboy-log (format " LD B,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x44 (eboy-log (format " LD B,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x45 (eboy-log (format " LD B,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x46 (eboy-log (format " LD B,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x48 (eboy-log (format " LD C,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x49 (eboy-log (format " LD C,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x4A (eboy-log (format " LD C,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x4B (eboy-log (format " LD C,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x4C (eboy-log (format " LD C,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x4D (eboy-log (format " LD C,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x4E (eboy-log (format " LD C,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x50 (eboy-log (format " LD D,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x51 (eboy-log (format " LD D,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x52 (eboy-log (format " LD D,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x53 (eboy-log (format " LD D,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x54 (eboy-log (format " LD D,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x55 (eboy-log (format " LD D,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x56 (eboy-log (format " LD D,(HL)"))
          (setq eboy-rD (eboy-mem-read-byte (eboy-get-rHL)))
          (incf eboy-clock-cycles 8))
    (#x58 (eboy-log (format " LD E,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x59 (eboy-log (format " LD E,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x5A (eboy-log (format " LD E,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x5B (eboy-log (format " LD E,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x5C (eboy-log (format " LD E,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x5D (eboy-log (format " LD E,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x5E (eboy-log (format " LD E,(HL)"))
          (setq eboy-rE (eboy-mem-read-byte (eboy-get-rHL))) (incf eboy-clock-cycles 8))
    (#x60 (eboy-log (format " LD H,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x61 (eboy-log (format " LD H,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x62 (eboy-log (format " LD H,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x63 (eboy-log (format " LD H,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x64 (eboy-log (format " LD H,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x65 (eboy-log (format " LD H,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x66 (eboy-log (format " LD H,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x68 (eboy-log (format " LD L,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x69 (eboy-log (format " LD L,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x6A (eboy-log (format " LD L,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x6B (eboy-log (format " LD L,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x6C (eboy-log (format " LD L,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x6D (eboy-log (format " LD L,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x6E (eboy-log (format " LD L,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x70 (eboy-log (format " LD (HL),B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x71 (eboy-log (format " LD (HL),C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x72 (eboy-log (format " LD (HL),D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x73 (eboy-log (format " LD (HL),E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x74 (eboy-log (format " LD (HL),H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x75 (eboy-log (format " LD (HL),L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x36 (eboy-log (format " LD (HL),0x%02x" (eboy-get-byte)))
          (eboy-mem-write-byte (eboy-get-rHL) (eboy-get-byte))
          (eboy-inc-pc 1) (incf eboy-clock-cycles 12))

    ;; LD A,n
    (#x0A (eboy-log (format " LD A,(BC)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x1A (eboy-log (format " LD A,(DE)"))
          (setq eboy-rA (eboy-mem-read-byte (eboy-get-rDE))) (incf eboy-clock-cycles 8))
    (#xFA (eboy-log (format " LD A,(0x%02x)" (eboy-get-short)))
          (setq eboy-rA (eboy-mem-read-byte (eboy-get-short)))
          (eboy-inc-pc 2)
          (incf eboy-clock-cycles 16))
    (#x3E (eboy-log (format " LD A,#0x%02x" (eboy-get-byte)))
          (setq eboy-rA (eboy-get-byte))
          (eboy-inc-pc 1) (incf eboy-clock-cycles 8))

    ;; LD n,A - Put value A into n.
    ;; n = A,B,C,D,E,H,L,(BC),(DE),(HL),(nn)
    ;; nn = two byte immediate value. (LS byte first.)
    (#x47 (eboy-log (format " LD B,A"))
          (setq eboy-rB eboy-rA)
           (incf eboy-clock-cycles 4))
    (#x4F (eboy-log (format " LD C,A (0x%02x)" eboy-rA))
          (setq eboy-rC eboy-rA) (incf eboy-clock-cycles 4))
    (#x57 (eboy-log (format " LD D,A"))
          (setq eboy-rD eboy-rA) (incf eboy-clock-cycles 4))
    (#x5F (eboy-log (format " LD E,A"))
          (setq eboy-rE eboy-rA)
           (incf eboy-clock-cycles 4))
    (#x67 (eboy-log (format " LD H,A"))
          (setq eboy-rH eboy-rA) (incf eboy-clock-cycles 4))
    (#x6F (eboy-log (format " LD L,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x02 (eboy-log (format " LD (BC),A") (assert nil t "unimplemented opcode"))
          ;;(eboy-set-rBC eboy-rA) maybe (BC) means memory address?
           (incf eboy-clock-cycles 8))
    (#x12 (eboy-log (format " LD (DE),A"))
          (eboy-mem-write-byte (eboy-get-rDE) eboy-rA) (incf eboy-clock-cycles 8))
    (#x77 (eboy-log (format " LD (HL),A"))
          (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
           (incf eboy-clock-cycles 8))
    (#xEA (eboy-log (format " LD (0x%0004x),A" (eboy-get-short)))
          (eboy-mem-write-byte (eboy-get-short) eboy-rA)
          (eboy-inc-pc 2) (incf eboy-clock-cycles 16))

    ;; Put value at address $FF00 + register C into A.
    (#xF2 (eboy-log (format " LD A,($FF00+C)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    ;; Put A into address $FF00 + register C.
    (#xE2 (eboy-log (format " LD ($FF00+C),A"))
          (eboy-mem-write-byte (+ #xFF00 eboy-rC) eboy-rA)
           (incf eboy-clock-cycles 8))
    ;; Put value at address HL into A. Decrement HL.
    (#x3A (eboy-log (format " LD A,(HLD)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    ;; Put A into memory address HL. Decrement HL.
    (#x32 (eboy-log (format " LD (HLD),A"))
          (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
          (eboy-set-rHL (1- (eboy-get-rHL)))
           (incf eboy-clock-cycles 8))
    ;; Put value at address HL into A. Increment HL.
    (#x2A (eboy-log (format " LD A,(HLI)"))
          (setq eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
          (eboy-set-rHL (1+ (eboy-get-rHL))) (incf eboy-clock-cycles 8))

    ;; Put A into memory address HL. Increment HL.
    (#x22 (eboy-log (format " LD (HLI),A"))
          (eboy-mem-write-byte (eboy-get-rHL) eboy-rA)
          (eboy-set-rHL (1+ (eboy-get-rHL)))
           (incf eboy-clock-cycles 8))
    ;; Put A into memory address $FF00+n
    (#xE0 (eboy-log (format " LD ($FF00+0x%02x),A (0x%02x)" (eboy-get-byte) eboy-rA))
          (eboy-mem-write-byte (+ #xFF00 (eboy-get-byte)) eboy-rA)
          (eboy-inc-pc 1) (incf eboy-clock-cycles 12))
    ;; Put memory address $FF00+n into A.
    (#xF0 (eboy-log (format " LD A,($FF00+0x%02x) (0x%02x)" (eboy-get-byte) (eboy-mem-read-byte (+ #xFF00 (eboy-get-byte)))))
          (setq eboy-rA (eboy-mem-read-byte (+ #xFF00 (eboy-get-byte))))
          (eboy-inc-pc 1) (incf eboy-clock-cycles 12))

    ;; 16 bit loads, nn = 16 bit immediate value
    (#x01 (eboy-log (format " LD BC, $%04x" (eboy-get-short)))
          (eboy-set-rBC (eboy-get-short))
          (eboy-inc-pc 2) (incf eboy-clock-cycles 12))
    (#x11 (eboy-log (format " LD DE, $%04x" (eboy-get-short)))
          (eboy-set-rDE (eboy-get-short))
          (eboy-inc-pc 2)
           (incf eboy-clock-cycles 12))
    (#x21 (eboy-log (format " LD HL, $%04x" (eboy-get-short)))
          (eboy-set-rHL (eboy-get-short))
          (eboy-inc-pc 2) (incf eboy-clock-cycles 12))
    (#x31 (eboy-log (format " LD SP, $%04x" (eboy-get-short)))
          (setq eboy-sp (eboy-get-short))
          (eboy-inc-pc 2) (incf eboy-clock-cycles 12))

    (#xF9 (eboy-log (format " LD SP,HL")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; Put SP + n effective address into HL, n = one byte signed immediate value.
    ;; Flags affected:
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xF8 (eboy-log (format " LDHL SP,n")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 12))
    ;; Put Stack Pointer (SP) at address n. n = two byte immediate address
    (#x08 (eboy-log (format " LD (nn),SP")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 20))

    ;; Push register pair nn onto stack.
    ;; Decrement Stack Pointer (SP) twice.
    (#xF5 (eboy-log (format " PUSH AF"))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (eboy-get-rpair eboy-rA (eboy-flags-to-byte flags)))
          (incf eboy-clock-cycles 16))
    (#xC5 (eboy-log (format " PUSH BC"))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (eboy-get-rBC))
          (incf eboy-clock-cycles 16))
    (#xD5 (eboy-log (format " PUSH DE"))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (eboy-get-rDE))
          (incf eboy-clock-cycles 12)) ;; TODO: maybe change into 16 again?
    (#xE5 (eboy-log (format " PUSH HL"))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (eboy-get-rHL))
          (incf eboy-clock-cycles 16))

    ;; Pop two bytes off stack into register pair nn.
    ;; Increment Stack Pointer (SP) twice.
    (#xF1 (eboy-log (format " POP AF"))
          (let ((data (eboy-mem-read-short eboy-sp)))
            (setq eboy-rA (lsh data -8))
            (setq eboy-flags (eboy-byte-to-flags (logand data #x00FF)))
            )
          (incf eboy-sp 2)
          (incf eboy-clock-cycles 12))
    (#xC1 (eboy-log (format " POP BC"))
          (eboy-set-rBC (eboy-mem-read-short eboy-sp))
          (incf eboy-sp 2)
          (incf eboy-clock-cycles 12))
    (#xD1 (eboy-log (format " POP DE"))
          (eboy-set-rDE (eboy-mem-read-short eboy-sp))
          (incf eboy-sp 2)
          (incf eboy-clock-cycles 12))
    (#xE1 (eboy-log (format " POP HL"))
          (eboy-set-rHL (eboy-mem-read-short eboy-sp))
          (incf eboy-sp 2)
          (incf eboy-clock-cycles 12))


    ;; ADD A,n    -    Add n to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x87 (eboy-log (format " ADD A,A"))
          (let ((rAold eboy-rA))
            (eboy-add-byte eboy-rA eboy-rA)
            (eboy-set-flag flags :Z (zerop eboy-rA))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
            (eboy-set-flag flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
           (incf eboy-clock-cycles 4))
    (#x80 (eboy-log (format " ADD A,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x81 (eboy-log (format " ADD A,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x82 (eboy-log (format " ADD A,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x83 (eboy-log (format " ADD A,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x84 (eboy-log (format " ADD A,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x85 (eboy-log (format " ADD A,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x86 (eboy-log (format " ADD A,(HL)"))
          (let ((rAold eboy-rA))
            (eboy-add-byte eboy-rA (eboy-mem-read-byte (eboy-get-rHL)))
            (eboy-set-flag flags :Z (zerop eboy-rA))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
            (eboy-set-flag flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
          (incf eboy-clock-cycles 8))
    (#xC6 (eboy-log (format " ADD A,#"))
          (let ((rAold eboy-rA))
            (eboy-add-byte eboy-rA (eboy-get-byte))
            (eboy-set-flag flags :Z (zerop eboy-rA))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H (< (logand eboy-rA #xf) (logand rAold #xf)))
            (eboy-set-flag flags :C (< (logand eboy-rA #xff) (logand rAold #xff))))
          (incf eboy-clock-cycles 8))

    ;;ADC A,n    -   Add n + Carry flag to A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Set if carry from bit 7.
    (#x8F (eboy-log (format " ADC A,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x88 (eboy-log (format " ADC A,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x89 (eboy-log (format " ADC A,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x8A (eboy-log (format " ADC A,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x8B (eboy-log (format " ADC A,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x8C (eboy-log (format " ADC A,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x8D (eboy-log (format " ADC A,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x8E (eboy-log (format " ADC A,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xCE (eboy-log (format " ADC A,#")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; SUB n - Subtract n from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x97 (eboy-log (format " SUB A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x90 (eboy-log (format " SUB B"))
          (let ((oldval eboy-rA))
            (eboy-add-byte eboy-rA (* -1 eboy-rB))
            (eboy-set-flag flags :Z (zerop eboy-rA))
            (eboy-set-flag flags :N t)
            (eboy-set-flag flags :H (> (logand eboy-rA #x0F) (logand oldval #x0F)))
            (eboy-set-flag flags :C (> (logand eboy-rA #xFF) (logand oldval #xFF)))
            )
           (incf eboy-clock-cycles 4))
    (#x91 (eboy-log (format " SUB C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x92 (eboy-log (format " SUB D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x93 (eboy-log (format " SUB E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x94 (eboy-log (format " SUB H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x95 (eboy-log (format " SUB L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x96 (eboy-log (format " SUB (HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xD6 (eboy-log (format " SUB #")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; SBC A,n - Subtract n + Carry flag from A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set if no borrow.
    (#x9F (eboy-log (format " SBC A,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x98 (eboy-log (format " SBC A,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x99 (eboy-log (format " SBC A,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x9A (eboy-log (format " SBC A,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x9B (eboy-log (format " SBC A,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x9C (eboy-log (format " SBC A,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x9D (eboy-log (format " SBC A,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x9E (eboy-log (format " SBC A,(HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xDE (eboy-log (format " SBC A,#")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles ))

    ;; AND n - Logically AND n with A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set.
    ;; C - Reset.
    (#xA7 (eboy-log (format " AND A"))
          (setq eboy-rA (logand eboy-rA eboy-rA))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H t)
          (eboy-set-flag flags :C nil) (incf eboy-clock-cycles 4))
    (#xA0 (eboy-log (format " AND B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA1 (eboy-log (format " AND C"))
          (setq eboy-rA (logand eboy-rA eboy-rC))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H t)
          (eboy-set-flag flags :C nil)
           (incf eboy-clock-cycles 4))
    (#xA2 (eboy-log (format " AND D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA3 (eboy-log (format " AND E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA4 (eboy-log (format " AND H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA5 (eboy-log (format " AND L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA6 (eboy-log (format " AND (HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xE6 (eboy-log (format " AND # (#%02x) " (eboy-get-byte)))
          (setq eboy-rA (logand eboy-rA (eboy-get-byte)))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H t)
          (eboy-set-flag flags :C nil)
          (eboy-inc-pc 1)
           (incf eboy-clock-cycles 8))

    ;; OR n - Logical OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xB7 (eboy-log (format " OR A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB0 (eboy-log (format " OR B"))
          (setq eboy-rA (logior eboy-rB eboy-rA))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil) (incf eboy-clock-cycles 4))
    (#xB1 (eboy-log (format " OR C"))
          (setq eboy-rA (logior eboy-rC eboy-rA))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil)
           (incf eboy-clock-cycles 4))
    (#xB2 (eboy-log (format " OR D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB3 (eboy-log (format " OR E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB4 (eboy-log (format " OR H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB5 (eboy-log (format " OR L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB6 (eboy-log (format " OR (HL)")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xF6 (eboy-log (format " OR #")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; XOR n - Logical exclusive OR n with register A, result in A.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Reset.
    (#xAF (eboy-log (format " XOR A "))
          (setq eboy-rA (logxor eboy-rA eboy-rA))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil)
           (incf eboy-clock-cycles 4))
    (#xA8 (eboy-log (format " XOR B ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xA9 (eboy-log (format " XOR C "))
          (setq eboy-rA (logxor eboy-rC eboy-rA))
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H nil)
          (eboy-set-flag flags :C nil)

           (incf eboy-clock-cycles 4))
    (#xAA (eboy-log (format " XOR D ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xAB (eboy-log (format " XOR E ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xAC (eboy-log (format " XOR H ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xAD (eboy-log (format " XOR L ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xAE (eboy-log (format " XOR (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#xEE (eboy-log (format " XOR * ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; CP n - Compare A with n. This is basically an A - n subtraction instruction but the results are thrown away.
    ;; Flags affected:
    ;; Z - Set if result is zero. (Set if A = n.)
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Set for no borrow. (Set if A < n.)
    (#xBF (eboy-log (format " CP A ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB8 (eboy-log (format " CP B ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xB9 (eboy-log (format " CP C ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xBA (eboy-log (format " CP D ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xBB (eboy-log (format " CP E ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xBC (eboy-log (format " CP H ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xBD (eboy-log (format " CP L ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#xBE (eboy-log (format " CP (HL) "))
          (let ((n (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-set-flag flags :Z (= eboy-rA n))
            (eboy-set-flag flags :N t)
            (eboy-set-flag flags :H (> (logand (- eboy-rA n) #x0F) (logand eboy-rA #x0F)))
            (eboy-set-flag flags :C (< eboy-rA n))
            )
          (incf eboy-clock-cycles 8))
    (#xFE (eboy-log (format " CP # (0x%02x)" (eboy-get-byte)))
          (let ((n (eboy-get-byte)))
            (eboy-set-flag flags :Z (= eboy-rA n))
            (eboy-set-flag flags :N t)
            (eboy-set-flag flags :H (> (logand (- eboy-rA n) #x0F) (logand eboy-rA #x0F)))
            (eboy-set-flag flags :C (< eboy-rA n))
            (eboy-inc-pc 1)
            )
          (incf eboy-clock-cycles 8))

    ;; INC n - Increment register n.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Set if carry from bit 3.
    ;; C - Not affected.
    (#x3C (eboy-log (format " INC A "))
          (eboy-add-byte eboy-rA 1)
          (eboy-set-flag flags :Z (zerop eboy-rA))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rA #xf) (logand (1- eboy-rA) #xf)))
          (incf eboy-clock-cycles 4))
    (#x04 (eboy-log (format " INC B "))
          (eboy-add-byte eboy-rB 1)
          (eboy-set-flag flags :Z (zerop eboy-rB))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rB #xf) (logand (1- eboy-rB) #xf)))
          (incf eboy-clock-cycles 4))
    (#x0C (eboy-log (format " INC C "))
          (eboy-add-byte eboy-rC 1)
          (eboy-set-flag flags :Z (zerop eboy-rC))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rC #xf) (logand (1- eboy-rC) #xf)))
          (incf eboy-clock-cycles 4))
    (#x14 (eboy-log (format " INC D ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    (#x1C (eboy-log (format " INC E "))
          (eboy-add-byte eboy-rE 1)
          (eboy-set-flag flags :Z (zerop eboy-rE))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rE #xf) (logand (1- eboy-rE) #xf)))
          (incf eboy-clock-cycles 4))
    (#x24 (eboy-log (format " INC H "))
          (eboy-add-byte eboy-rH 1)
          (eboy-set-flag flags :Z (zerop eboy-rH))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rH #xf) (logand (1- eboy-rH) #xf)))
          (incf eboy-clock-cycles 4))
    (#x2C (eboy-log (format " INC L "))
          (eboy-add-byte eboy-rL 1)
          (eboy-set-flag flags :Z (zerop eboy-rL))
          (eboy-set-flag flags :N nil)
          (eboy-set-flag flags :H (< (logand eboy-rL #xf) (logand (1- eboy-rL) #xf)))
          (incf eboy-clock-cycles 4))
    (#x34 (eboy-log (format " INC (HL)  "))
          (let* ((hl (eboy-get-rHL))
                (data (eboy-mem-read-byte hl)))
            (eboy-add-byte data 1)
            (eboy-mem-write-byte hl data)
            (eboy-set-flag flags :Z (zerop data))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H (< (logand data #xf) (logand (1- data) #xf)))
            )
          (incf eboy-clock-cycles 12))

    ;; DEC n - Decrement register n.
    ;; Flags affected:
    ;; Z - Set if reselt is zero.
    ;; N - Set.
    ;; H - Set if no borrow from bit 4.
    ;; C - Not affected.
    (#x3D (eboy-log (format " DEC A "))
          (eboy-dec eboy-rA flags) (incf eboy-clock-cycles 4))
    (#x05 (eboy-log (format " DEC B (before dec %d)" eboy-rB))
          (eboy-dec eboy-rB flags) (incf eboy-clock-cycles 4))
    (#x0D (eboy-log (format " DEC C ")) (eboy-dec eboy-rC flags) (incf eboy-clock-cycles 4))
    (#x15 (eboy-log (format " DEC D ")) (eboy-dec eboy-rD flags) (incf eboy-clock-cycles 4))
    (#x1D (eboy-log (format " DEC E ")) (eboy-dec eboy-rE flags) (incf eboy-clock-cycles 4))
    (#x25 (eboy-log (format " DEC H ")) (eboy-dec eboy-rH flags) (incf eboy-clock-cycles 4))
    (#x2D (eboy-log (format " DEC L ")) (eboy-dec eboy-rL flags) (incf eboy-clock-cycles 4))
    (#x35 (eboy-log (format " DEC (HL)  "))
          (let ((data (eboy-mem-read-byte (eboy-get-rHL))))
            (eboy-dec data flags)
            (eboy-mem-write-byte (eboy-get-rHL) data))
          (incf eboy-clock-cycles 12))

      ;;; 16-Bit Arithmetic
    ;;
    ;; ADD HL,n - Add n to HL.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Set if carry from bit 11.
    ;; C - Set if carry from bit 15.
    (#x09 (eboy-log (format " ADD HL,BC ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x19 (eboy-log (format " ADD HL,DE "))
          (let ((hl (eboy-get-rHL)))
            (eboy-set-rHL (+ hl (eboy-get-rDE)))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H (< (logand (eboy-get-rHL) #xfff) (logand hl #xfff)))
            (eboy-set-flag flags :C (< (logand (eboy-get-rHL) #xffff) (logand hl #xffff))))
          (incf eboy-clock-cycles 8))
    (#x29 (eboy-log (format " ADD HL,HL ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x39 (eboy-log (format " ADD HL,SP ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; ADD SP,n - Add n to Stack Pointer (SP).
    ;; Flags affected:
    ;; None.
    ;; Z - Reset.
    ;; N - Reset.
    ;; H - Set or reset according to operation.
    ;; C - Set or reset according to operation.
    (#xE8 (eboy-log (format " ADD SP,#  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

    ;; INC nn - Increment register nn.
    ;; Flags affected: None
    (#x03 (eboy-log (format " INC BC ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x13 (eboy-log (format " INC DE "))
          (eboy-set-rDE (1+ (eboy-get-rDE))) (incf eboy-clock-cycles 8))
    (#x23 (eboy-log (format " INC HL "))
          (eboy-set-rHL (1+ (eboy-get-rHL))) (incf eboy-clock-cycles 8))
    (#x33 (eboy-log (format " INC SP ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

    ;; DEC nn - Decrement register nn.
    ;; Flags affected:
    ;; None.
    (#x0B (eboy-log (format " DEC BC "))
          (eboy-set-rBC (1- (eboy-get-rBC))) (incf eboy-clock-cycles 8))
    (#x1B (eboy-log (format " DEC DE ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x2B (eboy-log (format " DEC HL ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
    (#x3B (eboy-log (format " DEC SP ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))

      ;;; Miscellaneous



    ;; DAA - Decimal adjust register A.
    ;; This instruction adjusts register A so that the correct representation of Binary Coded Decimal (BCD) is obtained.
    ;; Flags affected:
    ;; Z - Set if register A is zero.
    ;; N - Not affected.
    ;; H - Reset.
    ;; C - Set or reset according to operation.
    (#x27 (eboy-log (format " DAA -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; CPL - Complement A register. (Flip all bits.)
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Set.
    ;; H - Set.
    ;; C - Not affected.
    (#x2F (eboy-log (format " CPL -/- (0x%02x<-0x%02x)" (logand #xFF (lognot eboy-rA)) eboy-rA ))
          (setq eboy-rA (logand #xFF (lognot eboy-rA)))
          (eboy-set-flag flags :N t)
          (eboy-set-flag flags :H t)
          (incf eboy-clock-cycles 4))

    ;; CCF - Complement carry flag.
    ;; If C flag is set, then reset it.
    ;; If C flag is reset, then set it.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Complemented.
    (#x3F (eboy-log (format " CCF -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; SCF - Set Carry flag.
    ;; Flags affected:
    ;; Z - Not affected.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Set.
    (#x37 (eboy-log (format " SCF -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; HALT - Power down CPU until an interrupt occurs. Use this when ever possible to reduce energy consumption.
    (#x76 (eboy-log (format " HALT -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; STOP - Halt CPU & LCD display until button pressed.
    (#x10 (eboy-log (format " STOP -/- 10 ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))
    ;; This is a 2 byte opcode: 0x10 00

    ;; DI - This instruction disables interrupts but not immediately. Interrupts are disabled after instruction after DI is executed.
    ;; Flags affected:
    ;; None.
    (#xF3 (eboy-log (format " DI -/- ")) (setq eboy-interrupt-master-enbl nil) (incf eboy-clock-cycles 4))
    ;; EI - Enable interrupts. This intruction enables interrupts but not immediately. Interrupts are enabled after instruction after EI is executed.
    ;; Flags affected:
    ;; None.
    (#xFB (eboy-log (format " EI -/- "))
          (setq eboy-interrupt-master-enbl t)
          (incf eboy-clock-cycles 4)
          (setq eboy-delay-enabling-interrupt-p t))

      ;;; Rotates & Shift

    ;; RLCA - Rotate A left. Old bit 7 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x07 (eboy-log (format " RLCA -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; RLA - Rotate A left through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 7 data.
    (#x17 (eboy-log (format " RLA -/- "))
          (let ((c (= (lsh eboy-rA -7) #x01)))
            (setq eboy-rA (logior (lsh eboy-rA 1) (if (eboy-get-flag flags :C) #x01 #x00)))
            (eboy-set-flag flags :Z (zerop eboy-rA))
            (eboy-set-flag flags :N nil)
            (eboy-set-flag flags :H nil)
            (eboy-set-flag flags :C c)
            )
           (incf eboy-clock-cycles 4))

    ;; RRCA - Rotate A right. Old bit 0 to Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x0F (eboy-log (format " RRCA -/- "))
          (insert (format " %s " (logand eboy-rA #x01)) )
          (eboy-set-flag flags :C (= (logand eboy-rA #x01) 1))
          (setq eboy-rA (logand (logior (lsh eboy-rA 1) (lsh eboy-rA -7)) #xff))
          (eboy-set-flag flags :Z (= eboy-rA 0))
          (incf eboy-clock-cycles 4))

    ;; RRA - Rotate A right through Carry flag.
    ;; Flags affected:
    ;; Z - Set if result is zero.
    ;; N - Reset.
    ;; H - Reset.
    ;; C - Contains old bit 0 data.
    (#x1F (eboy-log (format " RRA -/- ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 4))

    ;; Extended instructions
    (#xCB (eboy-log (format " 2byte opcode CB: 0x%02x" (eboy-get-byte)))
          (let ((bc-opcode (eboy-get-byte)))
            (eboy-inc-pc 1)
            (cl-case bc-opcode
              ;; SWAP n - Swap upper & lower nibles of n.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Reset.
              (#x37 (eboy-log (format " SWAP A"))
                    (setq eboy-rA (logand (logior (lsh eboy-rA 4) (lsh eboy-rA -4)) #xFF))
                    (eboy-set-flag flags :Z (zerop eboy-rA))
                    (eboy-set-flag flags :N nil)
                    (eboy-set-flag flags :H nil)
                    (eboy-set-flag flags :C nil)
                    (incf eboy-clock-cycles 8))
              (#x30 (eboy-log (format " SWAP B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x31 (eboy-log (format " SWAP C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x32 (eboy-log (format " SWAP D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x33 (eboy-log (format " SWAP E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x34 (eboy-log (format " SWAP H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x35 (eboy-log (format " SWAP L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x36 (eboy-log (format " SWAP (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; RLC n - Rotate n left. Old bit 7 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x07 (eboy-log (format " RLC A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x00 (eboy-log (format " RLC B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x01 (eboy-log (format " RLC C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x02 (eboy-log (format " RLC D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x03 (eboy-log (format " RLC E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x04 (eboy-log (format " RLC H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x05 (eboy-log (format " RLC L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x06 (eboy-log (format " RLC (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; RL n - Rotate n left through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x17 (eboy-log (format " RL A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x10 (eboy-log (format " RL B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x11 (eboy-log (format " RL C"))
                    (let ((c (= (lsh eboy-rC -7) #x01)))
                      (setq eboy-rC (logior (lsh eboy-rC 1) (if (eboy-get-flag flags :C) #x01 #x00)))
                      (eboy-set-flag flags :Z (zerop eboy-rC))
                      (eboy-set-flag flags :N nil)
                      (eboy-set-flag flags :H nil)
                      (eboy-set-flag flags :C c)
                      )
                     (incf eboy-clock-cycles 8))
              (#x12 (eboy-log (format " RL D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x13 (eboy-log (format " RL E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x14 (eboy-log (format " RL H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x15 (eboy-log (format " RL L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x16 (eboy-log (format " RL (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; RRC n -  Rotate n right. Old bit 0 to Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x0F (eboy-log (format " RRC A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x08 (eboy-log (format " RRC B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x09 (eboy-log (format " RRC C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x0A (eboy-log (format " RRC D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x0B (eboy-log (format " RRC E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x0C (eboy-log (format " RRC H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x0D (eboy-log (format " RRC L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x0E (eboy-log (format " RRC (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; RR n - Rotate n right through Carry flag.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x1F (eboy-log (format " RR A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x18 (eboy-log (format " RR B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x19 (eboy-log (format " RR C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x1A (eboy-log (format " RR D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x1B (eboy-log (format " RR E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x1C (eboy-log (format " RR H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x1D (eboy-log (format " RR L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x1E (eboy-log (format " RR (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; SLA n - Shift n left into Carry. LSB of n set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 7 data.
              (#x27 (eboy-log (format " SLA A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x20 (eboy-log (format " SLA B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x21 (eboy-log (format " SLA C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x22 (eboy-log (format " SLA D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x23 (eboy-log (format " SLA E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x24 (eboy-log (format " SLA H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x25 (eboy-log (format " SLA L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x26 (eboy-log (format " SLA (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; SRA n - Shift n right into Carry. MSB doesn't change.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x2F (eboy-log (format " SRA A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x28 (eboy-log (format " SRA B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x29 (eboy-log (format " SRA C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x2A (eboy-log (format " SRA D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x2B (eboy-log (format " SRA E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x2C (eboy-log (format " SRA H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x2D (eboy-log (format " SRA L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x2E (eboy-log (format " SRA (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; SRL n - Shift n right into Carry. MSB set to 0.
              ;; Flags affected:
              ;; Z - Set if result is zero.
              ;; N - Reset.
              ;; H - Reset.
              ;; C - Contains old bit 0 data.
              (#x3F (eboy-log (format " SRL A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x38 (eboy-log (format " SRL B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x39 (eboy-log (format " SRL C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x3A (eboy-log (format " SRL D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x3B (eboy-log (format " SRL E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x3C (eboy-log (format " SRL H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x3D (eboy-log (format " SRL L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x3E (eboy-log (format " SRL (HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; Bit Opcodes - Test bit b in register r.
              ;; Flags affected:
              ;; Z - Set if bit b of register r is 0.
              ;; N - Reset.
              ;; H - Set.
              ;; C - Not affected.
              (#x47 (eboy-log (format " BIT b,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x40 (eboy-log (format " BIT b,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x41 (eboy-log (format " BIT b,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x42 (eboy-log (format " BIT b,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x43 (eboy-log (format " BIT b,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x44 (eboy-log (format " BIT b,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x45 (eboy-log (format " BIT b,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x46 (eboy-log (format " BIT b,(HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              (#x7C (eboy-log (format " BIT 7,H"))
                    (eboy-set-flag flags :Z (not (= (logand eboy-rH #x80) #x80)))
                    (eboy-set-flag flags :N nil)
                    (eboy-set-flag flags :H t)
                    (incf eboy-clock-cycles 8))

              ;; SET b,r - Set bit b in register r.
              ;; Flags affected:
              ;; None.
              (#xC7 (eboy-log (format " SET b,A")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC0 (eboy-log (format " SET b,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC1 (eboy-log (format " SET b,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC2 (eboy-log (format " SET b,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC3 (eboy-log (format " SET b,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC4 (eboy-log (format " SET b,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC5 (eboy-log (format " SET b,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#xC6 (eboy-log (format " SET b,(HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

              ;; RES b,r - Reset bit b in register r.
              ;; Flags affected:
              ;; None.
              (#x87 (eboy-log (format " RES b,A")) (setq eboy-rA (logand eboy-rA #xFE))  (incf eboy-clock-cycles 8))
              (#x80 (eboy-log (format " RES b,B")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x81 (eboy-log (format " RES b,C")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x82 (eboy-log (format " RES b,D")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x83 (eboy-log (format " RES b,E")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x84 (eboy-log (format " RES b,H")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x85 (eboy-log (format " RES b,L")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 8))
              (#x86 (eboy-log (format " RES b,(HL) ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
              (otherwise (eboy-log (assert nil t (format "Unimplemented BC opcode 0x%x" opcode))))
              )))


    ;; JP nn - Jump to address nn.
    (#xC3 (eboy-log (format " JP $%04x" (eboy-get-short)) )
          (setq eboy-pc (1- (eboy-get-short))) ;;Compensate for the pc +1 that is done with each instruction.
          (incf eboy-clock-cycles 16))

    ;; JP cc,nn - Jump to address n if following condition is true:
    ;;   cc = NZ, Jump if Z flag is reset.
    ;;   cc = Z, Jump if Z flag is set.
    ;;   cc = NC, Jump if C flag is reset.
    ;;   cc = C, Jump if C flag is set.
    ;; nn = two byte immediate value. (LS byte first.)
    (#xC2 (eboy-log (format " JP NZ,nn  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 12))
    (#xCA (eboy-log (format " JP Z,$%04x" (eboy-get-short)))
          (if (eboy-get-flag flags :Z)
              (progn (setq eboy-pc (1- (eboy-get-short)))
                     (incf eboy-clock-cycles 4)) ;; Compensate for the pc +1 that is done with each instruction.
            (eboy-inc-pc 2))
          (incf eboy-clock-cycles 12))
    (#xD2 (eboy-log (format " JP NC,nn  ")) (assert nil t "unimplemented opcode") ;;(incf eboy-clock-cycles 12)  or 16
          )
    (#xDA (eboy-log (format " JP C,nn  ")) (assert nil t "unimplemented opcode") ;;(incf eboy-clock-cycles 12)  or 16
          )

    ;; JP (HL) - Jump to address contained in HL.
    (#xE9 (eboy-log (format " JP (HL) "))
          (setq eboy-pc (1- (eboy-get-rHL))) (incf eboy-clock-cycles 4))

    ;; JR n - Add n to current address and jump to it.
    ;; nn = one byte signed immediate value
    (#x18 (eboy-log (format " JR %02d " (eboy-byte-to-signed (eboy-get-byte))))
          (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte)) 1))
          (incf eboy-clock-cycles 12))

    ;; JR cc,n - If following condition is true then add n to current address and jump to it:
    ;;  n = one byte signed immediate value
    ;;  cc = NZ, Jump if Z flag is reset.
    ;;  cc = Z, Jump if Z flag is set.
    ;;  cc = NC, Jump if C flag is reset.
    ;;  cc = C, Jump if C flag is set.
    (#x20 (eboy-log (format " JR NZ,* (%02d)" (eboy-byte-to-signed (eboy-get-byte))))
          (if (null (eboy-get-flag flags :Z))
              (progn (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
                     (incf eboy-clock-cycles 4)))
          (incf eboy-pc)
          (incf eboy-clock-cycles 8))
    (#x28 (eboy-log (format " JR Z,* "))
          (if (eboy-get-flag flags :Z)
              (progn (setq eboy-pc (+ eboy-pc (eboy-byte-to-signed (eboy-get-byte))))
                     (incf eboy-clock-cycles 4)))
          (incf eboy-pc)
          (incf eboy-clock-cycles 8))
    (#x30 (eboy-log (format " JR NC,* ")) (assert nil t "unimplemented opcode") ;; or 12 (incf eboy-clock-cycles 8)
          )
    (#x38 (eboy-log (format " JR C,* ")) (assert nil t "unimplemented opcode") ;; or 12(incf eboy-clock-cycles 8)
          )


      ;;; Calls

    ;; CALL nn - Push address of next instruction onto stack and then jump to address nn.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xCD (eboy-log (format " CALL 0x%0004x " (eboy-get-short)))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (+ eboy-pc 3))
          (setq eboy-pc (1- (eboy-get-short))) ;; decrement one, since it will be incremented next.
          (incf eboy-clock-cycles 24))

    ;; CALL cc,nn - Call address n if following condition is true:
    ;;  cc = NZ, Call if Z flag is reset.
    ;;  cc = Z, Call if Z flag is set.
    ;;  cc = NC, Call if C flag is reset.
    ;;  cc = C, Call if C flag is set.
    ;;  nn = two byte immediate value. (LS byte first.)
    (#xC4 (eboy-log (format " CALL NZ,nn  ")) (assert nil t "unimplemented opcode") ;; or 24(incf eboy-clock-cycles 12)
          )
    (#xCC (eboy-log (format " CALL Z,nn  ")) (assert nil t "unimplemented opcode") ;; or 24(incf eboy-clock-cycles 12)
          )
    (#xD4 (eboy-log (format " CALL NC,nn  ")) (assert nil t "unimplemented opcode") ;; or 24(incf eboy-clock-cycles 12)
          )
    (#xDC (eboy-log (format " CALL C,nn  ")) (assert nil t "unimplemented opcode") ;; or 24(incf eboy-clock-cycles 12)
          )


      ;;; Restarts

    ;; RST n - Push present address onto stack. Jump to address $0000 + n.
    ;;  n = $00,$08,$10,$18,$20,$28,$30,$38
    (#xC7 (eboy-log (format " RST 00H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xCF (eboy-log (format " RST 08H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xD7 (eboy-log (format " RST 10H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xDF (eboy-log (format " RST 18H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xE7 (eboy-log (format " RST 20H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xEF (eboy-log (format " RST 28H  "))
          (decf eboy-sp 2)
          (eboy-mem-write-short eboy-sp (+ eboy-pc 1))
          (setq eboy-pc (1- #x28)) ;; Decrement one since it will be incremented next.
          (incf eboy-clock-cycles 16))
    (#xF7 (eboy-log (format " RST 30H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))
    (#xFF (eboy-log (format " RST 38H  ")) (assert nil t "unimplemented opcode") (incf eboy-clock-cycles 16))

      ;;; Returns

    ;; RET - Pop two bytes from stack & jump to that address.
    (#xC9 (eboy-log (format " RET -/- (%0004x)" (eboy-mem-read-short eboy-sp)))
          (setq eboy-pc (1- (eboy-mem-read-short eboy-sp))) ;; decrement one, since it will be incremented next.
          (incf eboy-sp 2)
          (incf eboy-clock-cycles 16))

    ;; RET cc - Return if following condition is true:
    ;;  cc = NZ, Return if Z flag is reset.
    ;;  cc = Z, Return if Z flag is set.
    ;;  cc = NC, Return if C flag is reset.
    ;;  cc = C, Return if C flag is set.
    (#xC0 (eboy-log (format " RET NZ "))
          (if (null (eboy-get-flag flags :Z))
              (progn
                (setq eboy-pc (1- (eboy-mem-read-short eboy-sp))) ;; decrement one, since it will be incremented next.
                (incf eboy-sp 2)
                (incf eboy-clock-cycles 12)))
          (incf eboy-clock-cycles 8))
    (#xC8 (eboy-log (format " RET Z "))
          (if (eboy-get-flag flags :Z)
              (progn
                (setq eboy-pc (1- (eboy-mem-read-short eboy-sp))) ;; decrement one, since it will be incremented next.
                (incf eboy-sp 2)
                (incf eboy-clock-cycles 12)))
          (incf eboy-clock-cycles 8))
    (#xD0 (eboy-log (format " RET NC ")) (assert nil t "unimplemented opcode") ; 20 or (incf eboy-clock-cycles 8)
          )
    (#xD8 (eboy-log (format " RET C ")) (assert nil t "unimplemented opcode") ; 20 or (incf eboy-clock-cycles 8)
          )

    ;; RETI - Pop two bytes from stack & jump to that address then enable interrupts.
    (#xD9 (eboy-log (format " RETI -/- " ))
          (setq eboy-pc (1- (eboy-mem-read-short eboy-sp))) ;; decrement one, since it will be incremented next.
          (incf eboy-sp 2)
          (setq eboy-interrupt-master-enbl t)
          (incf eboy-clock-cycles 16)
          (setq eboy-delay-enabling-interrupt-p t))

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
  ;; (if (= eboy-pc #x235)
  ;;     (progn (message "indicate the V-Blank moment")
  ;;            (eboy-mem-write-byte #xFF44 #x94)))
  ;; (if (= eboy-pc #x282c)
  ;;     (progn (message "Set LCDC Y coordinate to V-Blank range 0x90-0x99")
  ;;            (eboy-mem-write-byte #xFF44 #x91)))
  ;; (if (= eboy-pc #x68) (eboy-mem-write-byte #xFF44 #x90) )
  ; (if (> eboy-pc eboy-debug-pc-max)
  ;     (progn
  ;       (insert (format "instr: %x pc: %x\n" eboy-debug-nr-instructions eboy-pc))
  ;       (setq eboy-debug-pc-max eboy-pc)))
;;  (if (= eboy-pc #x91) (setq eboy-debug-nr-instructions 99999999999) )
  )

;;(message (format "%x" (logand #xFF (lognot eboy-im-vblank))))
(defun eboy-disable-interrupt (interrupt-mask)
  "Remove the interrupt flag for INTERRUPT-MASK."
  (setq eboy-interrupt-pending (logand eboy-interrupt-pending (lognot interrupt-mask)))
  )

(defun eboy-process-interrupt (address)
  "Disable interrupts, put PC on stack and set PC to ADDRESS."
  (setq eboy-interrupt-master-enbl nil)
  (decf eboy-sp 2)
  (eboy-mem-write-short eboy-sp eboy-pc)
  (setq eboy-pc address)
  )

(defun eboy-process-interrupts ()
  "Process pending interrupts."
  (if eboy-delay-enabling-interrupt-p
      (setq eboy-delay-enabling-interrupt-p nil)
    (let ((enbl-intr (logand eboy-interrupt-enabled  eboy-interrupt-pending)))
      (if (and eboy-interrupt-master-enbl (> enbl-intr #x00))
          (progn (eboy-msg "handle enbl interrupt")
                 (cond
                  ((= 1 (logand enbl-intr eboy-im-vblank))
                   (eboy-msg " V-Blank")
                   (eboy-disable-interrupt eboy-im-vblank)
                   (eboy-process-interrupt #x40))
                  ((= 1 (logand enbl-intr eboy-im-lcdc))
                   (eboy-msg " LCDC")
                   (eboy-disable-interrupt eboy-im-lcdc)
                   (eboy-process-interrupt #x48))
                  ((= 1 (logand enbl-intr eboy-im-tmr-overflow))
                   (eboy-msg " Timer Overflow")
                   (eboy-disable-interrupt eboy-im-tmr-overflow)
                   (eboy-process-interrupt #x50))
                  ((= 1 (logand enbl-intr eboy-im-s-trans-compl))
                   (eboy-msg " Serial I/O transfer complete")
                   (eboy-disable-interrupt eboy-im-s-trans-compl)
                   (eboy-process-interrupt #x58))
                  ((= 1 (logand enbl-intr eboy-im-h2l-pins))
                   (eboy-msg " transition from h to l of pin P10-P13")
                   (eboy-disable-interrupt eboy-im-h2l-pins)
                   (eboy-process-interrupt #x60))
                  )))
      ))
  )
(defvar eboy-flags nil "Temp storage flags.")

(defun eboy-load-rom ()
  "Load the rom file.  For now just automatically load a test rom."
  (interactive)
  (if nil
      (progn
        (setq eboy-boot-rom-filename "boot/DMG_ROM.bin")
        (setq eboy-boot-rom (vconcat (eboy-read-bytes eboy-boot-rom-filename)))
        (setq eboy-boot-rom-disabled-p nil)
        (setq eboy-pc 0))
    (setq eboy-pc eboy-pc-start-address)
    (setq eboy-sp eboy-sp-initial-value)
    (eboy-init-registers)
    (eboy-init-memory)
    (setq eboy-boot-rom-disabled-p t)
    )
  (setq eboy-rom-filename "roms/test_rom.gb")
  ;;(setq eboy-rom-filename "cpu_instrs/individual/01-special.gb")
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))
  (setq eboy-clock-cycles 0)
  (setq eboy-debug-nr-instructions 0)
  (setq eboy-debug-pc-max 0)
  (setq eboy-lcd-ly 0)
  (switch-to-buffer "*eboy*")
  (switch-to-buffer "*eboy-display*")
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

    ;(while (< eboy-debug-nr-instructions 50) ;(or (<= eboy-pc #x100) )
    ;  (eboy-process-interrupts)
    ;  ;;(if eboy-debug-1 (insert (format "%2d: " eboy-debug-nr-instructions)))
    ;  (eboy-process-opcode (eboy-mem-read-byte eboy-pc) flags)
    ;  (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1)))

    ;; save flags
    (setq eboy-flags flags)
    )
  (message "we finished? eboy-pc: %x" eboy-pc)
  )

(defun eboy-debug-step ()
  "docstring"
  (interactive)
  (switch-to-buffer "*eboy-debug*")
  (eboy-process-opcode (eboy-mem-read-byte eboy-pc) eboy-flags)
  (eboy-process-interrupts)
  (eboy-lcd-cycle)
  (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1))
  )

(defun eboy-debug-step-nr-of-times (nr)
  "docstring"
  ;(interactive "nNr of steps: ")
  (switch-to-buffer "*eboy-debug*")
  (while (/= nr 0) ;(or (<= eboy-pc #x100) )
    ;(switch-to-buffer "*eboy-debug*")
    (eboy-process-opcode (eboy-mem-read-byte eboy-pc) eboy-flags)
    ;;(funcall (nth (eboy-mem-read-byte eboy-pc) eboy-cpu))
    ;;(eboy-inc-pc 1)
    (eboy-process-interrupts)
    (eboy-lcd-cycle)
    (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1))
    (decf nr)
    )
  (message "stopped! left: %d" nr)
  )

(defun eboy-debug-break-at-pc-addr (pc-addr)
  "docstring"
  (interactive "npc address: ")
  ;;  (setq eboy-debug-1 t)
  (switch-to-buffer "*eboy-debug*")
  (while (/= pc-addr eboy-pc ) ;(or (<= eboy-pc #x100) )
    ;(switch-to-buffer "*eboy-debug*")
    (eboy-process-opcode (eboy-mem-read-byte eboy-pc) eboy-flags)
    (eboy-process-interrupts)
    (eboy-lcd-cycle)
    (setq eboy-debug-nr-instructions (+ eboy-debug-nr-instructions 1))
    )
  (message "break at opcode %02x\n" pc-addr)
  )

(defun eboy-debug-toggle-verbosity ()
  "Toggle debug verbosity level."
  (interactive)
  (cond ((not eboy-debug-1)
         (setq eboy-debug-1 t)
         (message "Verbosity level 1"))
        ((and eboy-debug-1 (not eboy-debug-2))
         (setq eboy-debug-2 t)
         (message "Verbosity level 2"))
        ((and eboy-debug-1 eboy-debug-2)
         (setq eboy-debug-1 nil)
         (setq eboy-debug-2 nil)
         (message "Verbosity level 0"))
        ))


(setq eboy-debug-1 nil)
(setq eboy-debug-2 nil)
(eboy-load-rom)


(defvar eboy-mode-map nil "Keymap for `eboy-mode'.")

(progn
  (setq eboy-mode-map (make-sparse-keymap))
  (define-key eboy-mode-map (kbd "<f5>") 'eboy-debug-step)
  (define-key eboy-mode-map (kbd "f") 'eboy-print-registers)
  (define-key eboy-mode-map (kbd "v") 'eboy-debug-toggle-verbosity)
  ;; by convention, major mode's keys should begin with the form C-c C-‹key›
  ;; by convention, keys of the form C-c ‹letter› are reserved for user. don't define such keys in your major mode
  )

;;(define-minor-mode eboy-mode
;;  "Eboy"
;;  :lighter " eboy"
;;  :keymap (let ((map (make-sparse-keymap)))
;;            (define-key map (kbd "C-c t") 'step)
;;            map))


(define-derived-mode eboy-mode text-mode "eboy"
  "eboy-mode is a major mode for editing language my.

\\{eboy-mode-map}"

  ;; actually no need
  (use-local-map eboy-mode-map) ; if your keymap name is modename follow by -map, then this line is not necessary, because define-derived-mode will find it and set it for you

  )

(provide 'eboy-mode)
;;; eboy.el ends here

;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;;; Commentary:
(eval-when-compile (require 'cl))
;;(require 'cl-lib)


;;; Code:
(require 'eboy-cpu)

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
(defvar eboy-clock-cycles 0 "Number of elapsed clock cycles.") ;; TODO: when to reset?
(defvar eboy-display-write-done nil "Indicate if at line 143 we need to write the display.")
(defvar eboy-lcd-ly nil "The LCDC Y Coordinate.")
(defvar eboy-lcd-scrollx nil "The LCDC Scroll X register.")
(defvar eboy-lcd-scrolly nil "The LCDC Scroll Y register.")

;; LCD Control Register
(defvar eboy-lcdc-display-enable nil "LCD Control register - lcd display enabled.")
(defvar eboy-lcdc-window-tile-map nil "LCD Control register - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF).")
(defvar eboy-lcdc-window-display-enable nil "LCD Control register - Window display enabled.")
(defvar eboy-lcdc-bg-window-tile nil "LCD Control register - BG & Window Tile Data select.")
(defvar eboy-lcdc-bg-tile-map-display nil "LCD Control register - BG Tile Map Display Select.")
(defvar eboy-lcdc-obj-size nil "LCD Control register - OBJ (sprite) Size.")
(defvar eboy-lcdc-obj-disp-en nil "LCD Control register - OBJ (Sprite) Display Enable.")
(defvar eboy-lcdc-bg-enable nil "LCD Control register - BG Display.")


(defvar eboy-delay-enabling-interrupt-p nil "Enabling interrupts is delayd with one instruction.")

;;(defvar eboy-flags (make-bool-vector 4 t) "The flags Z(Zero) S(Negative) H(Halve Carry) and C(Carry).")
(defvar eboy-debug-nr-instructions nil "Number of instructions executed.")
(defvar eboy-debug-1 t "Enable debugging info.")
(defvar eboy-debug-2 nil "Enable debugging info.")
(defvar eboy-debug-pc-max nil "The program counter max.")
(defvar eboy-debug-fps-timestamp (time-to-seconds (current-time)) "The FPS calculation.")
(defvar eboy-debug-nr-of-frames 0 "The number of frames processed since FPS calculation.")
(defvar eboy-debug-last-log-line "" "The last log line.")

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
        ;; if lcd is on then eboy-lcd-y, else 0
        (setq data (if eboy-lcdc-display-enable eboy-lcd-ly 0)))
       ((= address #xFF43) (eboy-msg "Read SCX: Scroll X.")
        (setq data eboy-lcd-scrollx))
       ((= address #xFF42) (eboy-msg "Read SCY: Scroll Y.")
        (setq data eboy-lcd-scrolly))
       ((= address #xFF41) (eboy-msg "Read STAT: LCDC Status.") )
       ((= address #xFF40) (eboy-msg "Read LCDC: LCD Control.")
        (setq data (logior (lsh (if eboy-lcdc-display-enable 1 0) -7)
                           (lsh (if eboy-lcdc-window-tile-map 1 0) -6)
                           (lsh (if eboy-lcdc-window-display-enable 1 0) -5)
                           (lsh (if eboy-lcdc-bg-window-tile 1 0) -4)
                           (lsh (if eboy-lcdc-bg-tile-map-display 1 0) -3)
                           (lsh (if eboy-lcdc-obj-size 1 0) -2)
                           (lsh (if eboy-lcdc-obj-disp-en 1 0) -1)
                           (if eboy-lcdc-bg-enable 1 0))))
       ;; in between sound registers, but not consecutive, some unknow address.
       ((= address #xFF0F)
        (eboy-msg "Read IF: Interrupt Flag.")
        (setq data eboy-interrupt-pending))
       ((= address #xFF07) (eboy-msg "Read TAC: Timer Control.") )
       ((= address #xFF06) (eboy-msg "Read TMA: Timer Modulo.") )
       ((= address #xFF05) (eboy-msg "Read TIMA: Timer Counter.") )
       ((= address #xFF04) (eboy-msg "Read DIV: Divider Register.") )
       ((= address #xFF02) (eboy-msg "Read SC: SIO control.") )
       ((= address #xFF01) (eboy-msg "Read SB: Serial transfer data.") )
       ((= address #xFF00) (eboy-msg "Read P1: Joy Pad info and system type register.")
        (cond
         ((= (logand (lognot data) #x10) #x10) (eboy-msg "Direction keys selected") (setq data #x0F))
         ((= (logand (lognot data) #x20) #x20) (eboy-msg "Button keys selected") (setq data #x0F)))))
      data))

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
     ((= address #xFF40) (eboy-msg "Write LCDC: LCD Control.")
      (setq eboy-lcdc-display-enable (logand data #x80) )
      (setq eboy-lcdc-window-tile-map (logand data #x40) )
      (setq eboy-lcdc-window-display-enable (logand data #x20) )
      (setq eboy-lcdc-bg-window-tile (logand data #x10) )
      (setq eboy-lcdc-bg-tile-map-display (logand data #x08) )
      (setq eboy-lcdc-obj-size (logand data #x04) )
      (setq eboy-lcdc-obj-disp-en (logand data #x02) )
      (setq eboy-lcdc-bg-enable (logand data #x01) )
      )
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
  (redisplay))

(defun eboy-write-display-unicode ()
  "Write the display as unicode characters."
  (interactive)
  (with-current-buffer "*eboy-display*")
  (erase-buffer)
  (dotimes (y 144)
    (dotimes (x 160)
      (insert (gethash (eboy-get-color-xy (mod (+ x eboy-lcd-scrollx) 256) (mod (+ y eboy-lcd-scrolly) 256)) eboy-display-unicode-table)))
    (insert "\n"))
  (goto-char (point-min))
  (redisplay))

(defun eboy-lcd-cycle ()
  "Perform a single lcd cycle."
  (if eboy-lcdc-display-enable
      (let ((screen-cycle (mod eboy-clock-cycles 70224)))
        (setq eboy-lcd-ly (/ screen-cycle 456))
        (when (and (= eboy-lcd-ly 143) (not eboy-display-write-done))
          ;; (eboy-write-display-unicode)
          (eboy-debug-update-fps)
          (setq eboy-display-write-done t))
        (when (and (= eboy-lcd-ly 144) eboy-display-write-done)
          (setq eboy-interrupt-pending (logior eboy-interrupt-pending eboy-im-vblank))
          (setq eboy-display-write-done nil)
          ))))
(defun eboy-debug-update-fps ()
  "Update the Frames Per Second counter."
  (incf eboy-debug-nr-of-frames)
  (when (> (time-to-seconds (current-time)) (+ 2 eboy-debug-fps-timestamp))
    (setq eboy-debug-fps-timestamp (time-to-seconds (current-time)))
    (message "FPS: %d" (/ eboy-debug-nr-of-frames 2))
    (setq eboy-debug-nr-of-frames 0)
    (eboy-write-display-unicode)
    ))
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
  (if eboy-cpu-halted
      (eboy-inc-pc 1)

    (setq eboy-debug-last-log-line (format "0x%x, pc: 0x%x, opcode: 0x%02x, f: 0x%02x A: 0x%02x B: 0x%02x C: 0x%02x D: 0x%02x E: 0x%02x H: 0x%02x L: 0x%02x" eboy-clock-cycles eboy-pc opcode (eboy-flags-to-byte eboy-flags) eboy-rA eboy-rB eboy-rC eboy-rD eboy-rE eboy-rH eboy-rL))

    (when (> eboy-debug-nr-instructions 629290)
        (if eboy-debug-1 (insert eboy-debug-last-log-line)))
    ;;(if eboy-debug-1 (insert (format "\npc: 0x%x, opcode: 0x%02x" eboy-pc opcode)))
    ;;(if eboy-debug-1 (insert (format "\n0x%02x" eboy-pc)))
    ;;(if eboy-debug-1 (insert (format "\n0x%x, pc: 0x%x, opcode: 0x%02x, f: 0x%02x" eboy-clock-cycles eboy-pc opcode (eboy-flags-to-byte eboy-flags))))
    (funcall (nth opcode eboy-cpu))
    (eboy-inc-pc 1))
  )

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
  (if t
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
  ;;(setq eboy-rom-filename "cpu_instrs/cpu_instrs.gb")
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))
  (setq eboy-clock-cycles 0)
  (setq eboy-debug-nr-instructions 0)
  (setq eboy-debug-pc-max 0)
  (setq eboy-lcd-ly 0)
  ;;(switch-to-buffer "*eboy*")
  (switch-to-buffer "*eboy-display*")
  ;;(erase-buffer)
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
  ;;(switch-to-buffer "*eboy-debug*")
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

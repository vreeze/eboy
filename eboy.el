;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Pieter de Vreeze <mail@de-vreeze.com>

;; Author: Pieter de Vreeze <mail@de-vreeze.com>
;; URL: https://github.com/vreeze/eboy
;; Version: 0.0.1
;; Package-Requires:
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This is a Gameboy emulator for Emacs -> eboy

(eval-when-compile (require 'cl))
;;(require 'cl-lib)


;;; Code:
(require 'eboy-macros)
(require 'eboy-cpu)

(defgroup eboy nil
  "A Gameboy emulator"
  :group 'games
  :prefix "eboy-")

(defcustom eboy-skip-frames 20
  "The number of frames to skip before displaying."
  :type 'integer
  :group 'eboy)

(defun eboy-read-bytes (path)
  "Read binary data from PATH.  Return the binary data as unibyte string."
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

(defconst eboy-debug-show-fps-after-sec 10.0 "The number of seconds between FPS displaying.")

(defvar eboy-rom-filename nil "The file name of the loaded rom.")
(defvar eboy-boot-rom-filename nil "The path to the boot rom.")
(defvar eboy-boot-rom nil "The binary vector of the boot rom.")
(defvar eboy-low-rom nil "The low rom keeps a copy the first 256 bytes of the game rom, to be copied after boot.")
(defvar eboy-rom nil "The binary vector of the rom.")
(defvar eboy-rom-size nil "The size of the rom in bytes.")
(defvar eboy-pc nil "The program counter.")
(defvar eboy-sp nil "The stack pointer.")
(defvar eboy-flags nil "The flags register.")
(defvar eboy-interrupt-master-enbl nil "Interupts master enabled flag.")
(defvar eboy-interrupt-pending #x00 "Flags of pending interrupts.")
(defvar eboy-interrupt-enabled #x00 "Flags of enabled interrupts.")
(defvar eboy-clock-cycles 0 "Number of elapsed clock cycles.") ;; TODO: when to reset?
(defvar eboy-display-write-done nil "Indicate if at line 143 we need to write the display.")
(defvar eboy-lcd-ly nil "The LCDC Y Coordinate.")
(defvar eboy-lcd-scrollx nil "The LCDC Scroll X register.")
(defvar eboy-lcd-scrolly nil "The LCDC Scroll Y register.")

;; LCD Control Register
(defvar eboy-lcdc-display-enable nil "LCD Control register - lcd display enabled.")
(defvar eboy-lcdc-window-tile-map-select nil "LCD Control register - Window Tile Map Display Select (nil=9800-9BFF, t=9C00-9FFF).")
(defvar eboy-lcdc-window-display-enable nil "LCD Control register - Window display enabled.")
(defvar eboy-lcdc-bg-&-window-tile-data-select nil "LCD Control register - BG & Window Tile Data select (nil: 8800-97FF, t:8000-8FFF).")
(defvar eboy-lcdc-bg-tile-map-display-select nil "LCD Control register - BG Tile Map Display Select (nil: 9800-9BFF, t:9C00-9FFF).")
(defvar eboy-lcdc-obj-size nil "LCD Control register - OBJ (sprite) Size.  (nil: 8*8, t: 8*16).")
(defvar eboy-lcdc-obj-disp-on nil "LCD Control register - OBJ (Sprite) Display On.")
(defvar eboy-lcdc-bg-window-on nil "LCD Control register - BG & Window Display On.")


(defvar eboy-delay-enabling-interrupt-p nil "Enabling interrupts is delayd with one instruction.")

(defvar eboy-timer-cycles 0 "Timer cycle counter.")

;;(defvar eboy-flags (make-bool-vector 4 t) "The flags Z(Zero) S(Negative) H(Halve Carry) and C(Carry).")

(defvar eboy-debug nil "Enable debugging info.")
(defvar eboy-debug-fps-timestamp (time-to-seconds (current-time)) "The FPS calculation.")
(defvar eboy-debug-nr-of-frames 0 "The number of skipped frames since last display.")
(defvar eboy-debug-nr-of-displayed-frames 0 "The number of frames processed since FPS calculation.")

(defvar eboy-rA 0 "Register A.")
(defvar eboy-rB 0 "Register B.")
(defvar eboy-rC 0 "Register C.")
(defvar eboy-rD 0 "Register D.")
(defvar eboy-rE 0 "Register E.")
;;(defvar eboy-rF 0 "Register F.") flags
(defvar eboy-rH 0 "Register H.")
(defvar eboy-rL 0 "Register L.")

;; JoyPad
(defvar eboy-joypad-direction-keys #xF "The direction keys of the joypad, |Down|Up|Left|Right| (0=pressed).")
(defvar eboy-joypad-button-keys #xF "The button keys of the joypad, |Start|Select|B|A| (0=pressed).")

(defun eboy-byte-to-signed (byte)
  "Convert BYTE to signed number."
  ;;(* (1+ (logxor byte #xFF)) -1)
  (if (not (zerop (logand #x80 byte)))
      (setq byte (- byte #x100)))
  byte)

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
  (setq eboy-rL #x4d))

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
  (eboy-mem-write-byte #xFFFF #x00)) ; IE


(defun eboy-log (logstring)
  "Log string LOGSTRING."
  (if eboy-debug (message logstring)))

(defvar eboy-ram (make-vector (* 8 1024) 0) "The 8 kB interal RAM.")
(defvar eboy-sram (make-vector (* 8 1024) 0) "The 8kB switchable RAM bank.")
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
  ;; most frequent accessed memory on top.
  (cond
   ((and (>= address #x0000) (< address #x8000))
    ;; 32kB Cartidge
    (aref eboy-rom address));; 16kB ROM bank #0 & 16kB switchable ROM bank. The lower 256 bytes contain either the boot-rom or the first 256 bytes of the rom.

   ((and (>= address #xFF80) (<= address #xFFFF))
      ;;(message "Internal RAM")
      (if (= address #xFFFF)
          eboy-interrupt-enabled
        (aref eboy-hram (- address #xFF80))))
     ((and (>= address #xFF4C) (< address #xFF80))
      ;;(message "Empty but unusable for I/O")
      (aref eboy-ram (- address #xE000)))
     ((and (>= address #xFF00) (< address #xFF4C))
      ;;(message "I/O ports")
      (let ((data (aref eboy-io (- address #xFF00))))
        (cond
         ((= address #xFF4B) ) ;; Read WX: Window X position.
         ((= address #xFF4A) ) ;; Read WY: Window Y position.
         ((= address #xFF49) ) ;; Read OBP1: Object Palette 1 Data.
         ((= address #xFF48) ) ;; Read OBPO: Object Palette 0 Data.
         ((= address #xFF47) ) ;; Read BGP: BG & Window Palette DATA.
         ;;((= address #xFF46)) ;; Read DMA: DMA Transfer and Start Address
         ((= address #xFF45) ) ;; Read LYC: LY Compare.
         ((= address #xFF44)  ;; Read LY: LCDC Y Coordinate
          ;; if lcd is on then eboy-lcd-y, else 0
          (setq data (if eboy-lcdc-display-enable eboy-lcd-ly 0)))
         ((= address #xFF43) ;; Read SCX: Scroll X.
          (setq data eboy-lcd-scrollx))
         ((= address #xFF42) ;; SCY: Scroll Y.
          (setq data eboy-lcd-scrolly))
         ((= address #xFF41) ) ;; Read STAT: LCDC Status.
         ((= address #xFF40)  ;;  Read LCDC: LCD Control.
          (setq data (logior (lsh (if eboy-lcdc-display-enable 1 0) 7)
                             (lsh (if eboy-lcdc-window-tile-map-select 1 0) 6)
                             (lsh (if eboy-lcdc-window-display-enable 1 0) 5)
                             (lsh (if eboy-lcdc-bg-&-window-tile-data-select 1 0) 4)
                             (lsh (if eboy-lcdc-bg-tile-map-display-select 1 0) 3)
                             (lsh (if eboy-lcdc-obj-size 1 0) 2)
                             (lsh (if eboy-lcdc-obj-disp-on 1 0) 1)
                             (if eboy-lcdc-bg-window-on 1 0))))
         ;; in between sound registers, but not consecutive, some unknow address.
         ((= address #xFF0F)
          ;;Read IF: Interrupt Flag.
          (setq data eboy-interrupt-pending))
         ((= address #xFF07) ) ;; Read TAC: Timer Control.
         ((= address #xFF06) ) ;; Read TMA: Timer Modulo.
         ((= address #xFF05) ) ;; Read TIMA: Timer Counter.
         ((= address #xFF04) ) ;; Read DIV: Divider Register.
         ((= address #xFF02) ) ;; Read SC: SIO control.
         ((= address #xFF01) ) ;; Read SB: Serial transfer data.
         ((= address #xFF00) ;; Read P1: Joy Pad info and system type register.
          (cond
           ((= (logand (lognot data) #x10) #x10) (setq data (logior #xE0 eboy-joypad-direction-keys)))
           ((= (logand (lognot data) #x20) #x20) (setq data (logior #xD0 eboy-joypad-button-keys))))))
        data))

     ((and (>= address #xFEA0) (< address #xFF00))
      ;;(message "Empty but unusable for I/O")
      (aref eboy-ram (- address #xE000)))
     ((and (>= address #xFE00) (< address #xFEA0))
      ;;(message "Sprite Attrib Memory (OAM)")
      (aref eboy-ram (- address #xE000)))
     ((and (>= address #xE000) (< address #xFE00))
      ;;(message "Echo of 8kB Internal RAM")
      (aref eboy-ram (- address #xE000)))
     ((and (>= address #xC000) (< address #xE000))
      ;;(message "8kB Internal RAM")
      (aref eboy-ram (- address #xC000)))
     ((and (>= address #xA000) (< address #xC000))
      ;;(message "8kB switchable RAM bank")
      (aref eboy-sram (- address #xA000)))
     ((and (>= address #x8000) (< address #xA000))
      ;;(message "8kB Video RAM")
      (aref eboy-vram (- address #x8000)))
     ))

(defun eboy-mem-write-byte (address data)
  "Write DATA byte to memory at ADDRESS."
  (setq data (logand data #xff))

  (cond
   ((= address #xFFFF) (setq eboy-interrupt-enabled data))
   ((and (>= address #xFF80) (<= address #xFFFF)) (aset eboy-hram (- address #xFF80) data))
   ((and (>= address #xFF4C) (< address #xFF80)) (aset eboy-ram (- address #xE000) data))
   ((and (>= address #xFF00) (< address #xFF4C))
    ;; (message "Write I/O ports")
    (cond
     ((= address #xFF4B)  ) ;; Write WX: Window X position.
     ((= address #xFF4B)  ) ;; Write WX: Window X position.
     ((= address #xFF4A)  ) ;; Write WY: Window Y position.
     ((= address #xFF50) ;; Write: Disable boot rom.
      ;; on boot it writes 0x01, lets disable it always
      (setq eboy-rom (concat eboy-low-rom (seq-drop eboy-rom (length eboy-low-rom)))))
     ((= address #xFF49)  ) ;; Write OBP1: Object Palette 1 Data.
     ((= address #xFF48)  ) ;; Write OBPO: Object Palette 0 Data.
     ((= address #xFF47)  ) ;; Write BGP: BG & Window Palette DATA.
     ((= address #xFF46) ;; Write DMA: DMA Transfer and Start Address.
      (let ((byte_nr 0)
            (base_address (lsh data 8)))
        (while (< byte_nr #xA0)
          (eboy-mem-write-byte (+ #xFE00 byte_nr) (eboy-mem-read-byte (+ base_address byte_nr)))
          (incf byte_nr))))
     ((= address #xFF45)  ) ;; Write LYC: LY Compare.
     ((= address #xFF44) ;; Write LY: Reset the LCDC Y Coordinate.
      (setq eboy-lcd-ly 0))
     ((= address #xFF43) ;; Write SCX: Scroll X.
      (setq eboy-lcd-scrollx data))
     ((= address #xFF42) ;; Write SCY: Scroll Y.
      (setq eboy-lcd-scrolly data))
     ((= address #xFF41)  ) ;; Write STAT: LCDC Status.
     ((= address #xFF40) ;; Write LCDC: LCD Control.
      (setq eboy-lcdc-display-enable (= (logand data #x80) #x80))
      (setq eboy-lcdc-window-tile-map-select (= (logand data #x40) #x40) )
      (setq eboy-lcdc-window-display-enable (= (logand data #x20) #x20) )
      (setq eboy-lcdc-bg-&-window-tile-data-select (= (logand data #x10) #x10) )
      (setq eboy-lcdc-bg-tile-map-display-select (= (logand data #x08) #x08) )
      (setq eboy-lcdc-obj-size (= (logand data #x04) #x04) )
      (setq eboy-lcdc-obj-disp-on (= (logand data #x02) #x02) )
      (setq eboy-lcdc-bg-window-on (= (logand data #x01) #x01) ))
     ;; in between sound registers, but not consecutive, some unknow address.
     ((= address #xFF0F) (setq eboy-interrupt-pending data))
     ((= address #xFF07)  ) ;; Write TAC: Timer Control.
     ((= address #xFF06)  ) ;; Write TMA: Timer Modulo.
     ((= address #xFF05)  ) ;; Write TIMA: Timer Counter.
     ((= address #xFF04)  ) ;; Write DIV: Divider Register.
     ((= address #xFF02)  ) ;; Write SC: SIO control.
     ((= address #xFF01)  ) ;; Write SB: Serial transfer data.
     ((= address #xFF00) )) ;; Write P1: Joy Pad info and system type register.
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
    (aset eboy-sram (- address #xA000) data))
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
    )))

(defun eboy-mem-write-short (address data)
  "Write DATA short to memory at ADDRESS.
Little Endian."
  (eboy-mem-write-byte address (logand data #xFF))
  (eboy-mem-write-byte (1+ address) (logand (lsh data -8) #xFF)))

(defun eboy-mem-read-short (address)
  "Read short from memory at ADDRESS.
Little Endian."
  (logior (eboy-mem-read-byte address) (lsh (eboy-mem-read-byte (1+ address)) 8)))

;; This can be done in a faster way, ecase is slow.
(defun eboy-set-flag (flags flag state)
  "Set FLAG in FLAGS to STATE."
  (ecase flag
    (:C (aset flags 0 state))
    (:H (aset flags 1 state))
    (:N (aset flags 2 state))
    (:Z (aset flags 3 state))))

(defun eboy-get-flag (flags flag)
  "Get FLAG from FLAGS."
  (ecase flag
    (:C (aref flags 0))
    (:H (aref flags 1))
    (:N (aref flags 2))
    (:Z (aref flags 3))))

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
    byte-flags))

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
    flags))

(defun eboy-rom-title ()
  "Retrieve the title of the game from the loaded rom."
  (let ((title ""))
    (dotimes (i 16)
      (let ((c (aref eboy-rom (+ i #x134))))
        (unless (equal c 0)
            (setq title (concat title (format "%c" c))))))
    title))


(defun eboy-get-short ()
  "Skip opcode and get next two bytes."
  (eboy-mem-read-short (+ eboy-pc 1)))

(defun eboy-get-byte ()
  "Skip opcode and get next byte."
  (eboy-mem-read-byte (+ eboy-pc 1)))

(defun eboy-inc-pc (nr-bytes)
  "Increment program counter with NR-BYTES."
  (setq eboy-pc (+ eboy-pc nr-bytes)))

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

;;;;;
;;;; JOYPAD section
;;;;;
;; +++++++++ sy
;; |
;; ----- Y
;; |
;; +++++++++ sy + spritesize
;;;;;
;;;; DISPLAY section
;;;;;

(defun eboy-display-sprites (y)
  "Display the sprite for this line Y."
  ;; Find the sprites on this line
  ;; maybe combine for all lines, since I wite all lines at once.. not how it should be done... but faster.
  (let ((count 0)
        (sy 0)
        (sprite_list nil))
    ;; Object Attribute Memory (OAM) contains 40 4-byte blocks
    (while (< count 40) ;; Max 40 sprites, max 10 per line.
      (setq sy (- (eboy-mem-read-byte (+ #xFE00 (* count 4))) 16)) ;; -16 upperleft corner of sprite
      (when (and (>= y sy) (< y (+ sy 8))) ;; eboy-lcdc-obj-size
        ;; FOUND SPRITE FOR THIS LINE
        ;;(message "sprite! line: %d count: %d" y count)
        ;; TODO: insert sorted
        (setq sprite_list (append sprite_list (list (list
                                                     sy ; y
                                                     (- (eboy-mem-read-byte (+ #xFE00 (* count 4) 1)) 8) ; x_end - sprit size
                                                     (eboy-mem-read-byte (+ #xFE00 (* count 4) 2)) ; sprite number
                                                     (eboy-mem-read-byte (+ #xFE00 (* count 4) 3)) ; flags
                                                     )))))
      (incf count))
    sprite_list))

(defun eboy-get-color (byte1 byte2 x)
  "Get from line y BYTE1 and BYTE2 the color of coordinate X."
  (let* ((bit (- 7 (mod x 8)))
         (mask (lsh 1 bit)))
    (logior (lsh (logand byte2 mask) (+ (* -1 bit) 1))
            (lsh (logand byte1 mask) (* -1 bit)))))

(defun eboy-window-tile-map-selected-addr ()
  "Get the tile map address."
  (if eboy-lcdc-window-tile-map-select
      #x9C00
    #x9800))

(defun eboy-bg-&-window-tile-data-selected-addr (tileid)
  "Get the TILEID data address."
  (if eboy-lcdc-bg-&-window-tile-data-select
      (+ #x8000 (* tileid 16))
    (+ #x8800 (* (eboy-byte-to-signed tileid) 16))))

(defun eboy-get-tile-nr (x y)
  "Given a X Y coordinate, return tile number."
  (+ (/ x 8) (* (/ y 8) 32)))

(defun eboy-get-tile-id (tile-nr)
  "Get tile id from Background Tile Map using the TILE-NR."
  (eboy-mem-read-byte (+ (if eboy-lcdc-bg-tile-map-display-select #x9C00 #x9800) tile-nr)))

(defun eboy-get-color-xy (x y)
  "Get the color for coordinate X, Y from the bg & window tile data."
  (let ((tile-line-addr (+ (eboy-bg-&-window-tile-data-selected-addr (eboy-get-tile-id (eboy-get-tile-nr x y))) (* (mod y 8) 2))))
    (eboy-get-color (eboy-mem-read-byte tile-line-addr) (eboy-mem-read-byte (1+ tile-line-addr)) x)))

(defvar eboy-display-unicode-list-light-theme (list #x0020 #x2592 #x2593 #x2588  ) "List with unicode charachters for the different shades of gray.")
(defvar eboy-display-unicode-list-dark-theme (list #x2588 #x2593 #x2592 #x0020))
(defvar eboy-display-unicode-list eboy-display-unicode-list-dark-theme)

(defun eboy-write-display ()
  "Write the colors."
  (interactive)
  (with-current-buffer "*eboy-display*")
  ;; (switch-to-buffer "*eboy-display*")
  ;; (fundamental-mode)
  ;; (inhibit-read-only t)
  (erase-buffer)
  (insert "P3\n")
  (insert "160 144\n")
  (insert "255\n")
  (dotimes (y 144)
    (dotimes (x 160)
      (insert (format "%s " (gethash (eboy-get-color-xy x y) eboy-display-color-table))))
    (insert "\n"))
  (image-mode)
  (deactivate-mark)
  (display-buffer "*eboy-display*")
  ;;(switch-to-buffer "*eboy*")
  )

(defun eboy-write-display-unicode ()
  "Write the display as unicode characters."
  ;;(with-current-buffer "*eboy-display*")
  (erase-buffer)
  (let ((line nil)
        (c))
    (dotimes (y 144)
      (setq line nil)
      (dotimes (x 160)
        (setq c (nth (eboy-get-color-xy (mod (+ x eboy-lcd-scrollx) 256) (mod (+ y eboy-lcd-scrolly) 256)) eboy-display-unicode-list))
        (setq line (cons c line))
        (setq line (cons c line))) ;; draw x twice, to get a more square sized display.
      (setq line (nreverse line))
      ;; insert the sprites
      (let ((sprites (eboy-display-sprites y))
            (xs)
            (ys)
            (xs_end)
            (nr)
            (tile-addr))
        (when (not (null sprites))
          ;;(message "%s" sprites)
          ;; Draw sprites
          ;; TODO: Check if off screen
          ;; TODO: Check if sprite is flipped
          (dolist (sprite sprites)
            ;;(message "y:%d %s %s" y sprite  eboy-lcdc-obj-size)
            (setq xs (nth 1 sprite)) ; x coordinate of the sprite
            (setq xs_end (+ xs 8))
            (setq nr (nth 2 sprite)) ;; sprite number
            (setq ys (- y (nth 0 sprite))) ; y coordinate of the sprite
            (setq tile-addr (+ #x8000 (* nr 16) (* ys 2)))
            ;;(nth 3 sprite) ;; flags
            (while (and (< xs xs_end) (< xs 160))
              ;;(message "%s" (length line))
              (let ((color (nth (eboy-get-color (eboy-mem-read-byte tile-addr) (eboy-mem-read-byte (1+ tile-addr)) xs) eboy-display-unicode-list)))
                (setf (nth (* 2 xs) line) color)
                (setf (nth (1+ (* 2 xs)) line) color))
              ;;(message "%s" (length line))
              (incf xs)
              ;;(user-error "Stop!")
              )
            )
          ))
      (insert (concat line "\n"))
      ))
;;  (goto-char (point-min))
  (redisplay)
;;  (sit-for 0.1)
  )




(defun eboy-lcd-cycle ()
  "Perform a single lcd cycle."
  ;; TODO: enable again later for speed increase?
  ;;(if eboy-lcdc-display-enable
  (let ((screen-cycle (mod eboy-clock-cycles 70224)))
    (setq eboy-lcd-ly (/ screen-cycle 456))
    (when (and (= eboy-lcd-ly 143) (not eboy-display-write-done))
      ;; (eboy-write-display-unicode)
      (eboy-debug-update-fps)
      (setq eboy-display-write-done t))
    (when (and (= eboy-lcd-ly 144) eboy-display-write-done)
      (setq eboy-interrupt-pending (logior eboy-interrupt-pending eboy-im-vblank))
      (setq eboy-display-write-done nil))))
  ;;)

(defun eboy-read-keys (key)
  "See if the user pressed any KEY."
  ;; | Start       | Enter    |         13 | 0111 (#x7)          |
  ;; | Select      | Space    |         32 | 1011 (#xB)          |
  ;; | B           | D        |        100 | 1101 (#xD)          |
  ;; | A           | S        |        115 | 1110 (#xE)          |
  ;; |-------------+----------+------------+---------------------|
  ;; | down        | k        |        107 | 0111 (#x7)          |
  ;; | up          | i        |        105 | 1011 (#xB)          |
  ;; | left        | j        |        106 | 1101 (#xD)          |
  ;; | right       | l        |        108 | 1110 (#xE)          |
  (setq eboy-joypad-button-keys #xF)
  (setq eboy-joypad-direction-keys #xF)
  (unless (null key)
    (cond
     ((= key 13) (setq eboy-joypad-button-keys #x7))
     ((= key 32) (setq eboy-joypad-button-keys #xB))
     ((= key 100) (setq eboy-joypad-button-keys #xD))
     ((= key 115) (setq eboy-joypad-button-keys #xE))
     ((= key 107) (setq eboy-joypad-direction-keys #x7))
     ((= key 105) (setq eboy-joypad-direction-keys #xB))
     ((= key 106) (setq eboy-joypad-direction-keys #xD))
     ((= key 108) (setq eboy-joypad-direction-keys #xE)))))

(defun eboy-debug-update-fps ()
  "Update the Frames Per Second counter."
  (incf eboy-debug-nr-of-frames)
  (when (> eboy-debug-nr-of-frames eboy-skip-frames)
    (setq eboy-debug-nr-of-frames 0)
    (incf eboy-debug-nr-of-displayed-frames)
    (eboy-write-display-unicode)
    (eboy-read-keys (read-char-exclusive nil nil 0.1)))
  (when (> (time-to-seconds (current-time)) (+ eboy-debug-fps-timestamp eboy-debug-show-fps-after-sec))
    (setq eboy-debug-fps-timestamp (time-to-seconds (current-time)))
    (message "FPS: %.2f" (/ eboy-debug-nr-of-displayed-frames eboy-debug-show-fps-after-sec))
    (setq eboy-debug-nr-of-displayed-frames 0)))

(defun eboy-timer-cycle ()
  "Update and check timer."
  (let ((delta (- eboy-clock-cycles eboy-timer-cycles)))
    (when (> delta 256) ;; 16384 Hertz
      (let ((div-reg-val (aref eboy-io 4)))
        (if (= div-reg-val 255)
            (aset eboy-io 4 0))
        (aset eboy-io 4 (1+ div-reg-val)))
      (setq eboy-timer-cycles (+ eboy-timer-cycles 256)))))

(defun eboy-process-opcode (opcode)
  "Process OPCODE."
  (if eboy-cpu-halted
      (eboy-inc-pc 1)
    (funcall (aref eboy-cpu opcode))
    (eboy-inc-pc 1)))

(defun eboy-disable-interrupt (interrupt-mask)
  "Remove the interrupt flag for INTERRUPT-MASK."
  (setq eboy-interrupt-pending (logand eboy-interrupt-pending (lognot interrupt-mask))))

(defun eboy-process-interrupt (address)
  "Disable interrupts, put PC on stack and set PC to ADDRESS."
  (setq eboy-interrupt-master-enbl nil)
  (decf eboy-sp 2)
  (eboy-mem-write-short eboy-sp eboy-pc)
  (setq eboy-pc address))

(defun eboy-process-interrupts ()
  "Process pending interrupts."
  (if eboy-delay-enabling-interrupt-p
      (setq eboy-delay-enabling-interrupt-p nil)
    (let ((enbl-intr (logand eboy-interrupt-enabled  eboy-interrupt-pending)))
      (when (and eboy-interrupt-master-enbl (> enbl-intr #x00))
          ;; handle enbl interrupt
          (cond
           ((= 1 (logand enbl-intr eboy-im-vblank))
            ;; V-Blank
            (eboy-disable-interrupt eboy-im-vblank)
            (eboy-process-interrupt #x40))
           ((= 1 (logand enbl-intr eboy-im-lcdc))
            ;; LCDC
            (eboy-disable-interrupt eboy-im-lcdc)
            (eboy-process-interrupt #x48))
           ((= 1 (logand enbl-intr eboy-im-tmr-overflow))
            ;; Timer Overflow
            (eboy-disable-interrupt eboy-im-tmr-overflow)
            (eboy-process-interrupt #x50))
           ((= 1 (logand enbl-intr eboy-im-s-trans-compl))
            ;; Serial I/O transfer complete
            (eboy-disable-interrupt eboy-im-s-trans-compl)
            (eboy-process-interrupt #x58))
           ((= 1 (logand enbl-intr eboy-im-h2l-pins))
            ;; transition from h to l of pin P10-P13
            (eboy-disable-interrupt eboy-im-h2l-pins)
            (eboy-process-interrupt #x60)))))))

;;;###autoload
(defun eboy-load-rom (path-to-rom)
  "Load the rom file PATH-TO-ROM."
  (interactive "f")

  (setq eboy-rom-filename path-to-rom)
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))

  (when (> eboy-rom-size (* 32 1024))
    (error "Currently only 32 kB roms are supported"))

  (if nil
      (progn
        (setq eboy-boot-rom-filename "boot/DMG_ROM.bin")
        (setq eboy-boot-rom (vconcat (eboy-read-bytes eboy-boot-rom-filename)))
        ;; replace the lower rom with boot rom, to reduce the number condition statements in mem.
        (setq eboy-low-rom (seq-take eboy-rom (length eboy-boot-rom)))
        (setq eboy-rom (concat eboy-boot-rom (seq-drop eboy-rom (length eboy-boot-rom))))
        (setq eboy-pc 0))
    (setq eboy-pc eboy-pc-start-address)
    (setq eboy-sp eboy-sp-initial-value)
    (eboy-init-registers)
    (eboy-init-memory))

  (setq eboy-clock-cycles 0)
  (setq eboy-lcd-ly 0)
  (setq eboy-lcd-scrollx 0)
  (setq eboy-lcd-scrolly 0)
  (setq eboy-timer-cycles 0)
  (switch-to-buffer "*eboy-display*")
  (text-scale-set -8) ; only when using unicode display function
  (erase-buffer)
  (eboy-log (format "Load rom: %s\n" eboy-rom-filename))
  (eboy-log (format "Rom size: %d bytes\n" eboy-rom-size))
  (eboy-log (format "Rom title: %s\n" (eboy-rom-title)))

  (let ((flags (make-bool-vector 4 nil)))
     ;; init flags 0xB0
    (eboy-set-flag flags :Z t)
    (eboy-set-flag flags :N nil)
    (eboy-set-flag flags :H t)
    (eboy-set-flag flags :C t)
    (setq eboy-flags flags))
  (message "eboy at pc: %x" eboy-pc)
  (eboy-run))

(defun eboy-run ()
  "Run infinite loop."
  (interactive)
  (switch-to-buffer "*eboy-display*")
  (while t
    (eboy-process-opcode (eboy-mem-read-byte eboy-pc))
    (eboy-process-interrupts)
    (eboy-lcd-cycle)
    (eboy-timer-cycle)))

;(eboy-load-rom "roms/test_rom.gb")

(provide 'eboy)
;;; eboy.el ends here

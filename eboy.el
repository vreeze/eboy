;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

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

;;; Code:
(defun eboy-read-bytes (path)
  "Read binary data from PATH.
Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

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

(defun eboy-get-short ()
  "Skip opcode and get next two bytes."
  (logior (lsh (aref eboy-rom (+ eboy-pc 2)) 8) (aref eboy-rom (+ eboy-pc 1)))
  )

(defun eboy-process-opcode (opcode)
  "Process OPCODE."
  (insert (format "pc: 0x%x  " eboy-pc))
    (cl-case opcode
      (0 (insert "NOP\n"))
      (#x1 (insert (format "LD BC, $%04x\n" (eboy-get-short))))
      (#xc3 (progn
              (insert (format "JP $%04x\n" (eboy-get-short)))
              ;;(setq eboy-pc (logior (lsh (aref eboy-rom (+ eboy-pc 2)) 8) (aref eboy-rom (+ eboy-pc 1))))
              ))

      (otherwise (insert (format "Unimplemented opcode 0x%x\n" opcode)))
      )
    )
(defun eboy-load-rom ()
  "Load the rom file.  For now just automatically load a test rom."
  (interactive)
  (setq eboy-rom-filename "roms/test_rom.gb")
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))
  (setq eboy-pc #x100)
  (setq eboy-debugging-nr-instructions 0)
  (eboy-reset-CPU-flags)
  (switch-to-buffer "*eboy*")
  (erase-buffer)
  (insert (format "Load rom: %s\n" eboy-rom-filename))
  (insert (format "Rom size: %d bytes\n" eboy-rom-size))
  ;; loop
  (while (and (< eboy-pc eboy-rom-size) (< eboy-debugging-nr-instructions 500))
    (eboy-process-opcode (aref eboy-rom eboy-pc))
    (setq eboy-pc (+ eboy-pc 1))
    (setq eboy-debugging-nr-instructions (+ eboy-debugging-nr-instructions 1))
    )
  )

(eboy-load-rom)


(provide 'eboy)
;;; eboy.el ends here

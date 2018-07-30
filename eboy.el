;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;;; Code:
(defun eboy-read-bytes (path)
  "Read binary data from PATH.
Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

;; vconcat, list to vector
(defvar eboy-rom-filename nil "The file name of the loaded rom.")
(defvar eboy-rom nil "The binary vector of the rom.")
(defvar eboy-rom-size nil "The size of the rom in bytes.")
(defvar eboy-pc nil "The program counter.")


(defun eboy-process-opcode (opcode)
  "Process OPCODE."
    (cl-case opcode
      (0 (insert "NOP\n"))
      (otherwise (insert (format "Unimplemented opcode 0x%x\n" opcode)))
      )
    )
(defun eboy-load-rom ()
  "Load the rom file.  For now just automatically load a test rom."
  (interactive)
  (setq eboy-rom-filename "../desktop.ini")
  (setq eboy-rom (vconcat (eboy-read-bytes eboy-rom-filename)))
  (setq eboy-rom-size (length eboy-rom))
  (setq eboy-pc 0)
  (switch-to-buffer "*eboy*")
  (erase-buffer)
  (insert (format "Load rom: %s\n" eboy-rom-filename))
  (insert (format "Rom size: %d bytes\n" eboy-rom-size))
  ;; loop
  (while (< eboy-pc eboy-rom-size)
    (eboy-process-opcode (aref eboy-rom eboy-pc))
    (setq eboy-pc (+ eboy-pc 1))
    )
  )

(eboy-load-rom)


(provide 'eboy)
;;; eboy.el ends here

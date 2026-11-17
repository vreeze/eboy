;;; eboy-headless.el --- Run eboy without a display: ROM loading, Blargg tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers to run ROMs in batch mode, used by the Blargg test runner and the
;; benchmarks.  Nothing here changes how the emulator itself works: the serial
;; port output (which Blargg's tests use to print their results) is captured
;; with an advice on `eboy-mem-write-byte' that is only active during a test.
;;
;;   emacs -Q --batch -L . -L test -l eboy-headless -f eboy-blargg-run-batch [ROM...]
;;
;; Without ROM arguments it runs every .gb file in roms/blargg/cpu_instrs/.
;; `eboy-blargg-run-visual' runs the same tests in a GUI Emacs with the
;; display on (`make test-visual').

;;; Code:
(require 'cl-lib)
(require 'eboy)

(defconst eboy-headless-cycles-per-frame 70224)
(defconst eboy-headless-gb-fps (/ 4194304.0 70224))

(defvar eboy-headless-serial nil
  "Bytes sent over the serial port, newest first, while capturing.")

(defvar eboy-blargg-default-dir
  (expand-file-name "../roms/blargg/cpu_instrs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Where the Blargg cpu_instrs ROMs are expected.")

(defvar eboy-blargg-timeout-seconds
  (string-to-number (or (getenv "EBOY_BLARGG_TIMEOUT") "40"))
  "Emulated seconds after which a test that hasn't finished counts as hanging.")

(defun eboy-headless-reset ()
  "Reset all emulator state, like `eboy-load-rom' does, minus loading."
  (fillarray eboy-ram 0) (fillarray eboy-ram-banks 0) (fillarray eboy-hram 0)
  (fillarray eboy-vram 0) (fillarray eboy-io 0)
  (setq eboy-pc eboy-pc-start-address eboy-sp eboy-sp-initial-value
        eboy-clock-cycles 0 eboy-lcd-ly 0 eboy-lcd-scrollx 0 eboy-lcd-scrolly 0
        eboy-timer-cycles 0 eboy-timer-tac-timer-started nil
        eboy-timer-262144hz-counter-tac 0 eboy-timer-262144hz-counter-div 0
        eboy-timer-tac-input-clock-select 0
        eboy-rom-bank-nr 1 eboy-ram-bank-nr 0 eboy-mem-bank-type nil
        eboy-ram-bank-enable nil
        eboy-rom-banking-mode t eboy-mbc1-bank-low 1 eboy-mbc1-bank-high 0
        eboy-cpu-halted nil eboy-interrupt-master-enbl nil
        eboy-interrupt-pending 0 eboy-delay-enabling-interrupt-p nil
        eboy-display-write-done nil
        eboy-debug-nr-of-frames 0 eboy-debug-nr-of-displayed-frames 0
        eboy-debug-fps-timestamp (float-time))
  (eboy-init-registers)
  (eboy-init-memory)
  (let ((flags (make-bool-vector 4 nil)))   ; flags #xB0, as `eboy-load-rom'
    (eboy-set-flag flags :Z t)
    (eboy-set-flag flags :H t)
    (eboy-set-flag flags :C t)
    (setq eboy-flags flags)))

(defun eboy-headless-load-rom (file)
  "Load ROM FILE and reset the emulator, without starting it."
  (let ((eboy-debug nil))
    (setq eboy-rom-filename file
          eboy-rom (vconcat (eboy-read-bytes file))
          eboy-rom-size (length eboy-rom))
    (eboy-headless-reset)
    (eboy-read-cartridge-header)))

(defmacro eboy-headless-step ()
  "One iteration of the body of `eboy-run'."
  '(progn
     (eboy-process-opcode (eboy-mem-read-byte eboy-pc))
     (eboy-process-interrupts)
     (eboy-lcd-cycle)
     (eboy-timer-cycle)))

(defun eboy-headless--serial-advice (address data)
  "Capture a serial transfer: writing #x81 to SC (ADDRESS #xFF02, DATA)."
  (when (and (= address #xFF02) (= (logand data #x81) #x81))
    (push (aref eboy-io 1) eboy-headless-serial)))

(defun eboy-headless-serial-string ()
  "The captured serial output as a string."
  (apply #'string (reverse eboy-headless-serial)))

(defvar eboy-blargg--progress ""
  "Shown in the header line while running tests visually, e.g. \"3/11\".")

(defun eboy-blargg--show-display (file)
  "Prepare the *eboy-display* buffer to watch test FILE."
  (switch-to-buffer (get-buffer-create "*eboy-display*"))
  (setq header-line-format
        (format " Blargg %s: %s   (C-g stops)" eboy-blargg--progress
                (file-name-base file)))
  (text-scale-set -8)                   ; as `eboy-load-rom' does
  (eboy-fit-frame-to-display)
  (erase-buffer))

(defun eboy-blargg-run-rom (file &optional visual)
  "Run Blargg test ROM FILE until it reports, hangs or crashes.
With VISUAL, draw the display while it runs (like `eboy-run' does) and keep
the final screen up for a moment.
Return a plist (:name :result :emulated :wall :output)."
  (let ((eboy-skip-frames (if visual eboy-skip-frames
                            most-positive-fixnum)) ; never draw or read keys
        (inhibit-message (not visual))
        (eboy-headless-serial nil)
        (limit (* eboy-blargg-timeout-seconds 4194304))
        (t0 (float-time))
        result)
    (advice-add 'eboy-mem-write-byte :after #'eboy-headless--serial-advice)
    (unwind-protect
        (condition-case err
            (progn
              (eboy-headless-load-rom file)
              (when visual (eboy-blargg--show-display file))
              (while (not result)
                (let ((frame-end (+ eboy-clock-cycles eboy-headless-cycles-per-frame)))
                  (while (< eboy-clock-cycles frame-end) (eboy-headless-step)))
                (let ((out (eboy-headless-serial-string)))
                  (cond ((string-match-p "Passed" out) (setq result "passed"))
                        ((string-match-p "Failed" out) (setq result "FAILED"))
                        ((>= eboy-clock-cycles limit) (setq result "TIMEOUT"))))))
          (error (setq result (format "ERROR %S" err))))
      (advice-remove 'eboy-mem-write-byte #'eboy-headless--serial-advice))
    (when visual                        ; show the final screen with the result
      (eboy-write-display-unicode)
      (setq header-line-format
            (format " Blargg %s: %s   %s" eboy-blargg--progress
                    (file-name-base file) result))
      (sit-for 2))
    (list :name (file-name-base file)
          :result result
          :emulated (/ eboy-clock-cycles 4194304.0)
          :wall (- (float-time) t0)
          :output (eboy-headless-serial-string))))

(defun eboy-blargg-run (files &optional visual)
  "Run Blargg test ROM FILES and print an org table of the results.
With VISUAL, show each test on screen while it runs."
  (let ((passed 0) (i 0))
    (princ "| ROM | Result | Emulated s | Wall s | Serial output |\n|-\n")
    (dolist (file files)
      (let* ((eboy-blargg--progress (format "%d/%d" (cl-incf i) (length files)))
             (r (eboy-blargg-run-rom file visual)))
        (when (equal (plist-get r :result) "passed") (cl-incf passed))
        (princ (format "| %s | %s | %.1f | %.1f | %s |\n"
                       (plist-get r :name) (plist-get r :result)
                       (plist-get r :emulated) (plist-get r :wall)
                       (replace-regexp-in-string
                        "|" "\\vert{}"
                        (string-trim (replace-regexp-in-string
                                      "[\n\r]+" " " (plist-get r :output)))
                        t t)))))
    (princ (format "\n%d of %d passed\n" passed (length files)))
    passed))

(defun eboy-blargg--default-files ()
  "The numbered test ROMs in `eboy-blargg-default-dir'."
  (or (directory-files eboy-blargg-default-dir t "^[0-9].*\\.gb\\'")
      (error "No ROMs found in %s (run `make roms')" eboy-blargg-default-dir)))

;;;###autoload
(defun eboy-blargg-run-visual (&optional files)
  "Run Blargg test ROM FILES one after another with the display on.
Defaults to all tests in `eboy-blargg-default-dir'.  When done, show the
results table in the *Eboy Blargg* buffer."
  (interactive)
  (let ((files (or files (eboy-blargg--default-files)))
        (results (get-buffer-create "*Eboy Blargg*")))
    (with-current-buffer results (erase-buffer))
    (let ((standard-output results))
      (eboy-blargg-run files t))
    (switch-to-buffer results)
    (when (require 'org nil t)
      (org-mode)
      (goto-char (point-min))
      (org-table-align))
    (goto-char (point-min))))

(defun eboy-blargg-run-batch ()
  "Entry point for batch mode: ROMs from the command line, or the default set."
  (let ((files (or (mapcar #'expand-file-name command-line-args-left)
                   (eboy-blargg--default-files))))
    (setq command-line-args-left nil)
    (eboy-blargg-run files)))

(provide 'eboy-headless)
;;; eboy-headless.el ends here

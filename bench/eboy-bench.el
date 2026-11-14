;;; eboy-bench.el --- Repeatable performance benchmarks for eboy  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run through bench/run.sh, which compiles a copy of eboy and calls the
;; entry points below.  Every benchmark appends one result line to the file
;; named by the EBOY_BENCH_OUT environment variable.
;;
;; Two workloads:
;; - synthetic: a deterministic "ROM" with a short loop at #x100 (INC A,
;;   LD (HL),A, INC L, DEC B, BIT 0,A, BIT 7,A, JR), random VRAM tiles and 40
;;   sprites placed diagonally, so the renderer has real work.
;; - a real ROM given by EBOY_BENCH_ROM (run.sh uses Blargg's "10-bit ops").

;;; Code:
(require 'cl-lib)
(require 'eboy)
(require 'eboy-headless)

(defvar eboy-bench-rom nil
  "ROM file for the current workload, or nil for the synthetic one.")

(defconst eboy-bench-program
  '(#x21 #x00 #xC0   ; LD HL,$C000
    #x3C             ; INC A
    #x77             ; LD (HL),A
    #x2C             ; INC L
    #x05             ; DEC B
    #xCB #x47        ; BIT 0,A
    #xCB #x7F        ; BIT 7,A
    #x18 #xF6)       ; JR -10  (back to INC A)
  "Machine code placed at #x100.")

(defconst eboy-bench-cycles-per-frame 70224)
(defconst eboy-bench-gb-fps (/ 4194304.0 70224))

(defun eboy-bench--out (fmt &rest args)
  "Append a formatted result line (FMT, ARGS) to $EBOY_BENCH_OUT."
  (let ((line (apply #'format fmt args)))
    (princ (concat line "\n") #'external-debugging-output)
    (with-temp-buffer
      (insert line "\n")
      (append-to-file (point-min) (point-max) (getenv "EBOY_BENCH_OUT")))))

(defun eboy-bench-setup ()
  "Put the emulator in a fresh, deterministic state for the workload."
  (if eboy-bench-rom
      (eboy-headless-load-rom eboy-bench-rom)
    (eboy-bench-setup-synthetic)))

(defun eboy-bench-workloads ()
  "List of (LABEL . ROM) workloads to run."
  (let ((rom (getenv "EBOY_BENCH_ROM")))
    (append '(("synthetic" . nil))
            (when (and rom (file-exists-p rom))
              (list (cons (file-name-base rom) rom))))))

(defmacro eboy-bench-each-workload (mode &rest body)
  "Run BODY for each workload with MODE bound to \"MODE, workload\"."
  (declare (indent 1))
  `(dolist (w (eboy-bench-workloads))
     (let ((eboy-bench-rom (cdr w))
           (,mode (format "%s, %s" ,mode (car w))))
       ,@body)))

(defun eboy-bench-setup-synthetic ()
  "Set up the synthetic workload."
  (random "eboy-bench")
  (let ((rom (make-vector #x8000 0)))
    (cl-loop for b in eboy-bench-program
             for i from #x100 do (aset rom i b))
    (aset rom #x147 0)
    (setq eboy-rom rom eboy-rom-size #x8000 eboy-mem-bank-type 'rom-only
          eboy-pc #x100 eboy-sp #xFFFE
          eboy-clock-cycles 0 eboy-lcd-ly 0 eboy-lcd-scrollx 0 eboy-lcd-scrolly 0
          eboy-timer-cycles 0 eboy-timer-tac-timer-started nil
          eboy-timer-262144hz-counter-tac 0 eboy-timer-262144hz-counter-div 0
          eboy-rom-bank-nr 1 eboy-ram-bank-nr 0 eboy-ram-bank-enable nil
          eboy-cpu-halted nil eboy-interrupt-master-enbl nil
          eboy-interrupt-pending 0 eboy-display-write-done nil
          eboy-debug-nr-of-frames 0 eboy-debug-nr-of-displayed-frames 0
          eboy-debug-fps-timestamp (float-time)
          eboy-flags (make-bool-vector 4 nil))
    (eboy-init-registers)
    (eboy-init-memory)              ; LCDC #x91: LCD on, tile data #x8000
    (dotimes (i #x2000) (aset eboy-vram i (random 256)))
    (dotimes (i 40)                 ; OAM at #xFE00 lives in eboy-ram
      (aset eboy-ram (+ #x1E00 (* i 4)) (+ 16 (* 3 i)))      ; y
      (aset eboy-ram (+ #x1E01 (* i 4)) (+ 8 (* 4 i)))       ; x
      (aset eboy-ram (+ #x1E02 (* i 4)) (random 256)))))     ; tile

(defmacro eboy-bench--step ()
  "One iteration of the body of `eboy-run'."
  '(progn
     (eboy-process-opcode (eboy-mem-read-byte eboy-pc))
     (eboy-process-interrupts)
     (eboy-lcd-cycle)
     (eboy-timer-cycle)))

(defun eboy-bench-cpu (mode &optional n)
  "CPU core only (display never drawn), N instructions, labelled MODE."
  (setq n (or n 3000000))
  (eboy-bench-setup)
  (let ((eboy-skip-frames most-positive-fixnum))
    (garbage-collect)
    (let ((t0 (float-time)))
      (dotimes (_ n) (eboy-bench--step))
      (let* ((dt (- (float-time) t0))
             (speed (/ eboy-clock-cycles dt 4194304.0)))
        (eboy-bench--out "| cpu-only | %s | %d instr | %.2f s | %.0f instr/s | %.0f%% |"
                         mode n dt (/ n dt) (* 100 speed))))))

(defun eboy-bench-render (mode &optional n)
  "Call `eboy-write-display-unicode' N times, labelled MODE.
In batch this is only building the buffer; in a GUI it includes redisplay."
  (setq n (or n 30))
  (eboy-bench-setup)
  (when eboy-bench-rom                  ; let the ROM put something on screen
    (let ((eboy-skip-frames most-positive-fixnum)
          (end (* 300 eboy-bench-cycles-per-frame)))
      (while (< eboy-clock-cycles end) (eboy-bench--step))))
  (with-current-buffer (get-buffer-create "*eboy-display*")
    (garbage-collect)
    (let ((t0 (float-time)))
      (dotimes (_ n) (eboy-write-display-unicode))
      (let ((dt (- (float-time) t0)))
        (eboy-bench--out "| %s | %s | %d frames | %.2f s | %.1f ms/frame | |"
                         (if noninteractive "render-build" "render+redisplay")
                         mode n dt (* 1000 (/ dt n)))))))

(defun eboy-bench-full (mode frames skip)
  "Run like `eboy-run' for FRAMES emulated frames with `eboy-skip-frames' SKIP.
Labelled MODE.  Breaks wall time down into display, input wait and the rest."
  (eboy-bench-setup)
  (let* ((eboy-skip-frames skip)
         (disp-time 0.0) (fps-time 0.0)
         (time-disp (lambda (f &rest args)
                      (let ((t0 (float-time)))
                        (prog1 (apply f args)
                          (cl-incf disp-time (- (float-time) t0))))))
         (time-fps (lambda (f &rest args)
                     (let ((t0 (float-time)))
                       (prog1 (apply f args)
                         (cl-incf fps-time (- (float-time) t0))))))
         (displayed 0)
         (count-disp (lambda (&rest _) (cl-incf displayed)))
         (target (* frames eboy-bench-cycles-per-frame)))
    (advice-add 'eboy-write-display-unicode :around time-disp)
    (advice-add 'eboy-write-display-unicode :before count-disp)
    (advice-add 'eboy-debug-update-fps :around time-fps)
    (unwind-protect
        (with-current-buffer (get-buffer-create "*eboy-display*")
          (garbage-collect)
          (let ((t0 (float-time)))
            (while (< eboy-clock-cycles target) (eboy-bench--step))
            (let* ((dt (- (float-time) t0))
                   (wait (- fps-time disp-time))
                   (cpu (- dt fps-time)))
              (eboy-bench--out
               (concat "| full skip=%d | %s | %d frames | %.2f s | %.1f%% GB speed, "
                       "%.2f displayed fps | display %.0f%% (%.0f ms/frame), "
                       "input wait %.0f%%, emulation %.0f%% |")
               skip mode frames dt
               (* 100 (/ frames dt eboy-bench-gb-fps))
               (/ displayed dt)
               (* 100 (/ disp-time dt)) (* 1000 (/ disp-time (max displayed 1)))
               (* 100 (/ wait dt)) (* 100 (/ cpu dt))))))
      (advice-remove 'eboy-write-display-unicode time-disp)
      (advice-remove 'eboy-write-display-unicode count-disp)
      (advice-remove 'eboy-debug-update-fps time-fps))))

(defun eboy-bench-batch (mode)
  "Batch-mode benchmarks for MODE."
  (eboy-bench-each-workload mode
    (eboy-bench-cpu mode)
    (eboy-bench-render mode)))

(defun eboy-bench-gui (mode)
  "GUI benchmarks for MODE, with real redisplay.  Exits Emacs when done."
  (run-with-timer
   1 nil
   (lambda ()
     (unwind-protect
         (progn
           (set-frame-size (selected-frame) 1280 900 t)
           (switch-to-buffer "*eboy-display*")
           (delete-other-windows)
           (text-scale-set -8)            ; as `eboy-load-rom' does
           (redisplay t)
           (eboy-bench--out "| frame | %s | %dx%d px | | | |"
                            mode (frame-pixel-width) (frame-pixel-height))
           (eboy-bench-each-workload mode
             (eboy-bench-render mode)
             (eboy-bench-full mode 600 20)  ; default settings, 10 emulated seconds
             (eboy-bench-full mode 60 0)))  ; draw every frame
       (kill-emacs 0)))))

(provide 'eboy-bench)
;;; eboy-bench.el ends here

;;; eboy-mbc-test.el --- Check MBC1 ROM/RAM bank switching  -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses a fake 2 MB MBC1 ROM (128 banks) in which every bank starts with its
;; own number, so reading #x4000 tells which bank is mapped.  Run with
;;   emacs -Q --batch -L . -L test -l eboy-mbc-test -f eboy-mbc-test-run-batch
;; (part of `make test').

;;; Code:
(require 'cl-lib)
(require 'eboy-headless)

(defun eboy-mbc-test-run-batch ()
  "Run the MBC checks, print the results and exit non-zero on failure."
  (let ((rom (make-vector (* 128 #x4000) 0)) (fails 0))
    (dotimes (b 128) (aset rom (* b #x4000) b))
    (aset rom #x147 1)
    (setq eboy-rom rom eboy-rom-size (length rom))
    (let ((eboy-debug nil)) (eboy-headless-reset) (eboy-get-rom-cartridge-type))
    (cl-flet ((check (what expect)
                (let ((got (eboy-mem-read-byte #x4000)))
                  (princ (format "%-38s bank %3d %s\n" what got (if (= got expect) "ok" (progn (cl-incf fails) (format "WRONG, expected %d" expect))))))))
      (check "after reset" 1)
      (eboy-mem-write-byte #x2000 5)  (check "write 5 to 2000" 5)
      (eboy-mem-write-byte #x2000 0)  (check "write 0 to 2000 (acts as 1)" 1)
      (eboy-mem-write-byte #x2000 #x1F) (eboy-mem-write-byte #x4000 3) (check "lower 1F + upper 3" 127)
      (eboy-mem-write-byte #x2000 0)  (check "lower 0 + upper 3 = 61h quirk" #x61)
      (eboy-mem-write-byte #x6000 1)  (check "RAM banking mode: upper bits off" 1)
      (princ (format "%-38s ram bank %d %s\n" "  ... and upper register = RAM bank" eboy-ram-bank-nr (if (= eboy-ram-bank-nr 3) "ok" (progn (cl-incf fails) "WRONG"))))
      (eboy-mem-write-byte #x6000 0)  (check "back to ROM banking mode" #x61))
    ;; Small ROM: bank numbers wrap to the ROM size
    (setq eboy-rom (vconcat (cl-subseq rom 0 (* 4 #x4000))) eboy-rom-size (* 4 #x4000))
    (let ((eboy-debug nil)) (eboy-headless-reset) (eboy-get-rom-cartridge-type))
    (eboy-mem-write-byte #x2000 6)
    (princ (format "%-38s bank %3d %s\n" "64 KB ROM, write 6 (wraps to 2)" (eboy-mem-read-byte #x4000)
                   (if (= (eboy-mem-read-byte #x4000) 2) "ok" (progn (cl-incf fails) "WRONG"))))
    (princ (format "%d failures\n" fails))
    (kill-emacs (if (zerop fails) 0 1))))

(provide 'eboy-mbc-test)
;;; eboy-mbc-test.el ends here

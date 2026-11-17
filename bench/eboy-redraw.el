;;; eboy-redraw.el --- Experiment: how fast can Emacs show a Game Boy frame?  -*- lexical-binding: t; -*-

;;; Commentary:
;; Compares ways to put one 160x144 frame on screen, without changing the
;; emulator.  Every variant gets the same pixels (colour numbers 0-3) and is
;; timed in three parts: building its text or image, putting it in the
;; buffer, and Emacs redisplaying it.  Each frame shifts the picture by one
;; pixel, so redisplay can't skip unchanged content.
;;
;; Variants:
;; - text-2x    the current format: 320x144 characters (every pixel twice)
;; - text-1x    160x144 characters (half the characters, squashed picture)
;; - halfblock  160x72 "▀" characters with foreground/background colours
;; - image      a 160x144 PGM image, scaled 4x
;;
;; Run through bench/redraw.sh (`make bench-redraw').

;;; Code:
(require 'cl-lib)
(require 'eboy)
(require 'eboy-headless)
(require 'eboy-bench)         ; for the synthetic workload

(defconst eboy-redraw-width 160)
(defconst eboy-redraw-height 144)

(defvar eboy-redraw-frames 30 "Frames per variant.")

(defun eboy-redraw--out (fmt &rest args)
  "Append a formatted result line (FMT, ARGS) to $EBOY_BENCH_OUT."
  (let ((line (apply #'format fmt args)))
    (with-temp-buffer
      (insert line "\n")
      (append-to-file (point-min) (point-max) (getenv "EBOY_BENCH_OUT")))))

(defun eboy-redraw-pixels ()
  "The current background as a 160x144 vector of colour numbers 0-3."
  (let ((v (make-vector (* eboy-redraw-width eboy-redraw-height) 0)))
    (dotimes (y eboy-redraw-height)
      (dotimes (x eboy-redraw-width)
        (aset v (+ x (* y eboy-redraw-width))
              (eboy-get-color-xy (mod (+ x eboy-lcd-scrollx) 256)
                                 (mod (+ y eboy-lcd-scrolly) 256)))))
    v))

(defsubst eboy-redraw--px (pixels shift x y)
  "Colour at X, Y of PIXELS, shifted left by SHIFT pixels."
  (aref pixels (+ (% (+ x shift) eboy-redraw-width) (* y eboy-redraw-width))))

;;; Text variants

(defun eboy-redraw--text (pixels shift repeat)
  "PIXELS as text, each pixel REPEAT characters wide, shifted by SHIFT."
  (let* ((chars (vconcat eboy-display-unicode-list))
         (w (1+ (* repeat eboy-redraw-width)))      ; + newline
         ;; Emacs 31 can't `aset' a 3-byte block char over a 1-byte one in a
         ;; string, so fill a vector of chars and `concat' it.
         (v (make-vector (* w eboy-redraw-height) ?\n))
         (i 0))
    (dotimes (y eboy-redraw-height)
      (dotimes (x eboy-redraw-width)
        (let ((c (aref chars (eboy-redraw--px pixels shift x y))))
          (dotimes (_ repeat) (aset v i c) (setq i (1+ i)))))
      (setq i (1+ i)))
    (concat v)))

(defvar eboy-redraw--palette ["#e0f8d0" "#88c070" "#346856" "#081820"]
  "Colours for colour numbers 0-3, lightest first.")

(defvar eboy-redraw--faces
  (let ((v (make-vector 16 nil)))
    (dotimes (top 4)
      (dotimes (bottom 4)
        (aset v (+ (* top 4) bottom)
              (list :foreground (aref eboy-redraw--palette top)
                    :background (aref eboy-redraw--palette bottom)))))
    v)
  "Face for each (top, bottom) colour pair of a half-block character.")

(defun eboy-redraw--halfblock (pixels shift)
  "PIXELS as 160x72 upper-half-block characters with colour faces.
Runs of the same colour pair share one face property."
  (let* ((w (1+ eboy-redraw-width))
         (v (make-vector (* w (/ eboy-redraw-height 2)) ?▀)))
    (dotimes (row (/ eboy-redraw-height 2))
      (aset v (+ (* row w) eboy-redraw-width) ?\n))
    (setq v (concat v))
    (dotimes (row (/ eboy-redraw-height 2))
      (let ((start (* row w)) (run-start 0) (run-face nil))
        (dotimes (x eboy-redraw-width)
          (let ((face (aref eboy-redraw--faces
                            (+ (* 4 (eboy-redraw--px pixels shift x (* 2 row)))
                               (eboy-redraw--px pixels shift x (1+ (* 2 row)))))))
            (unless (eq face run-face)
              (when run-face
                (put-text-property (+ start run-start) (+ start x) 'face run-face v))
              (setq run-start x run-face face))))
        (put-text-property (+ start run-start) (+ start eboy-redraw-width) 'face run-face v)))
    v))

;;; Image variant

(defun eboy-redraw--pgm (pixels shift)
  "PIXELS as a binary 8-bit PGM image, shifted by SHIFT.
Colour 0 (lightest) is 255, colour 3 (darkest) is 0.  A full 0-255 range, as
not every reader honours a small maxval."
  (let* ((header (format "P5\n%d %d\n255\n" eboy-redraw-width eboy-redraw-height))
         (hl (length header))
         (s (make-string (+ hl (* eboy-redraw-width eboy-redraw-height)) 0)))
    (dotimes (i hl) (aset s i (aref header i)))
    (let ((i hl))
      (dotimes (y eboy-redraw-height)
        (dotimes (x eboy-redraw-width)
          (aset s i (aref [255 170 85 0] (eboy-redraw--px pixels shift x y)))
          (setq i (1+ i)))))
    s))

;;; Running

(defun eboy-redraw--prepare-buffer (scale)
  "A fresh, selected buffer for one variant with `text-scale' SCALE."
  (let ((buf (get-buffer-create "*eboy-redraw*")))
    (switch-to-buffer buf)
    (delete-other-windows)
    (setq buffer-undo-list t truncate-lines t cursor-type nil)
    (erase-buffer)
    (text-scale-set scale)
    (redisplay t)
    buf))

(defun eboy-redraw-variant (name mode pixels scale build put)
  "Time variant NAME (label MODE) on PIXELS, with `text-scale' SCALE.
BUILD takes (PIXELS SHIFT) and returns the frame; PUT inserts it."
  (eboy-redraw--prepare-buffer scale)
  (garbage-collect)
  (let ((t-build 0.0) (t-put 0.0) (t-redisplay 0.0) frame)
    (dotimes (i eboy-redraw-frames)
      (let ((t0 (float-time)))
        (setq frame (funcall build pixels i))
        (let ((t1 (float-time)))
          (funcall put frame)
          (let ((t2 (float-time)))
            (redisplay t)
            (let ((t3 (float-time)))
              (cl-incf t-build (- t1 t0))
              (cl-incf t-put (- t2 t1))
              (cl-incf t-redisplay (- t3 t2)))))))
    (let ((n (float eboy-redraw-frames))
          (size (window-text-pixel-size nil (point-min) (point-max))))
      (eboy-redraw--out
       "| %s | %s | %.1f | %.1f | %.1f | %.1f | %dx%d |"
       name mode (* 1000 (/ t-build n)) (* 1000 (/ t-put n))
       (* 1000 (/ t-redisplay n))
       (* 1000 (/ (+ t-build t-put t-redisplay) n))
       (car size) (cdr size)))
    ;; Keep a picture of the last frame for checking by eye
    (when (getenv "EBOY_REDRAW_SHOTS")
      (let ((f (expand-file-name (format "%s-%s.txt" name mode) (getenv "EBOY_REDRAW_SHOTS"))))
        (write-region (format "window %dx%d, window-end %s of %s\n"
                              (window-body-width nil t) (window-body-height nil t)
                              (window-end nil t) (point-max))
                      nil f)))))

(defun eboy-redraw-run (mode label pixels)
  "Run all variants on PIXELS, labelled MODE and LABEL."
  (let ((m (format "%s, %s" mode label)) (prev-image nil))
    (eboy-redraw-variant "text-2x" m pixels -8
                         (lambda (p s) (eboy-redraw--text p s 2))
                         (lambda (f) (erase-buffer) (insert f)))
    (eboy-redraw-variant "text-1x" m pixels -8
                         (lambda (p s) (eboy-redraw--text p s 1))
                         (lambda (f) (erase-buffer) (insert f)))
    (eboy-redraw-variant "halfblock" m pixels -4
                         #'eboy-redraw--halfblock
                         (lambda (f) (erase-buffer) (insert f)))
    (eboy-redraw-variant "image" m pixels 0
                         (lambda (p s) (create-image (eboy-redraw--pgm p s) 'pbm t :scale 4))
                         (lambda (img)
                           (erase-buffer)
                           (insert-image img)
                           ;; Each frame is a new image; drop the previous one
                           ;; from the image cache so memory doesn't grow.
                           (when prev-image (image-flush prev-image))
                           (setq prev-image img)))))

(defun eboy-redraw-gui (mode)
  "Run the experiment in a GUI frame, labelled MODE, then exit."
  (run-with-timer
   1 nil
   (lambda ()
     (unwind-protect
         (progn
           (set-frame-size (selected-frame) 1280 900 t)
           (redisplay t)
           ;; Synthetic: random tiles, a busy picture
           (eboy-bench-setup-synthetic)
           (eboy-redraw-run mode "synthetic" (eboy-redraw-pixels))
           ;; Blargg "10-bit ops" after 300 frames: mostly text on a plain background
           (let ((rom (getenv "EBOY_BENCH_ROM")))
             (when (and rom (file-exists-p rom))
               (eboy-headless-load-rom rom)
               (let ((eboy-skip-frames most-positive-fixnum)
                     (end (* 300 eboy-headless-cycles-per-frame)))
                 (while (< eboy-clock-cycles end) (eboy-headless-step)))
               (eboy-redraw-run mode (file-name-base rom) (eboy-redraw-pixels)))))
       (kill-emacs 0)))))

(provide 'eboy-redraw)
;;; eboy-redraw.el ends here

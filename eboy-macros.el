(defmacro eboy-add-byte (byte value)
  "Simulate byte behavior: add VALUE to BYTE."
  `(progn (setq ,byte (+ ,byte ,value))
          (setq ,byte (logand ,byte #xFF))))

(defmacro eboy-add-to-short (short value)
  "Simulate short behavior: add VALUE to SHORT."
  `(progn (setq ,short (+ ,short ,value))
          (setq ,short (logand ,short #xFFFF))))

(defmacro eboy-dec (reg flags)
  "CPU Instruction: Decrement register REG and update FLAGS."
  `(progn (eboy-add-byte ,reg -1)
          (eboy-set-flag ,flags ,:Z (= ,reg 0))
          (eboy-set-flag ,flags ,:N t)
          (eboy-set-flag ,flags ,:H (= (logand ,reg #xF) #xF))))

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

(provide 'eboy-macros)

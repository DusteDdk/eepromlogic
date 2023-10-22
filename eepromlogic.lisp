; An EEPROM memory chip can be used as a programmable logic device if you think of it this way:
;           The address pins are input pins, the data pins are output pins, and the memory stores the bitpattern that results from the logic operation.
;
; This tool makes it very easy to generate such truth-tables, it allos you to use a 64 kbyte (16 bit address, 8 bit data) eeprom chip as programmable logic. (instead of a PLA for slow enough applications, for example)
; Fairly advanced functions can be defined very easily.
; Disclaimer: I'm not an electronics person, and this is also my first "useful" lisp program, so, it's very ugly and inefficient and inelegant
; and all the other "in's" of negative connotation. But it will work for my purpose.
;
; Check examples.lisp and README.md for info on how to use it.
;
; License: WTFPL

(defpackage :eeprom-logic
    (:use :cl)
    (:export
        #:nand
        #:nor
        #:xor
        #:xnor
        #:on
        #:off
        #:truth
        #:btn
        #:ltnz
        #:q))

(in-package :eeprom-logic)


(defun nand (a b) 
    (not (and a b)))

(defun nor (a b) 
    (not (or a b)))

(defun xor (a b)
  (nand (nand a (nand a b))
        (nand b (nand a b))))

(defun xnor (a b)
        (not (xor a b)))

(defmacro q ((&rest keys) &body body)
  `(lambda (inaddrbits)
     (destructuring-bind (&key ,@keys &allow-other-keys) inaddrbits
       (progn ,@body)  )))


(defmacro update-states (&rest state-result-pairs)
  `(progn
     ,@(loop for (s r) on state-result-pairs by #'cddr
             collect `(if (or (eq ,r :on) (eq ,r :off))
                          (progn
                            (setq ,s :pass)
                            (setq ,r (eq ,r :on))
                          ))
             collect `(if (not (eq ,s :pass))
                          (if (eq ,s :new)
                              (setq ,s ,r)
                              (if (not (eq ,s ,r))
                                  (setq ,s :pass)))))))

; takes a bit and turns it into either 0 or 1
(defun btn (bit) (if bit 1 0))

; Takes a list of bits (t / nil) and turns them into a number
(defun ltnz (bitlist)
    (reduce (lambda (acc bit) (+ (* acc 2) (btn bit) )) bitlist :initial-value 0))


(defun check (state num)
  (if (eq state :pass)
        t
        (progn (format t "Logic error, bit ~A is stuck. If that is on purpose, return :on or :off to indicate it will never change. ~%" num) nil)))


; The truth function takes the following arguments
; show  truth table to console (t or NIL)
; filename (string or nil) - where to write the binary file (ready for your eeprom) (unless nil, then we don't write)
; start address (number)
; stop address (number)
; 8 Lambdas that determine the value of each bit for each input combination, use the o macro. 
; Note the order: From LSB to MSB! (First lambda determines output state of bit 0 (the D0 or Q0 pin on the EEPROM))

(defun truth (show outputfilename fistAddr lastAddr o0 o1 o2 o3 o4 o5 o6 o7)

    (format t "~% ~%Generating truth table...~%")

    (if (or (not (eq fistAddr 0)) (not (eq lastaddr (- (* 1024 64) 1))))
            (format t "Note: Not writing a full 64kib eeprom, logic check fail, binary file might not work. ~%"))
    
    (let ((s0 :new) (s1 :new) (s2 :new) (s3 :new) (s4 :new) (s5 :new) (s6 :new) (s7 :new))
            
        
    (let ((resultlist (loop for i from fistAddr to lastAddr 
              collect (let ((inaddr (list
                            :adr i
                            :a15 (logbitp 15 i)
                            :a14 (logbitp 14 i)
                            :a13 (logbitp 13 i)
                            :a12 (logbitp 12 i)
                            :a11 (logbitp 11 i)
                            :a10 (logbitp 10 i)
                            :a9 (logbitp 9 i)
                            :a8 (logbitp 8 i)
                            :a7 (logbitp 7 i)
                            :a6 (logbitp 6 i)
                            :a5 (logbitp 5 i)
                            :a4 (logbitp 4 i)
                            :a3 (logbitp 3 i)
                            :a2 (logbitp 2 i)
                            :a1 (logbitp 1 i)
                            :a0 (logbitp 0 i))))

                  (let ((inaddrNumeric (loop for (_ value) on (cdr (cdr inaddr)) by #'cddr collect (if value 1 0))))
                            (let ((r0 (funcall o0 inaddr)))
                                (let ((r1 (funcall o1 (append inaddr (list :d0 r0)))))
                                    (let ((r2 (funcall o2 (append inaddr (list :d0 r0 :d1 r1)))))
                                        (let ((r3 (funcall o3 (append inaddr (list :d0 r0 :d1 r1 :2 r2)))))
                                            (let ((r4 (funcall o4 (append inaddr (list :d0 r0 :d1 r1 :d2 r2 :d3 r3)))))
                                                (let ((r5 (funcall o5 (append inaddr (list :d0 r0 :d1 r1 :d2 r2 :d3 r3 :d4 r4)))))
                                                    (let ((r6 (funcall o6 (append inaddr (list :d0 r0 :d1 r1 :d2 r2 :d3 r3 :d4 r4 :d5 r5)))))
                                                        (let ((r7 (funcall o7 (append inaddr (list :d0 r0 :d1 r1 :d2 r2 :d3 r3 :d4 r4 :d5 r5 :d6 r6)))))

                                                                (update-states s0 r0 s1 r1 s2 r2 s3 r3 s4 r4 s5 r5 s6 r6 s7 r7)
                                                                (let ((databits (list
                                                                    (if r7 1 0)
                                                                    (if r6 1 0)
                                                                    (if r5 1 0)
                                                                    (if r4 1 0)
                                                                    (if r3 1 0)
                                                                    (if r2 1 0)
                                                                    (if r1 1 0)
                                                                    (if r0 1 0))))
                                                                                    
                                                                    (list :adr i :adrbits inaddrnumeric :databits databits :datanumeric (reduce (lambda (acc bit) (+ (* acc 2) bit )) databits :initial-value 0))))))))))))))))
                                                             
            
                          

            (if show 
                (progn
                 (format t "                                 ADDRESS  =>  DATA ~%")
                 (format t "        F E D C B A 9 8 7 6 5 4 3 2 1 0       7 6 5 4 3 2 1 0~%")
                    (loop for l in resultlist do 
                               (format t "  ~4,'0X ~A  => ~A ~4,'0X ~%" (getf l :adr) (getf l :adrbits) (getf l :databits) (getf l :datanumeric) ))))

            

            (if (and (check s0 0) (check s1 1) (check s2 2) (check s3 3) (check s4 4) (check s5 5) (check s6 6) (check s7 7))
                (progn
                    (format t "Logic check passed (Checked addresses ~4,'0X to ~4,'0X)~%" fistAddr lastaddr)
                    (if outputfilename 
                    (progn
                        (format t ">> Writing to file: ~A ~%" outputfilename )
                        (with-open-file (stream outputfilename :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
                            (loop for l in resultlist do (write-byte (getf l :datanumeric) stream))))))
                (progn
                    (format t "Logic check failed (Checked addresses ~4,'0X to ~4,'0X)~%" fistAddr lastaddr)
                    (if outputfilename (format t "Not writing to file when there are errors. ~%" ))))
            
            resultlist)))
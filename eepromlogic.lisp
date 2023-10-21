; See bottom of file. Tool to generate truth-tables, to use 64 kbyte (16 bit address, 8 bit data) eeprom chips as programmable logic.
; Fairly advanced functions can be defined, very easily.
; Disclaimer: I'm not an electronics person, and this is also my first "useful" lisp program, so, it's very ugly and inefficient and inelegant
; and all the other in's of negative connotation. But it will work for my purpose.
; One thing I find kinda neat is that the state of previous (less significant) data pins are available as inputs for the functions the state
; for more significant data pins. The logic checker will only work correctly when generating from 0 to 65535.



(defun nand (a b) 
    (not (and a b)))

(defun nor (a b) 
    (not (or a b)))

(defun xor (a b)
  (nand (nand a (nand a b))
        (nand b (nand a b))))

(defun xnor (a b)
        (not (xor a b)))

(defmacro o ((&rest keys) &body body)
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

; Takes a list of truthy bits and turns them into a number
(defun bton (bitlist)
    (reduce (lambda (acc bit) (+ (* acc 2) (if bit 1 0) )) bitlist :initial-value 0))


(defun check (state num)
  (if (eq state :pass)
        t
        (progn (format t "Logic error, bit ~A is stuck. If that is on purpose, return :on or :off to indicate it will never change. ~%" num) nil)))


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
                 (format t "                                 ADDRESS  =>  DATA ~C" #\newline)
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
                    (if outputfilename (format t "Not writing to file when there are errors. ~%" )))))))
                      
         

; The truth function takes the following arguments
; show  truth table to console (t or NIL)
; filename (string or nil) - where to write the binary file (ready for your eeprom) (unless nil, then we don't write)
; start address (number)
; stop address (number)
; o0 ... o7 Lambdas that determine the value of each bit for each input combination, use the o macro
; Note the order: From LSB to MSB!

; The example below example describes
;   3 input AND gate with inputs from A0 A1 A2 and output on D0
;   2 input NAND with input A3 A4 and output D1
;   Inverter with input A1 and output D2
;   Address 8 or 16 gives and output on D3 by checking the adr number rather than individual bits
;   An XOR gate taking A5 and D0 as inputs, result on D4
;   No other functions are implemented. D5 and D6 will always be low, and D7 will always be high.
; Note: To not spam the console, only the first 64 entries are generated, to fill a whole 64k eeprom, use 65535 instead of 64.
; Note: The last item in the list defined the behaviour of D0, the first defined behaviour of D7.
; Note: The lisp-native operations (or, and) will take any number of arguments, while mine, (nand, nor, xor, xnor) take only two, so you must either define your own or nest them to achieve the type of logic you desire
; Note: The logic can be whatever lisp operation you want, but it must return either t or nil.
; Note: All o functions have available the following input variables: (a0 a1 a2 a3 a4 a5 a6 a7 adr)
;       a0-7 are the t / nil boolean state of the address pin
;       adr is the entire address as a normal number
;       o functions also have the output result of all the previous functions available, except for the first one (Output logic for pin D0) since there is no previous outputs.
;           Example: The function for D3 has (a0 a1 a2 a3 a4 a5 a6 a7 adr d0 d1 d2) available, and the function for D7 has all of them (a0..a7 + adr + d0..d6)

(truth nil "test.bin" 0 65535
     (o (a0 a1 a2) (and a0 a1 a2))          ; Output logic for pin D0
     (o (a3 a4) (nand a3 a4))               ; Output logic for pin D1
     (o (a1) (not a1))                      ; Output logic for pin D2
     (o (adr) (or (eq adr 8) (eq adr 16)))  ; Output logic for pin D3
     (o (a4 d0) (xor a4 d0))                ; Output logic for pin D4
     (o () :off)                            ; Output logic for pin D5
     (o () :off)                            ; Output logic for pin D6
     (o () :on)                             ; Output logic for pin D7
)

; This implements an adder capable of adding a carry, and two 7 bit numbers into a 7 bit result + carry

; a0 - carry in
; a1 - bit A1 in
; a2 - bit B1 in
; a3 - bit A2 in
; a4 - bit B2 in
; a5 - bit A3 in
; a6 - bit B3 in
; a7 - bit A4 in
; a8 - bit B4 in
; a9 - bit A5 in
; a10 - bit B5 in
; a11 - bit A6 in
; a12 - bit B6 in
; a13 - bit A7 in
; a14 - bit B7 in
;
;d0 - sum bit 0 out
;d1 - sum bit 1 out
;d2 - sum bit 2 out
;d3 - sum bit 3 out
;d4 - sum bit 4 out
;d5 - sum bit 5 out
;d6 - sum bit 6 out
;d7 - carry-out

; Convenience function that implements a 7 bit adder (+carry in/out) and returns bit by position indicated in resbit
(defun adder (carry-in A1 B1 A2 B2 A3 B3 A4 B4 A5 B5 A6 B6 A7 B7 resbit)
    ; We just convert them back into lisp numbers and extract their bits ;)
    (let ((na (bton (list a7  a6 a5 a4 a3 a2 a1 )))
          (nb (bton (list b7  b6 b5 b4 b3 b2 b1 )))
          (carry (bton (list carry-in))))
        (logbitp resbit (+ na nb carry))))

(truth nil "adder.bin" 0 65535
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 0))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 1))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 2))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 3))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 4))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 5))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 6))
     (o (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 7))
)

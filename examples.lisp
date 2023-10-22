; Get access to the sweet stuff
(load "eepromlogic.lisp")

(defpackage :eeprom-logic-examples
     (:use :cl :eeprom-logic))
(in-package eeprom-logic-examples)


; See README.md for documentation on what functions/macros are now available and how to use them.
; Or peruse the examples below.





; Example 1 - Output to console
; Print the first 4 input states of the adder to the console, 
; showing the truth-table for the XOR function (input bits 0 and 1, output bit 0)

(truth t nil 0 3
       (q (a0 a1) (xor a0 a1))
       (q () :off)
       (q () :off)
       (q () :off)
       (q () :off)
       (q () :off)
       (q () :off)
       (q () :off))





; Example 2 - The common logic gates
; Here is an example with the 7 common logic gates, and a "straight through" (can be used for delaying a line with the same delay as the rest of the eeprom)
; Input A0,  A1  Output D0 - AND
; Input A2,  A3  Output D1 - OR
; Input A4,  A5  Output D2 - XOR
; Input A6       Output D3 - NOT
; Input A7,  A8  Output D4 - NAND
; Input A9,  A10 Output D5 - NOR
; Input A11, A12 Output D6 - XNOR
; Input A13      Output D7 - Straight through
; Inputs 14, 15 are unused and ignored

(truth nil "gates.bin" 0 65535
       (q (a0 a1) (and a0 a1))          ; D0
       (q (a2 a3) (or a2 a3))           ; D1
       (q (a4 a5) (xor a4 a5))          ; D2
       (q (a6) (not a6))                ; D3
       (q (a7 a8) (nand a7 a8))         ; D4
       (q (a9 a10) (nor a9 a10))        ; D5
       (q (a11 a12) (xnor a11 a12))     ; D6
       (q (a13) a13)                    ; D7
)





; Example 3 - Some random stuff
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
;       q functions also have the output result of all the previous functions available, except for the first one (Output logic for pin D0) since there is no previous outputs.
;           Example: The function for D3 has (a0 a1 a2 a3 a4 a5 a6 a7 adr d0 d1 d2) available, and the function for D7 has all of them (a0..a7 + adr + d0..d6)

(truth nil "example.bin" 0 65535
     (q (a0 a1 a2) (and a0 a1 a2))          ; Output logic for pin D0
     (q (a3 a4) (nand a3 a4))               ; Output logic for pin D1
     (q (a1) (not a1))                      ; Output logic for pin D2
     (q (adr) (or (eq adr 8) (eq adr 16)))  ; Output logic for pin D3
     (q (a4 d0) (xor a4 d0))                ; Output logic for pin D4
     (q () :off)                            ; Output logic for pin D5
     (q () :off)                            ; Output logic for pin D6
     (q () :on)                             ; Output logic for pin D7
)





; Example 4 - A full-adder, the bits of the two input numbers are interlaced
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
    (let ((na (ltn (list a7  a6 a5 a4 a3 a2 a1 )))
          (nb (ltn (list b7  b6 b5 b4 b3 b2 b1 )))
          (carry (btn (list carry-in))))
        (logbitp resbit (+ na nb carry))))

(truth nil "adder.bin" 0 65535
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 0))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 1))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 2))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 3))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 4))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 5))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 6))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 7))
)





; Example 5 - Full adder, input numbers organised next to each other instead of interlaced
; Another, adder organised so that it's more convenient to wire up:
; a0 - carry-in
; a1..a7 - number A in
; a8..14 - number B in
; d0..d6 - Sum of A+B out
; d7 - Carry out

(defun adder2 (carry-in a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 resbit)
    (adder carry-in A1 B1 A2 B2 A3 B3 A4 B4 A5 B5 A6 B6 A7 B7 resbit))

(truth nil "adder2.bin" 0 65535
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 0))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 1))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 2))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 3))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 4))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 5))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 6))
     (q (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)  (adder2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 7))
)

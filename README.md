Easy way to use EEPROMs as programmable logic.
==

```
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

(truth t "test.bin" 0 64
     (o (a0 a1 a2) (and a0 a1 a2))          ; Output logic for pin D0
     (o (a3 a4) (nand a3 a4))               ; Output logic for pin D1
     (o (a1) (not a1))                      ; Output logic for pin D2
     (o (adr) (or (eq adr 8) (eq adr 16)))  ; Output logic for pin D3
     (o (a4 d0) (xor a4 d0))                ; Output logic for pin D4
     (o () :off)                            ; Output logic for pin D5
     (o () :off)                            ; Output logic for pin D6
     (o () :on)                             ; Output logic for pin D7
)
```

Here's the example output:

```
Generating truth table...
Note: Not writing a full 64kib eeprom, logic check fail, binary file might not work. 
                                 ADDRESS  =>  DATA 
  0000 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  0001 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  0002 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  0003 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)  => (1 0 0 0 0 0 1 0) 0082 
  0004 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  0005 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  0006 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  0007 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)  => (1 0 0 1 0 0 1 1) 0093 
  0008 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0)  => (1 0 0 0 1 1 1 0) 008E 
  0009 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  000A (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  000B (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1)  => (1 0 0 0 0 0 1 0) 0082 
  000C (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  000D (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  000E (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  000F (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)  => (1 0 0 1 0 0 1 1) 0093 
  0010 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0)  => (1 0 0 1 1 1 1 0) 009E 
  0011 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1)  => (1 0 0 1 0 1 1 0) 0096 
  0012 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0)  => (1 0 0 1 0 0 1 0) 0092 
  0013 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1)  => (1 0 0 1 0 0 1 0) 0092 
  0014 (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0)  => (1 0 0 1 0 1 1 0) 0096 
  0015 (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)  => (1 0 0 1 0 1 1 0) 0096 
  0016 (0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0)  => (1 0 0 1 0 0 1 0) 0092 
  0017 (0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1)  => (1 0 0 0 0 0 1 1) 0083 
  0018 (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0)  => (1 0 0 1 0 1 0 0) 0094 
  0019 (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1)  => (1 0 0 1 0 1 0 0) 0094 
  001A (0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0)  => (1 0 0 1 0 0 0 0) 0090 
  001B (0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)  => (1 0 0 1 0 0 0 0) 0090 
  001C (0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0)  => (1 0 0 1 0 1 0 0) 0094 
  001D (0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1)  => (1 0 0 1 0 1 0 0) 0094 
  001E (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0)  => (1 0 0 1 0 0 0 0) 0090 
  001F (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)  => (1 0 0 0 0 0 0 1) 0081 
  0020 (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  0021 (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  0022 (0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  0023 (0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1)  => (1 0 0 0 0 0 1 0) 0082 
  0024 (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  0025 (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  0026 (0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  0027 (0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1)  => (1 0 0 1 0 0 1 1) 0093 
  0028 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  0029 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  002A (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  002B (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1)  => (1 0 0 0 0 0 1 0) 0082 
  002C (0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0)  => (1 0 0 0 0 1 1 0) 0086 
  002D (0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1)  => (1 0 0 0 0 1 1 0) 0086 
  002E (0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0)  => (1 0 0 0 0 0 1 0) 0082 
  002F (0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1)  => (1 0 0 1 0 0 1 1) 0093 
  0030 (0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0)  => (1 0 0 1 0 1 1 0) 0096 
  0031 (0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1)  => (1 0 0 1 0 1 1 0) 0096 
  0032 (0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0)  => (1 0 0 1 0 0 1 0) 0092 
  0033 (0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1)  => (1 0 0 1 0 0 1 0) 0092 
  0034 (0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0)  => (1 0 0 1 0 1 1 0) 0096 
  0035 (0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1)  => (1 0 0 1 0 1 1 0) 0096 
  0036 (0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 0)  => (1 0 0 1 0 0 1 0) 0092 
  0037 (0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1)  => (1 0 0 0 0 0 1 1) 0083 
  0038 (0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0)  => (1 0 0 1 0 1 0 0) 0094 
  0039 (0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1)  => (1 0 0 1 0 1 0 0) 0094 
  003A (0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0)  => (1 0 0 1 0 0 0 0) 0090 
  003B (0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1)  => (1 0 0 1 0 0 0 0) 0090 
  003C (0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0)  => (1 0 0 1 0 1 0 0) 0094 
  003D (0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1)  => (1 0 0 1 0 1 0 0) 0094 
  003E (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0)  => (1 0 0 1 0 0 0 0) 0090 
  003F (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)  => (1 0 0 0 0 0 0 1) 0081 
  0040 (0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0)  => (1 0 0 0 0 1 1 0) 0086 
Logic check passed (Checked addresses 0000 to 0040)
>> Writing to file: test.bin 
```
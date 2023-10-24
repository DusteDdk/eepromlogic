# Easily use EEPROMs as programmable logic

Once upon a time in the old world, someone way smarter than me observed that EEPROM memory chips can be used as a programmable logic devices.

You can consider the address pins as input pins, and the data pins as output pins. You can define arbitrarily complex logic expressions for each output pin.

Said even less elegant, whatever logic function you can dream up, as long as its input relies only on the state of the input pins, you can make the EEPROM carry out.

## Example

Example: One of each of the common logic gates, just write the generated "gates.bin" to the EEPROM chip and wire it up.

```lisp
(load "eepromlogic.lisp")
(use-package :eeprom-logic)

(truth nil "gates.bin" 0 65535
       (q (a0 a1) (and a0 a1))          ; Input on A0 and A1. Output on D0
       (q (a2 a3) (or a2 a3))           ; D1
       (q (a4 a5) (xor a4 a5))          ; D2
       (q (a6) (not a6))                ; D3
       (q (a7 a8) (nand a7 a8))         ; D4
       (q (a9 a10) (nor a9 a10))        ; D5
       (q (a11 a12) (xnor a11 a12))     ; D6
       (q () :off)                      ; D7 is always 0
)
```

_This is a simple example.
You're not limited to "one gate per output", your expressions can be as complex as you need. 
See documentation and examples below._

## Examples
Intrigued? Read and run the [examples.lisp](examples.lisp).

The file contains a few examples of gates and other logic, such as 7 bit full adders with different pinouts, and a binary to hex 7 segment display decoder.

```bash
sbcl --load "examples.lisp"
```

## Documentation / Reference

The following stuff is made available:

* [truth](#truth) - Make truth table
* [q](#Q) - Create expression for output bit
* [ltn](#ltn) - Convert list of booleans to number
* [btn](#btn) - Convert boolean to number
* [nand](#gates) - 2 input logic
* [nor](#gates) - 2 input logicocs below.
* [xor](#gates) - 2 input logic
* [xnor](#gates) - 2 input logic

_Additionally, any LISP function can be used to build expressions._

### Which EEPROMs can I use ?
You can use any eeprom arranged into 8 bytes, with up to 20 address, see [EEPROM sizes](#eeprom-sizes).

In my examples, I'm using an  Atmel AT27C512 (64Kx8). Feel free to expand this package to work with 16 bit chips.


### How to think about descibing logic
When using this tool, it's helpful to think of what you're doing in a certain way: You are describing a desired output state for a given input state.

Abstract enough for you? Okay, look at some chip, it has input pins and output pins, now, select a couple of input pins, and _ONE_ output pin.

You want to describe the logic that sets the state of that _ONE_ output pin based on those specific input pins. Nothing more, nothing less.

Basically, you will define 8 expressions, one for each output bit, which the "truth" function will then call, once for every possible bit combination, to generate the truth table.

Always think about one _output_ pin at a time.

That might feel constrained but it's not. You're totally free to use _as many or as few_ of the input pins, in whatever combination you want, along with any lisp function, to build your expression.

It's also fine to use the same input pins in the expressions for muliple output pins.

_Start by having a look at the truth function and q macro._


### Truth

* [Truth function](#truth-function)
* [Truth logic check](#truth-checks)
* [Truth lastAddr: EEPROM sizes](#eeprom-sizes)

### Truth function

The truth function generates the truth table. It can show it on the screen. It can also save it to a binary file, ready to write onto a EEPROM. This is the main function you'll want to call to do anything with this package.

```lisp
(truth
       show          ; Show truth table on console
       filename      ; Save binary to file
       firstAddr     ; Generate from this address
       lastAddr      ; Generate to this address
       q0            ; Logic expression for EEPROM pin Q0
       q1            ; Logic expression for EEPROM pin Q1
       q2            ; Logic expression for EEPROM pin Q2
       q3            ; Logic expression for EEPROM pin Q3
       q4            ; Logic expression for EEPROM pin Q3
       q5            ; Logic expression for EEPROM pin Q5
       q6            ; Logic expression for EEPROM pin Q6
       q7            ; Logic expression for EEPROM pin Q7
)
```

* show - (t / nil) Show truth table on console so we can see how our expressions behave
* filename - (nil / string) If not nil: Save binary output to this filename
* firstAddr - Generate from this address (usually 0)
* lastAddr - Generate to this address (Usually EEPROM size, see [eeprom sizes](#eeprom-sizes)))


You want "show" to be nil when generating a full truth-table, otherwise it's a lot of output.

If you want to check the output of just a single pattern, you can set "firstAddr" and "lastAddr" to the same value, or a short range covering the patterns you want to check. The #b prefix to a number is convenient, so you can type in the binary pattern you want to see the output for. For example, to check what happens when both A0 and A2 is set, you could set both "firstAddr" and "lastAddr" to #b101

The "q" parameters are lambda functions, but you won't notice that, use [the Q macro](#q) to write them.

_Note_: The bit-order of the "truth" function is LSB (least significant bit first) because, I find it's easier to
understand that I'm defining the expression for the first bit first and the last bit last... This means that the q0 parameter to "truth" defines  the logic behaviour for output bit 0, so it is the _rightmost_ bit in the truth table.

_Everywhere else, the bit-order is MSB (most significant bit  first)._

[Truth overview](#truth)

#### Truth checks
The truth function will check that all functions returned both t and nil at some point (that they are not always providing the same output), this check will fail if you write some logical error, such as ```(and a0 (not a0))```.

It will often fail when not generating the entire table, so if you expect your check to pass, but it fails, try using the full address range.

If you don't want to implement any behaviour for an output, use the  ```:on ``` or  ```:off ``` symbol, instead of t or nil. This allows the checker to understand that it's intended that those outputs never change.
```lisp
(q () :off) ; Bit always off
```

[Truth overview](#truth)
#### EEPROM Sizes

When generating a binary output file for writing to the EEPROM, be sure to select the correct "firstAddr" and "lastAddr".

"firstAddr" should probably always be 0.

If you set the "lastAddr" value too small, your chip won't be fully programmed any floating address pins may generate wrong results.

If you set the "lastAddr" value too large, the generated binary is too big and won't fit on the chip, you may truncate it, it might or might not work, but the logic check wouldn't have warned you if you requested address bits from pins not actually present on your chip.

An easy way to set the right "lastAddr" is to simply count the address pins on your chip, and punch 1 for every pin, that binary number is your "lastAddr", you can prefix a binary number with ```#b``` and use it directly

Example: For a chip with 10 address pins, the "lastAddr" is the binary number "ten ones", so #b1111111111 in binary, #x3FF in hex and  1023 in decimal.

"truth" don't care which representation you use, they're all the same number.

| Chip  |    Size | Arrangement | Available addresses | lastAddr |
|-------|---------|-------------|---------------------|----------|
| 27080 | 80 Mbit |      1M x 8 |           A0 to A19 |  #xFFFFF |
| 27040 | 40 Mbit |    512K x 8 |           A0 to A18 |  #x7FFFF |
| 27512 | 64 Kbit |     64K x 8 |           A0 to A15 |   #xFFFF |
| 27256 | 32 Kbit |     32K x 8 |           A0 to A14 |   #x3FFF |
| 27128 | 16 Kbit |     16K x 8 |           A0 to A13 |   #x1FFF |
|  2764 | 64 Kbit |      8K x 8 |           A0 to A12 |    #xFFF |
|  2732 | 32 Kbit |      4K x 8 |           A0 to A11 |    #x7FF |
|  2716 | 16 Kbit |      2K x 8 |           A0 to A10 |    #x3FF |
|   You |     get |         the |                idea |      now |
|  2704 |  4 Kbit |     512 X 8 |            A0 to A8 |     #xFF |


Note: "truth" don't detect if you request address bits for pins not on your chip, those bits will always be set to 0, and your logic will not work.

[Truth overview](#truth)

[Package overview](#documentation--reference)
### Q

* [The Q Macro](#the-q-macro)
* [Q - More detail](#q-extra-neatness)
* [Q - Slightly weird stuff](#q-brain-melty-extra-messiness)
* [Q - Clarification](#q-clarification)


### The Q Macro
The Q macro allows you to write a logic expression for an output pin (the data or "Q" pin on the EEPROM chip).

You must provide an expression for each of the q0..q7 parameters that the [truth](#truth) function takes.

Usage:
```lisp
(q 
       (inputs)
       logic)
```

Example usage:
```lisp
(truth t nil 0 3                   ; Show truth table for address 0..3
       (q                          ; Create expression for pin Q0
              (a2 a5)              ; Request the a2 and a5 variables
              (and a2 (not a5)))   ; Implement the expression

       (q                          ; Create expression for pin Q1
              (a1)                 ; Request the a1 variable
              (not a1))            ; Set pin Q1 to the inverse of A1
       
       (q                          ; Create expression for pin Q2
              (a2)                 ; Request a2 variable
              a2)                  ; Set pin Q2 equal to the state of A2
       ;; Snip for brewity, but all 8 Qs must be provided.
```

The logic expression is evaluated by "truth" for every possible input combination and its resulting state saved in the truth table.

It works by you telling which input bits (address pins on the EEPROM chip) you want to consider in your expression, and an expression that returns the desired state of the output bit based on the state of those input bits.


You can do whatever LISP you want in your expression, but remember each expression is evaluated only once per bitpattern, there's no state to be modified inside the EEPROM.

_Your expression must return t or nil._

Here's an example, where we want pins A0, A5 and A10 for some reason, and we want the output bit to be 1 if exactly any two bits are set and 0 otherwise.

We could implement this with pure boolean logic. But all of LISP is available and we can solve it however we please. In this example I convert the boolean state of the pins to numbers (1 or 0) and sum them, if the result is 2, then exactly two of the input pins were high.

```lisp
(q (a0 a5 a10) ; Request the a0 a5 and a10 variables
       (eq (+ (btn a0) (btn a5) (btn a10)) 2)) ; The logic expression
```

_Remember that you can use the same input in as many of your expressions as you want:_

```lisp
(truth t nil 0 1
       (q (a0 a1) (and a0 a1)) ; q0 - Uses a0
       (q (a1 a2) (and a1 a2)) ; q1 - Also uses a0
       ; ... snip for brewity, all 8 Qs must be provided
```

[Q overview](#q)

#### Q Extra neatness!
Q provides two other inputs, the first is easy to understand: "adr", the integer value on the address bus.

In this example, we want the output pin to be high whever the number present on the bus is in the range 16384..24576

```lisp
(q (adr) (and (>= adr 16384) (<= adr 24576)))
```

We can also check for a specific address:
```lisp
(q (adr) (eq adr 34215))
```

_The adr variable is convenient for creating memory mappers, decoders and bus controllers._

[Q overview](#q)
#### Q Brain-melty extra messiness!
The next feature is slightly less obvious: When defining logic expressions for output pins HIGHER than q0, you can also access the output state of all the lower pins!

For q0, there's no extra information, because there is no lower pin.

For q1, you can access the output of q0 through a variable named d0

For q7, you can access the output of q0..q6 through variables d0..d6!

In this example, we define q7, let's say we want q7 to be high if _any_ other pin is high:

```lisp
(q (d0 d1 d2 d3 d4 d5 d6) (or d0 d1 d2 d3 d4 d5 d6))
```

You're allowed to mix them all, say this might be for o3
```lisp
(q (d1 a0 adr) (and (> adr 5) (or d1 a0)))
```

#### Q Clarification

The q macro allows you to request for for variables a0..a19 d0..d6 and "adr" to be available in your expression body, and then define a logic expression using those variables to determine the output state for the bit.

"adr" is a number, a0..a19 and d0..d6 are boolean.

_Wonder how to tell the q macro which bit you're defining output state for ?_

The argument position in the call to "truth" determines this. "truth" calls the every function in order from q0 to q7, first function describes expression for lowest bit, this should be clear if you re-read the [truth function](#truth-function) description.

_Wonder how to tell the q macro that you don't want to implement anything on a pin, and not have the checker tell you about "stuck bits" ?_  See the [truth checks](#truth-checks) section.

[Q overview](#q)

[Package overview](#documentation--reference)

### Ltn
Convert list of booleans representing a binary number to a number, most significant bit first in list.

```lisp
(ltn (list t t nil)) ; => 6
```

[Package overview](#documentation--reference)

### Btn
Convert a single boolean to number. 

```lisp
(btn (nil)) ; => 0
(btn (t))   ; => 1
```

[Package overview](#documentation--reference)

### Gates 
The gates nand nor xor xnor implement the logic functions they're named after.

They all take exactly two boolean arguemnts and return a boolean value.
```lisp
(nand t t)
(xor a1 a5)
(nor d1 a6)
```

_Remember, LISP also provides logic functions that you can use, among them are ```not```, ```and```, ```or```, ```if```, ```eq```, ```>```, ```>=```, ```<=```, ```logbitp``` and lots of other stuff I don't know about, but as long as it ends up t or nil, it's fine to use._

[Package overview](#documentation--reference)

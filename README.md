# Easily use EEPROMs as programmable logic

Someone way smarter than me observed that one can use an EEPROM memory chip as a programmable logic device.

That is, if you consider the address pins to be input pins, and the data pins to be output pins for which you can define arbitrarily complex (as long as it's stateless) logic functions for.

Said even less elegant, whatever logic function you can dream up, as long as its input relies only on the state of the input pins, you can make the EEPROM carry out.

## Example

A simple example, one of each of the common logic gates on an EEPROM, just program gates.bin to the chip and hook it up:

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

This is a very simple example, you're not limited to the logic of one gate per output, there's no limit on the amount of logic operations you can describe between the inputs and an pin.

* See documentation and examples below

## How to run the examples ? 
Sold? Carry on and read run the [examples.lisp](examples.lisp).

```bash
sbcl --load "examples.lisp"
```

## Documentation / Reference

The following stuff is made available:

* [truth](#truth) - Make truth table
* [q](#Q) - Describe logic output
* [ltn](#ltn) - Convert list of booleans to number
* [btn](#btn) - Convert boolean to number
* [nand](#gates) - 2 input logic
* [nor](#gates) - 2 input logicocs below.
* [xor](#gates) - 2 input logic
* [xnor](#gates) - 2 input logic

Of course any LISP function (and/not/or and whatever you have) that returns t or nil can be used too.

### Which EEPROMs can I use ?
You can use any eeprom arranged into 8 bytes, to 8 mbit, so the biggest one the 27080 (1Mx8).
In my examples, I'm using a  27512 (64Kx8). Feel free to expand this to work with 16 bit chips.

### How to think about descibing logic
When using this tool, it's helpful to think of what you're doing in a certain way, which is: describing a desired output state for a given input state.

Abstract enough for you? Okay, look at some chip, it has input pins and output pins, now, select a couple of input pins, and _ONE_ output pin.

You want to describe the logic that sets the state of that _ONE_ output pin based on those specific input pins. Nothing more, nothing less.

Did that help ? If it did, this is for you.

Basically, you will tell the "truth" function what to do with each of the 8 output pins. So you really want to get this straight, thinking about one output pin at a time.

That might feel constrained but it's not, because: You're totally free to use as many or as few of the input pins, in whatever combination you want..

It's even okay for you to, for example, use the same input pins in the logic for muliple output pins.

I'll short circuit your brain later by telling you how you're allowed to use the OUTPUT state of some pins as inputs for the logic of other pins!


### Truth

The truth function generates the truth table and shows it to screen and saves it to binary file.

```lisp
(truth show filename firstAddr lastAddr q0 q1 q2 q3 q4 q5 q6 q7)
```

* show - (t / nil) Print the truth table to console so we can check what we're doing
* filename - (nil / string) Save the binary output to this file (if not nil)
* firstAddr - Generate from this address (usually 0)
* lastAddr - Generate to this addres (usaully 65535) but up to 
* q0 - Logic for EEPROM pin Q0
* q1 - Logic for EEPROM pin Q1
* q2 - Logic for EEPROM pin Q2
* q3 - Logic for EEPROM pin Q3
* q4 - Logic for EEPROM pin Q3
* q5 - Logic for EEPROM pin Q5
* q6 - Logic for EEPROM pin Q6
* q7 - Logic for EEPROM pin Q7

You generally want "show" to be nil when generating the full truth-table, because, it's a lot of output nobody's ever going to read.

"firstAddr" and "lastAddr" are useful when developing logic, generate a piece of the table and see if everything is as expected.

If you want to check the output of just a single pattern, you can set them as the same value. The #b prefix to a number is convenient, so you can type in the binary pattern you want to see the output for. For example, to check only happens when both A0 and A2 is set, you could set them both to #b101

The "q" parameters are lambda functions, but don't let that scare you, they're easy to make with the ["q" macro](#q)

It should be noted that the bit-order of the "truth" function uses LSB (least significant bit first) because, I find it's easier to
understand that I'm defining the first bit first and the alst bit last... While all other representations, lists and functions use MSB (most significant bit first).

This means that the q0 parameter defines the logic behaviour for output bit 0, and so it is the RIGHTMOST bit in the truth table.

Take a look at the examples.lisp file, it makes it very clear.

#### Truth checks
The truth function will check that all functions has returned both t and nil (that they are not always providing the same output), this check will fail if you write some logical error, such as (and a0 (not a0)).

It will often fail when not generating the entire table.

Use the :on and :off symbol to indicate that logic is not implemented for an output (see Q).


[To the top](#stuff-this-provides)
### Q

The Q macro helps you write logic definitions for an output pin (the data or "Q" pin on the EEPROM chip)
```lisp
(q (inputs) logic)
```

This macro allows you to describe the logic for a single output bit. The logic will then be evaluated by "truth" for every possible input combination and its resulting state saved in the truth table.

It works by you telling which information you want available, and then some logic that returns the state of the bit based on that information.

You can do whatever LISPness you want in there, but of course, you can't have state, so reading/setting variables probably won't give the results you want.

The return value of your function must be t or nil.

Here's an example, where we want pins A0, A5 and A10 for some reason, and we determine the output to be t if any two bits are set and nil in all other cases.

Since we're not constrained to gate-logic, we can just convert the binary state of the pins to either 1 and 0 and add them, if the sum is 2, then any two were set.

```lisp
(q (a0 a5 a10) (eq (+ (btn a0) (btn a5) (btn a10)) 2))
```

Another example, where we define an "or" gate that takes pins aa0 and a1 as input (this is perfectly okay even though we used a0 in the previous example)
```lisp
(q (a0 a1) (and a0 a1))
```


The reason for using this rather than nil or t is that "truth" will check that all bits flip at least once to help find logic errors, these symbols allow you to express that no change in output is intentional.

#### Q Extra neatness!
Q provides two other interesting inputs, the first is easist to understand: "adr", the integer value on the address bus.

In this example, we want our output pin to be true whever the number present on the bus is in the range 16384..24576
```lisp
(q (adr) (and (=>adr 16384) (<= adr 24576)))
```

Here we want our output pin true only on address 34215
```lisp
(q (adr) (eq adr 34215))
```


#### Q Brain-melty extra messiness!
The next feature is slightly less obvious, but you, when defining logic functions for output pins HIGHER than 0, you can also access the output state of all the lower pins!

That means, that for q0, there's no such feature, because there is no lower pins, and for q1, you can only access the output of D0.. But for q7, you can access the output of
pins D0..D6!

In this example, we define q7, let's say we want d7 to be high if ANY other pin is high (yes, I chose OR because it's the LISP build one and it takes any number of arguments while my own ones are lazy and take only two)

```lisp
(o (d0 d1 d2 d3 d4 d5 d6) (or d0 d1 d2 d3 d4 d5 d6))
```

Of course you're allowed to mix them all, say this might be for o3
```lisp
(o (d1 a0 adr) (and (> adr 5) (or d1 a0)))
```

#### Q Clarification

The q macro allows you to "ask" for a0..a15 d0..d6 and adr and use those to determine the resulting state.

adr is a number, a0..a15 and d0..d6 are boolean.

Wonder how to tell the q macro which bit you're defining output state for ?

You don't! The argument position in the call to "truth" determines this, "truth" calls the functions in order from q0 to q7, which is also why the output of "lower numbered" data pins is available.

Wonder how to tell the q macro that you don't want to implement anything on a pin, and not have the checker tell you about "stuck bits" ? 

You can set the pin permanently high or low by using :on or :off
```lisp
(q () :off)
```

[To the top](#stuff-this-provides)

### ltn
Convert list of booleans to number.

```lisp
(ltn (list t t nil)) ; => 6
```

[To the top](#stuff-this-provides)

### btn
Convert boolean to number. 

```lisp
(btn (nil)) returns 0 and (btn (t)) ; => 1
```

[To the top](#stuff-this-provides)

### gates 
The gates nand nor xor xnor implement the logic functions they're named after.

They all take exactly two boolean arguemnts and return a boolean value.
```lisp
(nand t t)
(xor a1 a5)
(nor d1 a6)
```

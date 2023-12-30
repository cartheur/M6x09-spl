## M6x09-spl
A simple programming language for 6x09 based machines with Forth-like syntax.

M6x09-spl is a Forth-style concatenative stack language. The compiler is (currently) written in Python and emits 6809/6309 assembly code. 

Originally created by Ron Kneusel for the Apple II original, adapted for the Atari 8-bit and enhanced by Carsten Strotmann. This 6809/6309 port is based upon this buggy [repo](https://github.com/cartheur/M6x09-spl-6502).

TODO: Retarget to the HD6309 CPU because of its more rich instruction set and support for 32-bit registers. 

A significant change from Carsten Strotmann version is the option to specify some primitiives to compile as inline code. This is particularly useful on the 6809/6309 CPU with its second User stack, and excellent support of 16-bit data. The motivation for this change was to reduce "JSR" calls to many of the primitives when they can be reduced to a very few 6809 instructions and even more clearly illustrated for the 6309.

#!/usr/bin/python
##########################################################
#  SPL-6x09
#    An SPL compiler for the 6809/6309 CPU
##########################################################
#
#  6502 Atari version by Carsten Strotmann, found here:
#    https://github.com/cstrotm/spl-6502
#
#  This, directly from Carsten Strotmann's readme:
#
#  spl-6502
#  A simple programming language for 6502 based machines
#    with Forth-like syntax
#
#  SPL-6502 is a Forth-style concatenative stack language.
#    The compiler is (currently) written in Python and emits
#    6502 assembly code.
#
#  Originally created by Ron Kneusel for the Apple II
#   (original source <dead link removed>), adapted for the
#   Atari 8bit and enhanced by Carsten Strotmann
#  (cas@strotmann.de).
#
#  SPL is still work-in-progress, it has many "rough-edges"
#    and might fail in certain places. It has many bugs.
#    However, it works fine for some developers.
#
##########################################################
#
#  Attribution to Ron Kneusel for Apple ][ version,
#    found in the Carsten Strotmann spl.py file:
#
#  RTK, 03-Sep-2007
#  Last update:  18-Aug-2015
#
#  $Id$
#
#  This code is freeware, do as you please with it.
#
##########################################################
#
#  Retarget to MC6809/HD6309 CPU & lwtools toolchain
#  Tom LeMense [TJL], 28-Aug-2023
#
#  Started by using "2to3" to convert to Python 3
#  Assumes lwtools 6809/6309 assembler toolchain
#    see http://lwtools.projects.l-w.ca/
#  Adapted some specifics to match my HD6309 SBC
#    see https://github.com/tomcircuit/hd6309sbc
#
#  Changed some of the SPL syntax to suit my preferences:
#
#  Numeric literal representation:
#   <unary.op><storage.spec><radix.spec><magnitude>
#
#  Optional unary negation operator
#       '-' (2's complement) or
#       '~' (1's complement) or
#       '+' (positive)
#
#  storage.spec: b' or w' (default) or d' or f'
#       b' is a BYTE size quantity (8b)
#       w' is a WORD size quantity (16b)
#       d' is a DOUBLE size quantity (32b)
#       f' is a FLOAT quantity (32b single precision)
#
#  radix.specification 0b, 0, 0x
#       0b is binary
#       0 is octal
#       0x is hexadecimal
#       none is decimal
#       exception: a single 0 is treated as a decimal 0
#                  to make assembly output nicer
#
#  magnitude nnn
#       use appropriate digit characters, either case.
#       underbars '_' are allowed as as "numeric whitespace"
#       e.g. 16_384 is same as 16384
#
#  Every numeric literal ends up in a named tuple
#    "LiteralValue", ["sign", "mag", "size", "radix",
#                     "bound", "text"]
#
#  /* and */ style block comments are supported
#  ( ) blcok comment style is still supported
#  # line comment style is still supported
#
#  /# and #/ are the delimiters for Assembly code
#
#  Code Block style:
#       code xxx /# <assembly code> #/
#    creates a named code block that can be called
#    from elsewhere in the program.
#
#  Inline Assembly style is now supported
#       /# <assembly code> #/
#    creates inline assembly, without any label
#
#    In both cases, the text in between the delimeters
#    is echoed to the output source-file as-is.
#
#  HD6309 Implementation notes:
#
#  lwasm is called to create the output.  *.s (source)
#    and *.l (list) and output files are created.
#
#  Adapted code generation and library files for
#    for HD6309 instruction set. I tried to use as
#    few HD6309 specifics as possible (for example,
#    no widespread use of W (E & F) registers) but
#    in some cases, it was hard to resist. Note that
#    lwasm provides some "convenience instructions"
#    that are HD6309 nmemonics yet assemble to MC6809
#    equivalent instructions.
#    Goal: have a pramga/option for 6809-only output
#
#  Strings are null-terminated, no "count byte" is
#    included.
#
#  The argment stack pointer is the 16-bit U regiseter
#    (User Stack Pointer) and as a consequence the
#    user stack depth is no longer limited to 256 words.
#
#  Numeric constants are described by a named tuple,
#    LiteralValue which contains 'sign', 'mag', 'size',
#    'radix', 'bound', and 'text'. This allows for
#    bounds checking, as well as nicer assembly source
#    output (e.g. keep original radix).
#
#  Reworked 'dependency' mechanism to parse a json file
#    that contains entries for library functions
#    and their dependent files only. The json file is
#    parsed and loaded into the library_dependencies
#    dictionary at startup.
#
##########################################################

"""

ToDo:
   
   fix string lib function(s)
   
   get enough functions correct to be able to do some actual test programs for HD6309SBC

"""


import os
import sys
import math
import time
import re
import struct
import json
import subprocess
import argparse
from types import *
from collections import namedtuple
from struct import pack, unpack

#  Modification date
LAST_MOD = "28-AUG-2023"

# -----------------------------------------------------------------------
#  Configuration section.  Modify these values to reflect
#  your working environment and target system.
# -----------------------------------------------------------------------

#  Assembler name and/or path.  If the executable is in your
#  $PATH just the name is needed.  If on Windows, a good place
#  to put the lwasm.exe file is in the C:\WINDOWS\ directory in
#  which case all you need is "lwasm" here.
ASSEMBLER = "lwasm"
ASM_DIR = os.getenv("SPL_LWASM_PATH", default="")

#  Library and include paths.  If you have environment variables set
#  for these, they will be used.  If you do not set environment variables
#  the default value will be used.  Therefore, set the default value to
#  the full pathname, trailing / or \ included, to the place where you
#  set up the directories, if not using environment variables.
LIB_DIR = os.getenv("SPL_LIB_PATH", default="lib/")
INCLUDE_DIR = os.getenv("SPL_INCLUDE_PATH", default="include/")

#  Default base output file name (out.s) and type (asm)
BASE_NAME = "out"
BASE_TYPE = "asm"

#  Compiler version number
VERSION = "0.3HD6309"

#  Header for assembly output files, must be a list of strings
HEADER = [
    "",
    "Output from SPL compiler version " + VERSION,
    "Generated on " + time.ctime(time.time()),
    "Target assembler is lwasm (http://www.lwtools.ca/)",
    "",
]

#  Code start and user stack location in RAM
#  [TJL] Choose 0x1000 in HD6309SBC
ORIGIN = 0x1000

#  Start of variable space, grows downwards
#  Choose 0xB800 in HD6309SBC to avoid MON 1V4
VARTOP = 0xB800

# fmt: off
# -----------------------------------------------------------------------
#  RESERVED is dictionary of variables required by the SPL internals and
#    library functions. These are stored near ORIGIN and grow upwards.
#    Add to these as needed, but keep in mind they are always present,
#    regarless of which library functions are called.
# -----------------------------------------------------------------------

RESERVED = {
    "op1":      4,      #  1st operand (4 bytes)
    "op2":      4,      #  2nd operand (4 bytes)
    "res":      8,      #  result (8 bytes)
    "rem":      4,      #  remainder (4 bytes)
    "tmp":      4,      #  scratch (4 bytes)
    "sign":     1,      #  sign (1 byte)
    "dreg":     2,      #  D (A,B) register (2 bytes)
    "wreg":     2,      #  W (E,F) register (2 bytes)
    "xreg":     2,      #  X register (2 bytes)
    "yreg":     2,      #  Y register (2 bytes)
    "ureg":     2,      #  U register (2 bytes)
    "outbuf":   16,     #  output text buffer (16 bytes)
    "inbuf":    80,     #  input text buffer
}
#fmt: on


#fmt: off
# -----------------------------------------------------------------------
#  LIBRARYMAP is dictionary to map between SPL 'keyword' and the
#    corresponding 6x09 assembly routine / library function filename
#    (which must always be the same...)
# -----------------------------------------------------------------------
#
# Note that some primitives have been migrated to CORE WORDS
#   rather than including them as library files. This is the
#   case for the very short and/or often used primitives.
#   They are sorted into the following groups:
#
#   CoreStack:        drop, 2drop, dup, 2dup, nip, over, rot, swap, 2swap
#   CoreBitwise:      b.and, b.or, b.xor, ~
#   CoreArithmetic:   +, -, 1+, 2+, 1-, 2-, *, /, mod, negate
#   CoreAccess:       !, c!, d!, @, c@, d@, +!, ++, c++, --, c--
#   CoreCall:         >xreg, xreg>, >yreg, yreg>, >dreg, dreg>, >areg, areg>, >breg, breg>, >ureg, ureg>

LIBRARYMAP = {
    "abs"       : "abs",            # ( a -- |a| ) Return the absolute value
    "accept"    : "accept",         # ( addr -- len ) Get text line up to 255 chars, place at addr and return length
    "&."        : "ampdot",         # &. ampdot  ( d addr -- ) convert 32bit number d into ASCII at addr
    "bclr"      : "bclr",           # ( n b -- n' ) Clear bit b of n
    "bset"      : "bset",           # ( n b -- n' ) Set bit b of n
    "btest"     : "btest",          # ( n b -- 0|1 ) Test if bit b of n is set (1) or clear (0)
    "&&"        : "booland",        # ( a b -- n ) return boolean AND of a and b
    "||"        : "boolor",         # ( a b -- n ) return boolean OR of a and b
    ">outbuf"   : "tooutbuf",       # ( n -- ) converts text representation of n into outbuf
    "bye"       : "bye",            # ( -- ) jump through RESET vector at $FFFE to warmstart
    "call"      : "call",           # ( addr -- ) jump to subroutine at addr - useful with CODE BLOCKS  ep as library subroutine
    "cmove"     : "cmove",          # ( a b n -- ) move n bytes from a to b
    "cmove>"    : "cmoveb",         # ( a b n -- ) move n bytes backwards from a to b
    "count"     : "count",          # ( c-addr1 -- caddr2 u ) return string spec for the counted string at c-addr1.
                                    #       c-addr2 is address of first char after c-addr1. u is the content of the character
                                    #       at caddr1, which is the length in character of the string in c-addr2.
    "cr"        : "cr",             # ( -- ) output a carriage return
    "dabs"      : "dabs",           # ( a -- |a| ) Return the absolute value (32b)
    "date"      : "date",           # ( -- dd mm yy ) push date elements onto stack
    "d+"        : "dadd",           # ( a b -- d ) Return b+a (32b)
    "d/"        : "ddivide",
    "d/mod"     : "ddivmod",
    "depth"     : "depth",          # ( -- d ) push depth of stack onto stack
    "d="        : "deq",            # ( a b -- a==b ) Return true if a == b  (32b)
    "d>="       : "dge",            # ( a b -- a>=b ) Return true if a >= b  (32b)
    "d>"        : "dgt",            # ( a b -- a>b ) Return true if a > b  (32b)
    "disp"      : "disp",
    "d<="       : "dle",            # ( a b -- a>=b ) Return true if a >= b  (32b)
    "d<"        : "dlt",            # ( a b -- a>b ) Return true if a > b  (32b)
    "dmod"      : "dmod",
    "d*"        : "dmult",
    "dnegate"   : "dnegate",        # ( d -- -d ) Return negated value (32b)
    "d<>"       : "dne",            # ( a b -- a!=b ) Return true if a != b  (32b)
    "dnumber"   : "dnumber",
    "d.$"       : "dprhex",
    "d."        : "dprint",
    "dsqrt"     : "dsqrt",
    "d-"        : "dsub",           # ( a b -- d ) Return b-a (32b)
    "du."       : "duprint",
    "emit"      : "emit",           # ( c -- )  output (word) c at TOS as a character
    "end@"      : "fetchend",       #  end@ ( -- addr ) place end of program address onto stack
    "exit"      : "exit",           # ( -- ) compiles an RTS (return from subroutine) leaves the current function
    "erase"     : "erase",          # ( adr len -- ) fill memory with 0's
    "="         : "eq",             # ( a b -- a==b ) Return true if a == b
    "execute"   : "execute",        # ( addr -- )  call routine via address on top of stack
    "fill"      : "fill",           # ( adr len val -- )  fill memory with a value
    ">="        : "ge",             # ( a b -- a>=b ) Return true if a >= b
    ">"         : "gt",             # ( a b -- a>b ) Return true if a > b
    "input"     : "input_s",        # ( -- len ) get a line of text into the input buffer
    "keyp"      : "keyp",           # ( -- bool ) return true if input character is available
    "key"       : "key",            # ( -- d ) return input character (word)
    "/"         : "div",
    "<="        : "le",             # ( a b -- a<=b, signed comparison)
    "<"         : "lt",             # ( a b -- a<b, signed comparison)
    "<>"        : "ne",             # ( a b -- a!=b ) Return true if a != b
    "not"       : "not",            # ( d -- !d ) LOGICAL not of TOS
    "number"    : "number",         # (addr -- n) convert an ascii string in memory to a signed 16-bit number
    "pad"       : "pad",            # ( -- addr ) push output buffer address onto stack
    ".2$"       : "prhex2",         # ( d -- ) output TOS as a 2 digit hex
    ".$"        : "prhex",          # ( d -- ) output TOS as a 4 digit hex
    "."         : "print",          # ( d -- ) signed print decimal
    "quit"      : "quit",           # ( -- ) exit to monitor
    "read_block": "read_block",
    "reset"     : "reset",          # ( xx -- ) re-initialize the user stack
    ">r"        : "to_r",
    "r>"        : "from_r",
    "r@"        : "r_fetch",
    "space"     : "space",          # ( -- ) output an ASCII space
    "sp@"       : "spfetch",
    "strcmp"    : "strcmp",         # ( a b -- -1|0|+1 ) compare two strings (lt/eq/gt)
    "strcpy"    : "strcpy",         # ( src dest -- ) copy string from src to dest
    "strlen"    : "strlen",         # ( addr -- len ) length of string onto TOS
    "strmatch"  : "strmatch",       # ( str pattern -- 0|1 ) determine is string matches pattern
    "strpos"    : "strpos",         # ( addr c -- -1|idx ) return index into string for first occurrence of c or -1 if not found
    "time"      : "time",           # ( -- hh mm ) return the hour and minute
    "type"      : "type",           # ( adr len -- ) print string at addr with len
    "uncount"   : "uncount",
    "u/"        : "udiv",
    "u>"        : "ugt",            # ( a b -- a>b uns. ) Return true if a > b
    "u<"        : "ult",            # ( a b -- a<b uns. ) Return true if a < b
    "umod"      : "umod",
    "u*"        : "umult",
    "u."        : "uprint",         # ( d -- ) unsigned print decimal
    "<<"        : "ushiftl",
    ">>"        : "ushiftr",
    "><"        : "swapcell",
    "within"    : "within",         # ( u ul uh - f ) - true if ul <= u <= uh
    "write_block": "write_block",
}
# fmt: on

# -----------------------------------------------------------------------
#  SPL Compiler Globals and Symbols
# -----------------------------------------------------------------------
RADIX_HEX = 16
RADIX_DECIMAL = 10
RADIX_OCTAL = 8
RADIX_BINARY = 2
RADIX_FLOAT = 1
RADIX_UNSPECIFIED = 0

SIZE_FLOAT = 4
SIZE_DOUBLE = 4
SIZE_WORD = 2
SIZE_BYTE = 1
SIZE_UNSPECIFIED = 0

SIGN_POSITIVE = 1
SIGN_ONES_COMPLEMENT = -1
SIGN_TWOS_COMPLEMENT = -2
SIGN_UNSPECIFIED = 0

MAX_DOUBLE = (2**31) - 1
MAX_DOUBLE_UNSIGNED = (2**32) - 1
MAX_WORD = (2**15) - 1
MAX_WORD_UNSIGNED = (2**16) - 1
MAX_BYTE = (2**7) - 1
MAX_BYTE_UNSIGNED = (2**8) - 1
MIN_UNSIGNED = 0
MIN_BYTE = -(2**7)
MIN_WORD = -(2**15)
MIN_DOUBLE = -(2**31)

# -----------------------------------------------------------------------
#  Named Tuple Declarations`
# -----------------------------------------------------------------------

# Literal namedtuple()
LiteralValue = namedtuple(
    "LiteralValue", ["sign", "mag", "size", "radix", "bound", "text"]
)
# sign:  +1 = specified as positive
#         0 = <no sign specified>
#        -1 = one's complement
#        -2 = two's complement
#
# mag:   magnitude of value (not including sign)
# size:  number of bytes of storage required; SIZE_BYTE, SIZE_WORD, SIZE_DOUBLE, SIZE_FLOAT
# radix: radix of literal; RADIX_FLOAT, RADIX_BINARY, RADIX_OCTAL, RADIX_DEC, RADIX_HEX
# bound: True if literal value fits within bounds of specified storage
# text:  String of the value (after numeric whitespace (_) removed)


# -----------------------------------------------------------------------
#  Compiler source code follows.
# -----------------------------------------------------------------------


######################################################
#  SPLCompiler
#
class SPLCompiler:
    """Implements a compiler from SPL to HD6309 assembly code."""

    # -------------------------------------------------
    #  LabelName
    #
    def LabelName(self, name):
        """Return a unique label name."""

        self.counter += 1
        return f"{name}_{self.counter:05}"

    # -------------------------------------------------
    #  StringToStorageSize
    #
    def StringToStorageSize(self, s):
        """Interpret storage specification and return
        number of bytes required (0,1,2,4)."""

        t = s.lower()
        x = re.search("(b'|d'|w'|f')", t)
        size = SIZE_UNSPECIFIED
        if x:
            if x.group() == "d'":
                size = SIZE_DOUBLE  # four bytes
            elif x.group() == "b'":
                size = SIZE_BYTE  # one byte
            elif x.group() == "w'":
                size = SIZE_WORD  # two bytes
            elif x.group() == "f'":
                size = SIZE_FLOAT  # four bytes
        return size

    # -------------------------------------------------
    #  StringToUnaryOperator
    #
    def StringToUnaryOperator(self, s):
        """Interpret unary operators and return
        -1 for 1's complement (~)
        -2 for 2's complement (-)
         0 for no sign specified ()
        +1 for positive (+)"""

        x = re.search("^(-|~|\+)?", s)
        sign = SIGN_UNSPECIFIED
        if x:
            if x.group() == "-":
                sign = SIGN_TWOS_COMPLEMENT  # two's complement unary minus (-)
            elif x.group() == "~":
                sign = SIGN_ONES_COMPLEMENT  # one's bitwise complement (~)
            elif x.group() == "+":
                sign = SIGN_POSITIVE  # positive
        return sign

    # -------------------------------------------------
    #  StringToFloat
    #
    def StringToFloat(self, s):
        """Interpret s as an IEEE-754 single precision float"""

        msg = "Illegal floating point number: "
        t = s.lower()

        # try to convert the IEEE-754 single precision float to a unsigned 32b int
        try:
            u = struct.unpack("<I", struct.pack("<f", eval(t)))[0]
        except:
            self.Error(msg + s)
        else:
            return LiteralValue(
                sign=SIGN_POSITIVE,
                mag=u,
                size=SIZE_FLOAT,
                radix=RADIX_FLOAT,
                bound=True,
                text=s,
            )

    # -------------------------------------------------
    #  StringToDecimal
    #
    def StringToDecimal(self, s):
        """Interpret s as a decimal number."""

        radix = RADIX_DECIMAL
        msg = "Illegal decimal number: "
        t = s.lower()

        # detect any leading unary operators
        sign = self.StringToUnaryOperator(t)

        # detect and interpret a storage specifier
        size = self.StringToStorageSize(t)

        # strip "numeric whitespace" (underscore) characters
        t = t.replace("_", "")

        # isolate the constant and convert to int
        u = re.search("[0-9]+[0-9]*$", t)
        if u:
            mag = int(u.group(), radix)
            bound = self.LiteralBoundCheck(sign, mag, size)
            return LiteralValue(
                sign=sign, mag=mag, size=size, radix=radix, bound=bound, text=s
            )
        else:
            self.Error(msg + s)

    # -------------------------------------------------
    #  StringToBinary
    #
    def StringToBinary(self, s):
        """Interpret s as a binary number."""

        radix = RADIX_BINARY
        msg = "Illegal binary number: "
        t = s.lower()

        # detect any leading unary operators
        sign = self.StringToUnaryOperator(t)

        # detect and interpret a storage specifier
        size = self.StringToStorageSize(t)

        # strip "numeric whitespace" (underscore) characters
        t = t.replace("_", "")

        # isolate the constant and convert to int
        u = re.search("0b[01]+$", t)
        if u:
            mag = int(u.group(), radix)
            bound = self.LiteralBoundCheck(sign, mag, size)
            return LiteralValue(
                sign=sign, mag=mag, size=size, radix=radix, bound=bound, text=s
            )
        else:
            self.Error(msg + s)

    # -------------------------------------------------
    #  StringToOctal
    #
    def StringToOctal(self, s):
        """Interpret s as an octal number."""

        radix = RADIX_OCTAL
        msg = "Illegal octal number: "
        t = s.lower()

        # detect any leading unary operators
        sign = self.StringToUnaryOperator(t)

        # detect and interpret a storage specifier
        size = self.StringToStorageSize(t)

        # strip "numeric whitespace" (underscore) characters
        t = t.replace("_", "")

        # isolate the constant and convert to int
        u = re.search("0[0-7]*$", t)
        if u:
            mag = int(u.group(), radix)
            bound = self.LiteralBoundCheck(sign, mag, size)
            return LiteralValue(
                sign=sign, mag=mag, size=size, radix=radix, bound=bound, text=s
            )
        else:
            self.Error(msg + s)

    # -------------------------------------------------
    #  StringToHexadecimal
    #
    def StringToHexadecimal(self, s):
        """Interpret s as a hexadecimal number."""

        radix = RADIX_HEX
        msg = "Illegal hexadecimal number: "
        t = s.lower()

        # detect any leading unary operators
        sign = self.StringToUnaryOperator(t)

        # detect and interpret a storage specifier
        size = self.StringToStorageSize(t)

        # strip "numeric whitespace" (underscore) characters
        t = t.replace("_", "")

        # isolate the constant and convert to int
        u = re.search("0x[0-9a-f]+$", t)

        if u:
            mag = int(u.group(), radix)
            bound = self.LiteralBoundCheck(sign, mag, size)
            return LiteralValue(
                sign=sign, mag=mag, size=size, radix=radix, bound=bound, text=s
            )
        else:
            self.Error(msg + s)

    # -------------------------------------------------
    #  RemoveStringFormatting
    #
    def RemoveStringFormatting(self, t):
        """Remove newlines and tabs from a string,
        replacing them with printable \n and \t"""

        s = t.replace("\t", "\\t")
        return "\\n".join(s.splitlines())

    # -------------------------------------------------
    #  LiteralBoundCheck
    #
    def LiteralBoundCheck(self, sign, mag, size):
        """Check if literal magnitue and sign are
        compatible with storage size specified"""

        if size == SIZE_BYTE:
            if sign in [SIGN_ONES_COMPLEMENT, SIGN_TWOS_COMPLEMENT]:
                return (mag <= MAX_BYTE) and (mag >= MIN_BYTE)
            else:
                return (mag <= MAX_BYTE_UNSIGNED) and (mag >= MIN_UNSIGNED)
        elif size == SIZE_WORD:
            if sign in [SIGN_ONES_COMPLEMENT, SIGN_TWOS_COMPLEMENT]:
                return (mag <= MAX_WORD) and (mag >= MIN_WORD)
            else:
                return (mag <= MAX_WORD_UNSIGNED) and (mag >= MIN_UNSIGNED)
        elif size == SIZE_DOUBLE:
            if sign in [SIGN_ONES_COMPLEMENT, SIGN_TWOS_COMPLEMENT]:
                return (mag <= MAX_DOUBLE) and (mag >= MIN_DOUBLE)
            else:
                return (mag <= MAX_DOUBLE_UNSIGNED) and (mag >= MIN_UNSIGNED)
        else:
            return True

    # -------------------------------------------------
    #  StringToLiteralValue
    #
    def StringToLiteralValue(self, s):
        """Return s as a LiteralValue named tuple."""
        n = ()
        if self.isNumber(s):
            if self.isDecimal(s):
                n = self.StringToDecimal(s)
                self.Debug(3, f"dec literal {n}")
            elif self.isHexadecimal(s):
                n = self.StringToHexadecimal(s)
                self.Debug(3, f"hex literal {n}")
            elif self.isBinary(s):
                n = self.StringToBinary(s)
                self.Debug(3, f"bin literal {n}")
            elif self.isOctal(s):
                n = self.StringToOctal(s)
                self.Debug(3, f"oct literal {n}")
            elif self.isFloat(s):
                n = self.StringToFloat(s)
                self.Debug(3, f"float literal {n}")
            else:
                self.Error(f"Invalid literal {s}")
        else:
            self.Error(f"Invalid literal {s}")

        return n

    # -------------------------------------------------
    #  isFloat
    #
    def isFloat(self, s):
        """True if string s is a valid floating point number."""

        t = s.lower()
        return re.fullmatch("^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$", t) != None

    # -------------------------------------------------
    #  isDecimal
    #
    def isDecimal(self, s):
        """True if string s is a valid decimal number.
        This also checks for the case of '0' to
        avoid it being interpreted as an octal"""

        t = s.lower()
        return re.fullmatch("^(-|~)?(b'|d'|w')?[1-9_]+[0-9_]*$", t) or re.fullmatch(
            "^(b'|d'|w')?0+$", t
        )

    # -------------------------------------------------
    #  isOctal
    #
    def isOctal(self, s):
        """True if string s is a valid octal number."""

        t = s.lower()
        return re.fullmatch("^(-|~)?(b'|d'|w')?0[0-7_]*$", t) != None

    # -------------------------------------------------
    #  isHexadecimal
    #
    def isHexadecimal(self, s):
        """True if string s is a valid hex number."""

        t = s.lower()
        return re.fullmatch("^(-|~)?(b'|d'|w')?0x[0-9a-f_]+$", t) != None

    # -------------------------------------------------
    #  isBinary
    #
    def isBinary(self, s):
        """True if string s is a valid binary number."""

        t = s.lower()
        return re.fullmatch("^(-|~)?(b'|d'|w')?0b[01_]+$", t) != None

    # -------------------------------------------------
    #  isNumber
    #
    def isNumber(self, s):
        """Return true if string s is a valid number."""

        #  It must be a string
        if type(s) != str:
            return False
        elif s == "":
            return False
        elif self.isDecimal(s):
            self.Debug(3, "isNum: decimal " + s)
            return True
        elif self.isHexadecimal(s):
            self.Debug(3, "isNum: hex " + s)
            return True
        elif self.isBinary(s):
            self.Debug(3, "isNum: bin " + s)
            return True
        elif self.isOctal(s):
            self.Debug(3, "isNum: oct " + s)
            return True
        elif self.isFloat(s):
            self.Debug(3, "isNum: float " + s)
            return True
        else:
            return False

    # -------------------------------------------------
    #  StringToScalar
    #
    def StringToScalar(self, t):
        """Parse a 'simple' number in s, if possible.
        Only decimal and hex are supported. Hex may be prefied with
        either '0x' or '$'. Neither unary operators nor storage
        specifications are allowed."""

        msg = "Illegal scalar: "

        # check for a simple decimal number (no storage, no unary ops)
        u = re.search("^[0-9]*$", t)
        if u:
            n = int(u.group(), RADIX_DECIMAL)
            self.Debug(3, f"simple dec {n}")
            return n

        # check for a simple 0x... hex number (no storage, no unary ops)
        u = re.search(r"^0x[0-9a-fA-F]+$", t)
        if u:
            n = int(u.group(), RADIX_HEX)
            self.Debug(3, f"simple 0x hex {n}")
            return n

        # check for a simple $... hex number (no storage, no unary ops)
        u = re.search(r"^\$[0-9a-fA-F]+$", t)
        if u:
            n = int(u.group()[1:], RADIX_HEX)
            self.Debug(3, f"simple $ hex {n}")
            return n

        self.Error(msg + t)

    # -------------------------------------------------
    #  InvalidName
    #
    def InvalidName(self, name):
        """Returns true if the given name is not valid."""

        if type(name) != str:
            return True
        if name == "":
            return True
        t = name.lower()
        if re.fullmatch("^[a-z][a-z0-9_]*$", t):
            return False
        else:
            return True

    # -------------------------------------------------
    #  Error
    #
    def Error(self, msg):
        """Output a value error message and quit."""

        raise ValueError(f"In {self.funcName} : {self.Token} : {msg}")

    # -------------------------------------------------
    #  Debug
    #
    def Debug(self, level, msg):
        """Output a debug message if self.debug is true."""

        if self.debug_level >= level:
            print(f"Debug [{level}] {self.funcName} : {msg}")

    # -------------------------------------------------
    #  ParseCommandLine
    #
    def ParseCommandLine(self):
        """Parse the command line arguments."""

        # Build and invoke argparse command line argument parser
        parser = argparse.ArgumentParser(
            prog="spl",
            description=f"An SPL compiler for the MC6809 and HD6309 version {VERSION} {LAST_MOD}",
            fromfile_prefix_chars="@",
        )
        parser.add_argument(
            "file", help="input filename(s) (.spl extension assumed)", nargs="+"
        )
        parser.add_argument(
            "--outfile",
            "-o",
            help="base filename for output file (no extension, default is 'out')",
            default=BASE_NAME,
        )
        parser.add_argument(
            "--format",
            "-f",
            help="output format (lwasm 'format' types)",
            choices={"asm", "srec", "ihex", "decb", "raw"},
            default=BASE_TYPE,
            dest="fmt",
        )
        parser.add_argument(
            "--org",
            "-g",
            help=f"compiled program ORIGIN (default is ${ORIGIN:04X})",
            type=SPLCompiler.StringToScalar,
            default=ORIGIN,
        )
        parser.add_argument(
            "--vartop",
            help=f"compiled program VARIBLE TOP (grows downward, default is ${VARTOP:04X})",
            type=SPLCompiler.StringToScalar,
            default=VARTOP,
        )
        parser.add_argument(
            "--sstack",
            help="initial address of CPU SYSTEM STACK (default is to leave it alone)",
            type=SPLCompiler.StringToScalar,
            default=None,
        )
        parser.add_argument(
            "--ustack",
            help="initial address of CPU USER STACK (grows downward, default is ORIGIN)",
            type=SPLCompiler.StringToScalar,
            default=ORIGIN,
        )
        parser.add_argument(
            "--comment",
            "-c",
            help="add comments to assembly source",
            action="store_true",
        )
        parser.add_argument(
            "--debug",
            "-d",
            help="enable compiler debug display",
            action="count",
            default=0,
        )
        parser.add_argument(
            "--trace", help="enable Python traceback info", action="store_true"
        )
        parser.add_argument(
            "--version", action="version", version="%(prog)s " + VERSION
        )
        args = parser.parse_args()

        # copy command line flags to class variables
        self.verbose = args.comment
        self.quiet = False
        self.debug_level = args.debug

        # Disable traceback info upon error unless enabled via --trace
        if args.trace == False:
            sys.tracebacklimit = 0

        self.Debug(
            1,
            f"trace={args.trace}, comment={self.verbose}, quiet={self.quiet}, debug_level={self.debug_level}",
        )

        # copy command line file list to class list
        self.files = args.file

        # build output filename based upon --outfile and --format options (or defaults)
        self.outtype = args.fmt
        self.Debug(1, f"output format = {self.outtype}")
        extensionsByFormat = {
            "asm": "s",
            "srec": "s19",
            "ihex": "hex",
            "decb": "dcb",
            "raw": "raw",
        }
        self.outname = args.outfile
        self.outext = extensionsByFormat[self.outtype]
        self.Debug(1, f"output filename = {self.outname}.{self.outext}")

        # set program origin based upon --org (defaults to ORIGIN)
        self.cmdOrigin = args.org
        self.Debug(1, f"program origin = {self.cmdOrigin}")

        # set program variable top address if --vartop is specified (defaults to VARTOP)
        self.varTop = args.vartop
        self.Debug(1, f"program vartop = {self.varTop}")

        # set top of system stack if --sstack is specified (defaults to "None"!)
        self.sysStackTop = args.sstack
        self.Debug(1, f"system stack = {self.sysStackTop}")

        # set top of user stack if --ustack specified (defaults to ORIGIN)
        self.userStackTop = args.ustack
        self.Debug(1, f"user stack = {self.userStackTop}")

    # -------------------------------------------------
    #  LoadFiles
    #
    def LoadFiles(self):
        """Load the source code on the command line."""

        self.source = ""

        for fname in self.files:
            #  If no .spl extension, add it
            if fname.find(".spl") == -1:
                fname += ".spl"

            try:
                #  Open the file as given on the command line
                f = open(fname, "r")
            except:
                #  Is it in the include directory?
                try:
                    f = open(INCLUDE_DIR + fname, "r")
                except:
                    raise OSError("Unable to locate %s" % fname)

            #  Append to the source string
            self.source += f.read() + "\n"
            f.close()

    # -------------------------------------------------
    #  Tokenize
    #
    def Tokenize(self):
        """Tokenize the input source code."""
        if self.source == "":
            return

        self.tokens = []
        inToken = inString = inComment = inCode = False
        delim = ""
        t = ""

        for c in self.source:
            # when inside a string constant, check for delimeter or copy as-is to token
            if inString:
                if c == delim:
                    t += c
                    self.tokens.append(t)
                    t = ""
                    inString = False
                    delim = ""
                else:
                    t += c

            # when inside a comment, check for delimeter but ignore the character
            elif inComment:
                if c == delim:
                    if delim == "*":  # match * of */ block comment delimiter
                        delim = "/"
                    elif delim == "/":  # match / of */ block comment delimiter
                        t = ""
                        inComment = False
                        delim = ""
                    elif delim == ")":  # match ) of block comment delimiter
                        t = ""
                        inComment = False
                        delim = ""
                    elif delim == "\n":  # match /n of line comments
                        t = ""
                        inComment = False
                        delim = ""
                else:  # if character did not match delimiter,
                    if delim == "/":  # and delimiter was / (of */) then
                        delim = "*"  # start looking for another *

            # when inside a code block, check for delimeter while adding to the token
            elif inCode:
                if t == "" and c == "\n":
                    continue
                elif c == delim:
                    if delim == "#":
                        t += c
                        delim = "/"
                    elif delim == "/":
                        t += c
                        self.tokens.append(t)
                        t = ""
                        inCode = False
                        delim = ""
                else:
                    delim = "#"  # no delimiter match, so keep looking for #
                    t += c  # accumulate char into token

            elif inToken:
                # check for /# pattern (start of code block)
                if t == "/" and c == "#":
                    t += c
                    inCode = True
                    inToken = False
                    delim = "#"

                # check for /* pattern (start of block comment)
                elif t == "/" and c == "*":
                    t = ""
                    inComment = True
                    inToken = False
                    delim = "*"

                # check for ( pattern (start of block comment)
                elif c == "(":
                    t = ""
                    inComment = True
                    inToken = False
                    delim = ")"

                # check for whitespace (end of token)
                elif c <= " ":
                    inToken = False
                    self.tokens.append(t)
                    t = ""

                # otherwise, accumulate characters into token
                else:
                    t += c

            else:
                # beginning of a string token?
                if c == '"' or c == "'":
                    inString = True
                    delim = c
                    t = c

                # check for # beginning of single-line comment
                elif c == "#":
                    inComment = True
                    delim = "\n"
                    t = ""

                # check for ( pattern (start of block comment)
                elif c == "(":
                    t = ""
                    inComment = True
                    delim = ")"

                # skip whitespace in between tokens
                elif c <= " ":
                    pass  # ignore whitespace

                # otherwise start a new token
                else:
                    t += c
                    inToken = True

            # debug tokenizer if enabled
            self.Debug(5, f"c:{c}, delim:{delim}, token:{t:<20}")
            self.Debug(
                5, f"Tok:{inToken}, Str:{inString}, Com:{inComment}, Code:{inCode}"
            )

        # if debug is True, display the tokens collected and the sum
        self.Debug(1, f"{len(self.tokens)} tokens found")

        # if debug is >3 then display all tokens collected
        if self.debug_level > 3:
            # pythonic multi-column display of collected tokens
            num_token_columns = 3
            current_column = 1
            self.Debug(4, "Tokens in order:")
            for count, item in enumerate(self.tokens, 1):
                s = self.RemoveStringFormatting(item)
                if len(s) > 19:  # magic-number: token field width - 1
                    s = s[:16] + "..."  # magic number: token field width - 4
                print(
                    f"{count:>4} :: {s:<20}", end=""
                )  # magic number: token field width
                if current_column >= num_token_columns:
                    current_column = 1
                    print()
                else:
                    current_column += 1
            if current_column > 1:
                print()

    # -------------------------------------------------
    #  AddName
    #
    def AddName(self, name):
        """Add a new name to the names dictionary."""

        if name in self.names:
            self.Error("Duplicate name found: " + str(name))
        #        self.names[name] = self.LabelName(name)
        self.names[name] = name

    # -------------------------------------------------
    #  Declarations
    #
    def Declarations(self):
        """Locate all defined variables, constants and strings."""

        #  Dictionaries of all defined variables, and numeric and string constants
        #  accessed by name as key.
        self.variables = {}
        self.consts = {}
        self.strings = {}
        indef = False
        i = 0

        while i < len(self.tokens):
            # Process VARIABLE declaration
            if self.tokens[i] == "var":
                if indef:
                    self.Error("Variables cannot be defined within functions.")
                if (i + 2) >= len(self.tokens):
                    self.Error("Syntax error: too few tokens for variable declaration.")
                name = self.tokens[i + 1]
                # simple Scalar for number of bytes associated with a variable
                if self.isNumber(self.tokens[i + 2]):
                    numBytes = self.StringToScalar(self.tokens[i + 2])
                else:
                    numBytes = self.StringToStorageSize(self.tokens[i + 2])
                i += 2
                if self.InvalidName(name):
                    self.Error("Illegal variable name: {name}")
                if (numBytes <= 0) or (numBytes >= 65535):
                    self.Error(f"Illegal variable size: {name}, size = {numBytes}")
                self.variables[name] = numBytes
                self.symtbl[name] = "VAR"
                self.AddName(name)
                self.Debug(2, f"variable: {name}, size = {numBytes}")
            # Process CONSTANT declaration
            elif self.tokens[i] == "const":
                if indef:
                    self.Error("Constants cannot be defined within functions.")
                if (i + 2) >= len(self.tokens):
                    self.Error("Syntax error: too few tokens for constant declaration.")
                name = self.tokens[i + 1]
                # process constant values as a literal to see if it's valid
                n = self.StringToLiteralValue(self.tokens[i + 2])
                i += 2
                if self.InvalidName(name):
                    self.Error(f"Illegal constant name: {name}")
                if n and n.bound:
                    self.consts[name] = n.text  # just save the text as the token
                    self.symtbl[name] = "CONST"
                    self.AddName(name)
                else:
                    self.Error(f"Illegal constant value: {name} {n}")
                self.Debug(2, f"constant: {name}, value {n}")
            # Process STRING declaration
            elif self.tokens[i] == "str":
                if indef:
                    self.Error("Strings cannot be defined within functions.")
                if (i + 2) >= len(self.tokens):
                    self.Error("Syntax error: too few tokens for string declaration.")
                name = self.tokens[i + 1]
                val = self.tokens[i + 2]
                i += 2
                if self.InvalidName(name):
                    self.Error(f"Illegal string name: {name}")
                if val == "":
                    self.Error(f"Illegal string value for {name}")
                self.strings[name] = val
                self.symtbl[name] = "STR"
                self.AddName(name)
                self.Debug(2, f"string: {name}, value {val}")
            elif self.tokens[i] == "def":
                indef = True
            elif self.tokens[i] == ":":
                indef = True
            elif self.tokens[i] == "end":
                indef = False
            elif self.tokens[i] == ";":
                indef = False

            i += 1

    # -------------------------------------------------
    #  ParseDataBlocks
    #
    def ParseDataBlocks(self):
        """Get all data blocks."""

        #  Dictionary of all data blocks.  Each entry stores the data values
        #  for that block.
        self.dataBlocks = {}

        indata = False
        i = 0
        name = ""
        data = []

        while i < len(self.tokens):
            if self.tokens[i] == "data":
                if indata:
                    self.Error("Data blocks may not be nested.")
                indata = True
                data = []
                if (i + 1) >= len(self.tokens):
                    self.Error("Too few tokens for data block declaration.")
                name = self.tokens[i + 1]
                if self.InvalidName(name):
                    self.Error("Illegal data block name: " + str(name))
                i += 2
            elif (self.tokens[i] == "end") and (indata):
                indata = False
                self.dataBlocks[name] = data
                self.symtbl[name] = "DATA"
                self.AddName(name)
                i += 1
            elif indata:
                # process data values as literals to see if they are valid
                #                if not isNumber(self.tokens[i]):
                #                    self.Error("Illegal data value: " + name + " [" + n.text + "]")

                n = self.StringToLiteralValue(self.tokens[i])
                if n:
                    if n.bound:
                        data.append(n.text)  # just save the text into the data array
                    else:
                        self.Error(
                            "Illegal data value in block " + name + " [" + n.text + "]"
                        )
                else:
                    self.Error(
                        "Illegal data value in block "
                        + name
                        + " ["
                        + self.tokens[i]
                        + "]"
                    )
                i += 1
            else:
                i += 1

    # -------------------------------------------------
    #  ParseCodeBlocks
    #
    def ParseCodeBlocks(self):
        """Get a code block."""

        #  Dictionary of all code blocks.  Each entry stores the code values
        #  for that block.
        self.codeBlocks = {}

        i = 0
        name = ""
        statement = ""
        code = []

        while i < len(self.tokens):
            if self.tokens[i] == "code":
                if (i + 2) >= len(self.tokens):
                    self.Error("Too few tokens for code block declaration.")

                name = self.tokens[i + 1]
                delimited = self.tokens[i + 2]
                inline = delimited[2:-2]
                if self.InvalidName(name):
                    self.Error(f"Illegal code block name: {name}")
                if not self.isInlineCode(delimited):
                    self.Error(f"Code block {name} is missing delimiters.")
                self.Debug(2, f"code {name} " + self.RemoveStringFormatting(inline))
                # the entire code block is contained in inline
                # so add it to the code block list, the name to the symbol table as "CODE"
                self.codeBlocks[name] = inline
                self.symtbl[name] = "CODE"
                self.AddName(name)
                i += 3
            else:
                i += 1

    # -------------------------------------------------
    #  ParseFunctions
    #
    def ParseFunctions(self):
        """Parse all functions."""

        #  Dictionary of all functions.  Each entry stores the tokens associated with
        #  that function, accessible by function name as key.
        self.funcs = {}

        indef = False
        i = 0
        name = ""
        func = []

        while i < len(self.tokens):
            if (self.tokens[i] == "def") or (self.tokens[i] == ":"):
                if indef:
                    self.Error("Function declarations may not be nested.")
                indef = True
                func = []
                if (i + 1) >= len(self.tokens):
                    self.Error("Too few tokens for function declaration.")
                name = self.tokens[i + 1]
                if self.InvalidName(name):
                    self.Error("Illegal function name: " + str(name))
                i += 2
            elif ((self.tokens[i] == "end") or (self.tokens[i] == ";")) and (indef):
                indef = False
                self.funcs[name] = func
                self.symtbl[name] = "FUNC"
                self.referenced[name] = False
                self.AddName(name)
                i += 1
            elif indef:
                func.append(self.tokens[i])
                i += 1
            else:
                i += 1

    # -------------------------------------------------
    #  TagFunctions
    #
    def TagFunctions(self):
        """Mark all functions that are referenced."""

        for f in self.funcs:
            for token in self.funcs[f]:
                if token in self.symtbl:
                    if self.symtbl[token] == "FUNC":
                        self.referenced[token] = True

    # -------------------------------------------------
    #  isStringConstant
    #
    def isStringConstant(self, token):
        """Return true if this token is a string constant."""

        return (token[0] == token[-1]) and ((token[0] == '"') or (token[0] == "'"))

    # -------------------------------------------------
    #  StringConstantName
    #
    def StringConstantName(self):
        """Generate a unique name for this string constant."""

        name = f"string_{self.stringCount:05}"
        self.stringCount += 1
        return name

    # -------------------------------------------------
    #  LocateStringConstants
    #
    def LocateStringConstants(self):
        """Locate string constants inside of functions and replace them with
        references to STR constants."""

        #  Always reference "main"
        self.referenced["main"] = True

        #  Check all tokens of all defined functions
        for key in self.funcs:
            if self.referenced[key]:
                i = 0
                while i < len(self.funcs[key]):
                    token = self.funcs[key][i]
                    if self.isStringConstant(token):  #  if it is a string constant
                        name = self.StringConstantName()  #  generate a name for it
                        self.strings[
                            name
                        ] = token  #  and add it to the string constant dictionary
                        self.symtbl[name] = "STR"  #  plus the symbol table
                        self.funcs[key][
                            i
                        ] = name  #  and change the token to the generated name
                        self.AddName(name)  #  add the new name
                    i += 1

    # -------------------------------------------------
    #  isInlineCode
    #
    def isInlineCode(self, token):
        """Return true if this token is inline ASM code."""
        return token[:2] == "/#" and token[-2:] == "#/"

    # -------------------------------------------------
    #  LoadLibraryDependencies
    #
    #  Populate library words dependency table from lib/depends.json.
    #  Any library functions that depend on additional, separate source files
    #  should have an entry in depends.json.
    #
    #  format of lib/depends.json:
    #  {
    #  "entertainers" : ["bob", "doug"],
    #  "bob" : ["hope", "mackenzie"],
    #  "doug" : ["mackenzie"],
    #  "hope" : [],
    #  "mackenzie" : []
    #  }
    def LoadLibraryDependencies(self):
        try:
            with open(LIB_DIR + "depends.json", "r") as depfile:
                self.library_dependencies = json.load(depfile)
                if type(self.library_dependencies) == dict:
                    self.Debug(1, LIB_DIR + "depends.json found and parsed")
        except:
            raise OSError("bad or missing " + LIB_DIR + "depends.json")

    # -------------------------------------------------
    #  Dependencies
    #
    def Dependencies(self, names):
        """Add all dependencies for the given list of dependencies."""

        for d in names:  #  for all dependencies
            if d not in self.dependencies:  #  if not in global list
                self.dependencies.append(d)  #  add it
                self.Debug(1, f"dependencies : added {d}")
                if d in self.library_dependencies:
                    self.Dependencies(
                        self.library_dependencies[d]
                    )  #  and all of its dependencies, too

    # -------------------------------------------------
    #  LibraryRoutines
    #
    def LibraryRoutines(self):
        """Locate all library routines used, and any dependencies they might have."""

        #  List of all library routines used
        self.lib = []
        libfilenames = []

        #  Check all tokens of all defined functions
        for key in self.funcs:
            for token in self.funcs[key]:
                if token in LIBRARYMAP:  #  if it is a library routine
                    if token not in self.lib:  #  and hasn't been added yet
                        self.lib.append(token)  #  add it to the list
                        libfilenames.append(
                            LIBRARYMAP[token] + ".s"
                        )  # add library filename to debug list
                        self.symtbl[token] = "LIB"  #  and the symbol table

        #  Now add all the dependencies
        depends = []
        for routine in self.lib:  #  for every identified library routine
            name = LIBRARYMAP[routine]  #  get the library file name
            self.Debug(1, f"library : token {routine}, file {name}.s")
            if name in self.library_dependencies:
                for d in self.library_dependencies[name]:  #  check its dependencies
                    if d not in depends:  #  if not already added
                        depends.append(d)  #  add it to the list

        #  Now add the dependencies of the dependencies
        self.Dependencies(depends)
        self.Debug(1, f"library: final file list: {libfilenames} {self.dependencies}")

    # -------------------------------------------------
    #  FormatByte
    #
    def FormatByte(self, num, radix):
        """Return a string representation of a byte formatted for LWASM
        in the various radix formats"""

        n = num & 0x00FF

        if radix == RADIX_HEX:
            return f"${n:2X}"

        elif radix == RADIX_OCTAL:
            return f"@{n:3o}"

        elif radix == RADIX_BINARY:
            return f"%{n:8b}"

        else:
            return f"{n}"

    # -------------------------------------------------
    #  FormatWord
    #
    def FormatWord(self, num, radix):
        """Return a string representation of a word formatted for LWASM
        in the various radix formats"""

        n = num & 0xFFFF

        if radix == RADIX_HEX:
            return f"${n:4X}"

        elif radix == RADIX_OCTAL:
            return f"@{n:5o}"

        elif radix == RADIX_BINARY:
            return f"%{n:16b}"

        else:
            return f"{n}"

    # -------------------------------------------------
    #  FormatDouble
    #
    def FormatDouble(self, num, radix):
        """Return a string representation of a doubleword formatted for LWASM
        in the various radix formats."""

        n = num & 0xFFFFFFFF

        if radix == RADIX_HEX:
            return f"${n:8X}"

        elif radix == RADIX_OCTAL:
            return f"@{n:11o}"

        elif radix == RADIX_BINARY:
            return f"%{n:32b}"

        else:
            return f"{n}"

    # -------------------------------------------------
    #  CompileNumber
    #
    def CompileNumber(self, token):
        """Compile a number by pushing it onto the User stack."""

        n = self.StringToLiteralValue(token)
        self.Debug(2, f"CompileNumber {n}")

        # bounds-check
        if n.bound == False:
            self.Error("Value out of bounds : " + token)

        # double-word number reference (includes Float literals)
        elif n.size == 4:
            # double-word
            if n.sign == -1:
                # 1's complemented double-word
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push doubleword {n.text} onto stack (1C, LSW first)",
                            "",
                            "",
                        ]
                    )
                self.fasm.append(["", "LDD", "#" + self.FormatWord(~n.mag, n.radix)])
                self.fasm.append(["", "PSHU", "D"])
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((~n.mag >> 16), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
            elif n.sign == -2:
                # 2's complemented double-word
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push doubleword {n.text} onto stack (2C, LSW first)",
                            "",
                            "",
                        ]
                    )
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((0 - n.mag), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord(((0 - n.mag) >> 16), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
            else:
                # double-word
                if self.verbose:
                    # comment for Float literal includes FP value
                    if n.radix == RADIX_FLOAT:
                        self.fasm.append(
                            [
                                f";; Push float {n.text} ({hex(n.mag)}) onto stack",
                                "",
                                "",
                            ]
                        )
                    else:
                        self.fasm.append(
                            [
                                f";; Push doubleword {n.text} onto stack (LSW first)",
                                "",
                                "",
                            ]
                        )
                # Use the same code generation for Float literal and double-word hex.
                # Change temporary radix from 1 (float) to RADIX_HEX (hex) to accomplish this
                if n.radix == RADIX_FLOAT:
                    radix = RADIX_HEX
                else:
                    radix = n.radix
                # lwasm code to push a double-word onto user stack
                self.fasm.append(["", "LDD", "#" + self.FormatWord(n.mag, radix)])
                self.fasm.append(["", "PSHU", "D"])
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((n.mag >> 16), radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
        elif n.size == 2 or n.size == 0:
            if n.sign == -1:
                # lwasm code to push a 1's complemented word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push word {n.text} onto stack (1C)",
                        ]
                    )
                self.fasm.append(["", "LDD", "#" + self.FormatWord(~n.mag, n.radix)])
                self.fasm.append(["", "PSHU", "D"])
            elif n.sign == -2:
                # lwasm code to push a 2's complemented word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push word {n.text} onto stack (2C)",
                        ]
                    )
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((0 - n.mag), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
            else:
                # lwasm code to push a word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push word {n.text} onto stack",
                        ]
                    )
                self.fasm.append(["", "LDD", "#" + self.FormatWord(n.mag, n.radix)])
                self.fasm.append(["", "PSHU", "D"])
        elif n.size == 1:
            if n.sign == -1:
                # lwasm code to push a 1's complemented byte as a word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push byte {n.text} onto stack as word (1C)",
                        ]
                    )
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord(~(n.mag & 0x00FF), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
            elif n.sign == -2:
                # lwasm code to push a 2's complemented byte as a word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push byte {n.text} onto stack as word (2C)",
                        ]
                    )
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((0 - (n.mag & 0x00FF)), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])
            else:
                # lwasm code to push a byte as a word onto user stack
                if self.verbose:
                    self.fasm.append(
                        [
                            f";; Push byte {n.text} onto stack as word",
                        ]
                    )
                self.fasm.append(
                    ["", "LDD", "#" + self.FormatWord((n.mag & 0x00FF), n.radix)]
                )
                self.fasm.append(["", "PSHU", "D"])

    # -------------------------------------------------
    #  CompileVariableRef
    #
    def CompileVariableRef(self, token):
        """Compile a variable ref by putting its address on the stack."""

        name = self.names[token]
        self.Debug(2, f"CompileVariableRef {name}")
        if self.verbose:
            self.fasm.append(
                [
                    f";; Push address of variable '{token}' onto stack",
                ]
            )
        self.fasm.append(["", "LDY", "#" + name])
        self.fasm.append(["", "PSHU", "Y"])

    # -------------------------------------------------
    #  CompileDataBlockRef
    #
    def CompileDataBlockRef(self, token):
        """Compile a data block ref by putting its address on the stack."""

        name = self.names[token]
        self.Debug(2, f"CompileDatablockRef {name}")
        if self.verbose:
            self.fasm.append(
                [
                    f";; Push address of block '{token}' onto stack",
                ]
            )
        self.fasm.append(["", "LDY", "#" + name])
        self.fasm.append(["", "PSHU", "Y"])

    # -------------------------------------------------
    #  CompileCodeBlockRef
    #
    def CompileCodeBlockRef(self, token):
        """Compile a code block ref by jsr to its address."""

        name = self.names[token]
        self.Debug(2, f"CompileCodeBlockRef {name}")
        if self.verbose:
            self.fasm.append(
                [
                    f";; Call code block '{token}'",
                ]
            )
        self.fasm.append(["", "JSR", name])

    # -------------------------------------------------
    #  CompileStringConstantRef
    #
    def CompileStringConstantRef(self, token):
        """Compile a reference to a string constant by putting its
        address on the stack."""

        name = self.names[token]
        self.Debug(2, f"CompileStringConstantRef {name}")
        if self.verbose:
            self.fasm.append(
                [
                    f";; Push address of string '{token}' onto stack",
                ]
            )
        self.fasm.append(["", "LDY", "#" + name])
        self.fasm.append(["", "PSHU", "Y"])

    # -------------------------------------------------
    #  CompileLoopBegin
    #
    def CompileLoopBegin(self):
        """Compile the start of a loop."""

        t = self.LabelName("loop")
        self.Debug(2, f"CompileLoopBegin {t}")
        self.fasm.append([t, " "])
        self.loop.append([t, self.LabelName("loop2")])

    # -------------------------------------------------
    #  CompileLoopEnd
    #
    def CompileLoopEnd(self):
        """Compile the end of a loop."""

        if self.loop == []:
            self.Error("Loop underflow!")
        t = self.loop[-1]
        self.loop = self.loop[0:-1]
        self.Debug(2, f"CompileLoopEnd {t[0]}")
        self.fasm.append(["", "JMP", t[0]])
        self.fasm.append(
            [
                t[1],
            ]
        )

    # -------------------------------------------------
    #  CompileIf
    #
    def CompileIf(self):
        """Compile an if statement."""

        t_else = self.LabelName("else")
        t_then = self.LabelName("then")
        t = self.LabelName("t")
        t_end = self.LabelName("tend")
        self.Debug(2, "CompileIf")
        self.compare.append([t_else, t_then, False, t_end])
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BEQ", t])
        self.fasm.append(["", "BNE", t_then])
        self.fasm.append([t, "JMP", t_else])
        self.fasm.append(
            [
                t_then,
            ]
        )

    # -------------------------------------------------
    #  CompileNotIf
    #
    def CompileNotIf(self):
        """Compile a not if statement."""

        t_else = self.LabelName("else")
        t_then = self.LabelName("then")
        t = self.LabelName("t")
        t_end = self.LabelName("tend")
        self.Debug(2, "CompileNotIf")
        self.compare.append([t_else, t_then, False, t_end])
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BNE", t])
        self.fasm.append(["", "BEQ", t_then])
        self.fasm.append([t, "JMP", t_else])
        self.fasm.append(
            [
                t_then,
            ]
        )

    # -------------------------------------------------
    #  CompileElse
    #
    def CompileElse(self):
        """Compile an else statement."""

        self.compare[-1][2] = True
        t_else, t_then, flag, t_end = self.compare[-1]
        self.Debug(2, "CompileElse")
        self.fasm.append(["", "JMP", t_end])
        self.fasm.append(
            [
                t_else,
            ]
        )

    # -------------------------------------------------
    #  CompileThen
    #
    def CompileThen(self):
        """Compile a then statement."""

        if self.compare == []:
            self.Error("Compare underflow!")
        t_else, t_then, flag, t_end = self.compare[-1]
        self.Debug(2, "CompileThen")
        self.compare = self.compare[0:-1]
        if not flag:
            self.fasm.append(
                [
                    t_else,
                ]
            )
        else:
            self.fasm.append(
                [
                    t_end,
                ]
            )

    # -------------------------------------------------
    #  CompileBreak
    #
    def CompileBreak(self):
        """Comple a break statement."""

        t = self.loop[-1]
        self.Debug(2, "CompileBreak")
        self.fasm.append(["", "JMP", t[1]])

    # -------------------------------------------------
    #  CompileCont
    #
    def CompileCont(self):
        """Compile a continue statement."""

        t = self.loop[-1]
        self.Debug(2, "CompileContinue")
        self.fasam.append(["", "JMP", t[0]])

    # -------------------------------------------------
    #  CompileIfBreak
    #
    def CompileIfBreak(self):
        """Compile a conditional break."""

        t = self.loop[-1]
        q = self.LabelName("break")
        self.Debug(2, "CompileConditionalBreak")
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BEQ", q])
        self.fasm.append(["", "JMP", t[1]])
        self.fasm.append(
            [
                q,
            ]
        )

    # -------------------------------------------------
    #  CompileIfCont
    #
    def CompileIfCont(self):
        """Compile a conditional continue."""

        t = self.loop[-1]
        q = self.LabelName("ifcont")
        self.Debug(2, "CompileConditionalContinue")
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BEQ", q])
        self.fasm.append(["", "JMP", t[0]])
        self.fasm.append(
            [
                q,
            ]
        )

    # -------------------------------------------------
    #  CompileNotIfBreak
    #
    def CompileNotIfBreak(self):
        """Compile a negated conditional break."""

        t = self.loop[-1]
        q = self.LabelName("notifbreak")
        self.Debug(2, "CompileNotIfBreak")
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BNE", q])
        self.fasm.append(["", "JMP", t[1]])
        self.fasm.append(
            [
                q,
            ]
        )

    # -------------------------------------------------
    #  CompileNotIfCont
    #
    def CompileNotIfCont(self):
        """Compile a negated conditional continue."""

        t = self.loop[-1]
        q = self.LabelName("notifcont")
        self.Debug(2, "CompileNotIfCont")
        self.fasm.append(["", "PULU", "D"])
        self.fasm.append(["", "TSTD", ""])
        self.fasm.append(["", "BNE", q])
        self.fasm.append(["", "JMP", t[0]])
        self.fasm.append(
            [
                q,
            ]
        )

    # -------------------------------------------------
    #  CompileLibraryRef
    #
    def CompileLibraryRef(self, token):
        """Compile a reference to a library word."""
        self.Debug(2, f"CompileLibraryRef {token}")
        self.fasm.append(["", "JSR", LIBRARYMAP[token]])

    # -------------------------------------------------
    #  CompileReturn
    #
    def CompileReturn(self):
        """Compile a return statement."""

        #  Return from subroutine
        self.Debug(2, f"CompileReturn")
        self.fasm.append(["", "RTS", ""])

    # -------------------------------------------------
    #  CompileCoreStack
    #
    def CompileCoreStack(self, token):
        """Compile a core stack manipulation function as inline code."""
        """ drop, 2drop, dup, 2dup, nip, over, rot, swap, 2swap """

        if token == "drop":
            self.fasm.append(
                [
                    ";; DROP : ( a -- )",
                ]
            )
            self.fasm.append(["", "LEAU", "2,U"])
            # self.fasm.append([";; end DROP","",""])

        elif token == "2drop":
            self.fasm.append(
                [
                    ";; 2DROP : ( a b -- )",
                ]
            )
            self.fasm.append(["", "LEAU", "4,U"])
            # self.fasm.append([";; end 2DROP","",""])

        elif token == "dup":
            self.fasm.append(
                [
                    ";; DUP : ( a -- a a )",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end DUP","",""])

        elif token == "2dup":
            self.fasm.append(
                [
                    ";; 2DUP : ( a b -- a b a b )",
                ]
            )
            self.fasm.append(["", "LDD", "2,U"])
            self.fasm.append(["", "PSHU", "D"])
            self.fasm.append(["", "LDD", "2,U"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end 2DUP","",""])

        elif token == "nip":
            self.fasm.append(
                [
                    ";; NIP : ( a b -- b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end NIP","",""])

        elif token == "over":
            self.fasm.append(
                [
                    ";; OVER : ( a b -- a b a )",
                ]
            )
            self.fasm.append(["", "LDD", "2,U"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end OVER","",""])

        elif token == "rot":
            self.fasm.append(
                [
                    ";; ROT : ( a b c -- b c a )",
                ]
            )
            self.fasm.append(["", "PULU", "D,Y"])
            self.fasm.append(["", "PULUW", ""])
            self.fasm.append(["", "PSHU", "D,Y"])
            self.fasm.append(["", "PSHUW", ""])
            # self.fasm.append([";; end ROT","",""])

        elif token == "swap":
            self.fasm.append(
                [
                    ";; SWAP : ( a b -- b a )",
                ]
            )
            self.fasm.append(["", "PULU", "D,Y"])
            self.fasm.append(["", "EXG", "D,Y"])
            self.fasm.append(["", "PSHU", "D,Y"])
            # self.fasm.append([";; end SWAP","",""])

        elif token == "2swap":
            self.fasm.append(
                [
                    ";; 2SWAP : ( a b c d -- c d a b",
                ]
            )
            self.fasm.append(["", "LDQ", "0,U"])  # get 32b from tos
            self.fasm.append(["", "PSHS", "D"])  # save to CPU stack
            self.fasm.append(["", "PSHSW", ""])
            self.fasm.append(["", "LDQ", "4,U"])  # get 32b from next-to-tos
            self.fasm.append(["", "STQ", "0,U"])  # copy to tos
            self.fasm.append(["", "PULSW", ""])  # retrieve old tos from CPU stack
            self.fasm.append(["", "PULS", "D"])
            self.fasm.append(["", "STQ", "4,U"])  # store 32b to next-to-tos
            # self.fasm.append([";; end 2SWAP","",""])

        else:
            self.Error("Unimplemented CORE STACK Function")

    # -------------------------------------------------
    #  CompileCoreBitwise
    #
    def CompileCoreBitwise(self, token):
        """Compile a core bitwise function as inline code."""
        """ b.and, b.or, b.xor, ~ (complement) """

        if token == "b.and":
            self.fasm.append(
                [
                    ";; B.AND : ( a b -- a AND b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "ANDD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end B.AND","",""])

        elif token == "b.or":
            self.fasm.append(
                [
                    ";; B.OR : ( a b -- a OR b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "ORD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end B.OR","",""])

        elif token == "b.xor":
            self.fasm.append(
                [
                    ";; B.XOR : ( a b -- a XOR b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "EORD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end B.XOR","",""])

        elif token == "~":
            self.fasm.append(
                [
                    ";; COMP (~) : ( a -- NOT a)",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "COMD", ""])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end COMP","",""])

        else:
            self.Error("Unimplemented CORE BITWISE Function")

    # -------------------------------------------------
    #  CompileCoreArithmetic
    #
    def CompileCoreArithmetic(self, token):
        """Compile a core arithmetic function as inline code."""
        """ +, -, 1+, 2+, 1-, 2- *, / """

        if token == "+":
            self.fasm.append(
                [
                    ";; ADD (+) : ( a b -- a+b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "ADDD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end ADD","",""])

        elif token == "-":
            self.fasm.append(
                [
                    ";; SUB (-) : ( a b -- a-b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "SUBD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end SUB","",""])

        elif token == "1+":
            self.fasm.append(
                [
                    ";; PLUS (+1) : ( a -- a+1 )",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "ADDD", "#1"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end PLUS1","",""])

        elif token == "2+":
            self.fasm.append(
                [
                    ";; PLUS2 (+2) : ( a -- a+2 )",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "ADDD", "#2"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end PLUS2","",""])

        elif token == "1-":
            self.fasm.append(
                [
                    ";; MINUS1 (-1) : ( a -- a-1 )",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "SUBD", "#1"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end MINUS1","",""])

        elif token == "2-":
            self.fasm.append(
                [
                    ";; MINUS2 (-2) : ( a -- a-2 )",
                ]
            )
            self.fasm.append(["", "LDD", "0,U"])
            self.fasm.append(["", "SUBD", "#2"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end MINUS2","",""])

        elif token == "*":
            self.fasm.append(
                [
                    ";; MULT16 (*) : ( a b -- a*b )",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "MULD", "0,U"])
            self.fasm.append(["", "STW", "0,U"])
            # self.fasm.append([";; end MULT16","",""])

        elif token == "/":
            self.fasm.append(
                [
                    ";; DIV16 (*) intrinsic",
                ]
            )
            self.fasm.append(["", "PULUW", ""])
            self.fasm.append(["", "CLRD", ""])
            self.fasm.append(["", "DIVQ", "0,U"])
            self.fasm.append(["", "STW", "2,U"])
            # self.fasm.append([";; end DIV16","",""])

        elif token == "mod":
            self.fasm.append(
                [
                    ";; MOD16 (*) : ( a b -- a%b )",
                ]
            )
            self.fasm.append(["", "PULUW", ""])
            self.fasm.append(["", "CLRD", ""])
            self.fasm.append(["", "DIVQ", "0,U"])
            self.fasm.append(["", "STD", "2,U"])
            # self.fasm.append([";; end MOD16","",""])

        elif token == "negate":
            self.fasm.append(
                [
                    ";; NEGATE : ( a -- -a)",
                ]
            )
            self.fasm.append(["", "LDD", "#0"])
            self.fasm.append(["", "SUBD", "0,U"])
            self.fasm.append(["", "STD", "0,U"])
            # self.fasm.append([";; end NEGATE","",""])

        else:
            self.Error("Unimplemented CORE ARITHMETIC Function")

    # -------------------------------------------------
    #  CompileCoreCall
    #
    def CompileCoreCall(self, token):
        """Compile a core system call function as inline code."""
        """ >xreg, xreg>, >yreg, yreg>, >dreg, dreg>, >areg, areg>, >breg, breg>, >ureg, ureg> """
        if token == ">xreg":
            self.fasm.append(
                [
                    ";; TO_XREG (>x) : ( a -- ) pop TOS into XREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "xreg"])
            # self.fasm.append([";; end TO_XREG","",""])

        elif token == "xreg>":
            self.fasm.append(
                [
                    ";; FROM_XREG (x>) : ( -- a ) place XREG storage onto TOS",
                ]
            )
            self.fasm.append(["", "LDD", "xreg"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_XREG","",""])

        elif token == ">yreg":
            self.fasm.append(
                [
                    ";; TO_YREG (>y) : ( a -- ) pop TOS into YREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "yreg"])
            # self.fasm.append([";; end TO_YREG","",""])

        elif token == "yreg>":
            self.fasm.append(
                [
                    ";; FROM_YREG (y>) : ( -- a ) place YREG storage onto TOS",
                ]
            )
            self.fasm.append(["", "LDD", "yreg"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_YREG","",""])

        elif token == ">dreg":
            self.fasm.append(
                [
                    ";; TO_DREG (>d) : ( a -- ) pop TOS into DREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "dreg"])
            # self.fasm.append([";; end TO_DREG","",""])

        elif token == "dreg>":
            self.fasm.append(
                [
                    ";; FROM_DREG (d>) : ( -- a ) place DREG storage onto TOS",
                ]
            )
            self.fasm.append(["", "LDD", "dreg"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_DREG","",""])

        elif token == ">areg":
            self.fasm.append(
                [
                    ";; TO_AREG (>a) : ( a -- ) pop TOS LSB into DREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STB", "dreg"])
            # self.fasm.append([";; end TO_AREG","",""])

        elif token == "areg>":
            self.fasm.append(
                [
                    ";; FROM_AREG (a>) : ( -- a ) place DREG storage LSB onto TOS",
                ]
            )
            self.fasm.append(["", "LDB", "dreg"])
            self.fasm.append(["", "CLRA", ""])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_AREG","",""])

        elif token == ">breg":
            self.fasm.append(
                [
                    ";; TO_BREG (>b) : ( a -- ) pop TOS LSB into DREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STB", "dreg+1"])
            # self.fasm.append([";; end TO_AREG","",""])

        elif token == "breg>":
            self.fasm.append(
                [
                    ";; FROM_BREG (b>) : ( -- a ) place DREG storage MSB onto TOS",
                ]
            )
            self.fasm.append(["", "LDD", "dreg"])
            self.fasm.append(["", "CLRA", ""])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_AREG","",""])

        elif token == ">ureg":
            self.fasm.append(
                [
                    ";; TO_UREG (>y) : ( a -- ) pop TOS into UREG storage",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "ureg"])
            # self.fasm.append([";; end TO_UREG","",""])

        elif token == "ureg>":
            self.fasm.append(
                [
                    ";; FROM_UREG (y>) : ( -- a ) place DREG storage onto TOS",
                ]
            )
            self.fasm.append(["", "LDD", "ureg"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FROM_UREG","",""])

        else:
            self.Error("Unimplemented CORE CALL Function")

    # -------------------------------------------------
    #  CompileCoreAccess
    #
    def CompileCoreAccess(self, token):
        """Compile a core arithmetic function as inline code."""
        """ !, c!, @, c@, ++, c++, --, c-- """
        if token == "!":
            self.fasm.append(
                [
                    ";; STORE (!) : ( a b -- ) store word a at addr b",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "0,Y"])
            # self.fasm.append([";; end STORE","",""])

        elif token == "c!":
            self.fasm.append(
                [
                    ";; CSTORE (c!) : ( a b -- ) store byte a at addr b",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STB", "0,Y"])
            # self.fasm.append([";; end CSTORE","",""])

        elif token == "d!":
            self.fasm.append(
                [
                    ";; DSTORE (d!) ( a b c -- ) store dword a:b at addr c",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "0,Y"])
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "STD", "2,Y"])
            # self.fasm.append([";; end DSTORE","",""])

        elif token == "@":
            self.fasm.append(
                [
                    ";; FETCH (@) : ( a -- b ) read word b from addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "LDD", "0,Y"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end FETCH","",""])

        elif token == "c@":
            self.fasm.append(
                [
                    ";; CFETCH (c@) : ( a -- b ) read word b from byte at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "LDB", "0,Y"])
            self.fasm.append(["", "CLRA", ""])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end CFETCH","",""])

        elif token == "d@":
            self.fasm.append(
                [
                    ";; DFETCH (d@) : ( a -- b c ) read dword b:c from addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "LDD", "2,Y"])
            self.fasm.append(["", "PSHU", "D"])
            self.fasm.append(["", "LDD", "0,Y"])
            self.fasm.append(["", "PSHU", "D"])
            # self.fasm.append([";; end DFETCH","",""])

        elif token == "+!":
            self.fasm.append(
                [
                    ";; ADDSTORE (+!) : ( a b -- ) add word b to word at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "D"])
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "ADDD", "0,Y"])
            self.fasm.append(["", "STD", "0,Y"])
            # self.fasm.append([";; end ADDSTORE","",""])

        elif token == "++":
            self.fasm.append(
                [
                    ";; INCADDR (++) : ( a -- ) increment word at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "LDD", "0,Y"])
            self.fasm.append(["", "INCD", ""])
            self.fasm.append(["", "STD", "0,Y"])
            # self.fasm.append([";; end INCADDR","",""])

        elif token == "c++":
            self.fasm.append(
                [
                    ";; INCCADDR (c++) : ( a -- ) increment byte at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "INC", "0,Y"])
            # self.fasm.append([";; end INCCADDR","",""])

        elif token == "--":
            self.fasm.append(
                [
                    ";; DECADDR (--) : ( a -- ) decrement word at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "LDD", "0,Y"])
            self.fasm.append(["", "DECD", ""])
            self.fasm.append(["", "STD", "0,Y"])
            # self.fasm.append([";; end DECADDR","",""])

        elif token == "c--":
            self.fasm.append(
                [
                    ";; DECCADDR (c--) : ( a -- ) decrement byte at addr a",
                ]
            )
            self.fasm.append(["", "PULU", "Y"])
            self.fasm.append(["", "DEC", "0,Y"])
            # self.fasm.append([";; end DECCADDR","",""])

        else:
            self.Error("Unimplemented CORE ACCESS Function")

    # -------------------------------------------------
    #  FunctionAddress
    #
    def FunctionAddress(self):
        """Mark the next function reference to push the address
        on the stack."""

        self.functionAddress = True

    # -------------------------------------------------
    #  Keywords
    #
    def Keywords(self):
        """Place all keywords in the symbol table and
        create the compile helper function dictionary."""

        self.symtbl["{"] = "KWD"
        self.symtbl["}"] = "KWD"
        self.symtbl["if"] = "KWD"
        self.symtbl["0if"] = "KWD"
        self.symtbl["else"] = "KWD"
        self.symtbl["then"] = "KWD"
        self.symtbl["break"] = "KWD"
        self.symtbl["cont"] = "KWD"
        self.symtbl["?break"] = "KWD"
        self.symtbl["?cont"] = "KWD"
        self.symtbl["?0break"] = "KWD"
        self.symtbl["?0cont"] = "KWD"
        self.symtbl["&"] = "KWD"
        self.symtbl["return"] = "KWD"

        #  Compile helper dictionary
        self.keywords = {
            "{": self.CompileLoopBegin,
            "}": self.CompileLoopEnd,
            "if": self.CompileIf,
            "0if": self.CompileNotIf,
            "else": self.CompileElse,
            "then": self.CompileThen,
            "break": self.CompileBreak,
            "cont": self.CompileCont,
            "?break": self.CompileIfBreak,
            "?cont": self.CompileIfCont,
            "?0break": self.CompileNotIfBreak,
            "?0cont": self.CompileNotIfCont,
            "return": self.CompileReturn,
            "&": self.FunctionAddress,
        }

    # -------------------------------------------------
    #  Core (Intrinsic) Words
    #
    def Corewords(self):
        """Place the core words that are practical to
        include as inline code in the symbol table and
        create the compile core function dictionary."""

        # Core stack manipulation functions
        self.symtbl["drop"] = "CORE"
        self.symtbl["2drop"] = "CORE"
        self.symtbl["dup"] = "CORE"
        self.symtbl["2dup"] = "CORE"
        self.symtbl["nip"] = "CORE"
        self.symtbl["over"] = "CORE"
        self.symtbl["rot"] = "CORE"
        self.symtbl["swap"] = "CORE"
        self.symtbl["2swap"] = "CORE"

        # Core bitwise functions
        self.symtbl["b.and"] = "CORE"
        self.symtbl["b.or"] = "CORE"
        self.symtbl["b.xor"] = "CORE"
        self.symtbl["~"] = "CORE"
        self.symtbl["~"] = "CORE"

        # Core arithmetic functions
        self.symtbl["+"] = "CORE"
        self.symtbl["-"] = "CORE"
        self.symtbl["1+"] = "CORE"
        self.symtbl["2+"] = "CORE"
        self.symtbl["1-"] = "CORE"
        self.symtbl["2-"] = "CORE"
        self.symtbl["*"] = "CORE"
        self.symtbl["/"] = "CORE"
        self.symtbl["mod"] = "CORE"
        self.symtbl["negate"] = "CORE"

        # Core access functions
        self.symtbl["!"] = "CORE"
        self.symtbl["c!"] = "CORE"
        self.symtbl["d!"] = "CORE"
        self.symtbl["@"] = "CORE"
        self.symtbl["c@"] = "CORE"
        self.symtbl["d@"] = "CORE"
        self.symtbl["+!"] = "CORE"
        self.symtbl["+1"] = "CORE"
        self.symtbl["+!"] = "CORE"
        self.symtbl["c++"] = "CORE"
        self.symtbl["--"] = "CORE"
        self.symtbl["c--"] = "CORE"

        # Core call functions
        self.symtbl[">xreg"] = "CORE"
        self.symtbl["xreg>"] = "CORE"
        self.symtbl[">yreg"] = "CORE"
        self.symtbl["yreg>"] = "CORE"
        self.symtbl[">dreg"] = "CORE"
        self.symtbl["dreg>"] = "CORE"
        self.symtbl[">areg"] = "CORE"
        self.symtbl["areg>"] = "CORE"
        self.symtbl[">breg"] = "CORE"
        self.symtbl["breg>"] = "CORE"
        self.symtbl[">ureg"] = "CORE"
        self.symtbl["ureg>"] = "CORE"

        #  Compile core (intrinsic) dictionary
        self.corewords = {
            "drop": self.CompileCoreStack,
            "2drop": self.CompileCoreStack,
            "dup": self.CompileCoreStack,
            "2dup": self.CompileCoreStack,
            "nip": self.CompileCoreStack,
            "over": self.CompileCoreStack,
            "rot": self.CompileCoreStack,
            "swap": self.CompileCoreStack,
            "2swap": self.CompileCoreStack,
            "b.and": self.CompileCoreBitwise,
            "b.or": self.CompileCoreBitwise,
            "b.xor": self.CompileCoreBitwise,
            "~": self.CompileCoreBitwise,
            "+": self.CompileCoreArithmetic,
            "-": self.CompileCoreArithmetic,
            "1+": self.CompileCoreArithmetic,
            "2+": self.CompileCoreArithmetic,
            "1-": self.CompileCoreArithmetic,
            "2-": self.CompileCoreArithmetic,
            "*": self.CompileCoreArithmetic,
            "/": self.CompileCoreArithmetic,
            "mod": self.CompileCoreArithmetic,
            "negate": self.CompileCoreArithmetic,
            "!": self.CompileCoreAccess,
            "c!": self.CompileCoreAccess,
            "d!": self.CompileCoreAccess,
            "@": self.CompileCoreAccess,
            "c@": self.CompileCoreAccess,
            "d@": self.CompileCoreAccess,
            "+!": self.CompileCoreAccess,
            "++": self.CompileCoreAccess,
            "c++": self.CompileCoreAccess,
            "--": self.CompileCoreAccess,
            "c--": self.CompileCoreAccess,
            ">xreg": self.CompileCoreCall,
            "xreg>": self.CompileCoreCall,
            ">yreg": self.CompileCoreCall,
            "yreg>": self.CompileCoreCall,
            ">dreg": self.CompileCoreCall,
            "dreg>": self.CompileCoreCall,
            ">areg": self.CompileCoreCall,
            "areg>": self.CompileCoreCall,
            ">breg": self.CompileCoreCall,
            "breg>": self.CompileCoreCall,
            ">ureg": self.CompileCoreCall,
            "ureg>": self.CompileCoreCall,
        }

    # -------------------------------------------------
    #  CompileDataBlocks
    #
    def CompileDataBlocks(self):
        """Create assembly instructions for all data blocks."""

        #  Holds the assembly code for the data blocks
        self.dasm = []

        #  Compile each block
        for f in self.dataBlocks:
            name = self.names[f]
            self.Debug(2, f"CompileDataBlock {name}")
            self.dasm.append(
                [
                    name,
                    "",
                    "",
                    f";; Data block '{name}'",
                ]
            )
            for number in self.dataBlocks[f]:
                n = self.StringToLiteralValue(number)

                # bounds-check
                if n.bound == False:
                    self.Error(f"Value out of bounds : {n.text}")

                # double-word value
                elif n.size == 4:
                    # double-word
                    if n.sign == -1:
                        # 1's complemented double-word
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Doubleword {n.text} (1C, LSW first)",
                                ]
                            )
                        self.dasm.append(
                            ["", "FQB", self.FormatDouble(~n.mag, n.radix)]
                        )
                    elif n.sign == -2:
                        # 2's complemented double-word
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Doubleword {n.text} (2C, LSW first)",
                                ]
                            )
                        self.dasm.append(
                            ["", "FQB", self.FormatDouble(0 - n.mag, n.radix)]
                        )
                    else:
                        # double-word
                        if self.verbose:
                            if n.radix == 1:
                                self.dasm.append(
                                    [
                                        f";; Float {n.text} ({hex(n.mag)})",
                                    ]
                                )
                            else:
                                self.dasm.append(
                                    [
                                        f";; Doubleword {n.text} (LSW first)",
                                    ]
                                )
                        if n.radix == 1:
                            radix = RADIX_HEX
                        else:
                            radix = n.radix
                        self.dasm.append(["", "FQB", self.FormatDouble(n.mag, radix)])
                elif n.size == 2 or n.size == 0:
                    if n.sign == -1:
                        # 1's complemented word
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Word {n.text} (1C)",
                                ]
                            )
                        self.dasm.append(["", "FDB", self.FormatWord(~n.mag, n.radix)])
                    elif n.sign == -2:
                        # 2's complemented word
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Word {n.text} (2C)",
                                ]
                            )
                        self.dasm.append(
                            ["", "FDB", self.FormatWord(0 - n.mag, n.radix)]
                        )
                    else:
                        # word
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Word {n.text}",
                                ]
                            )
                        self.dasm.append(["", "FDB", self.FormatWord(n.mag, n.radix)])
                elif n.size == 1:
                    if n.sign == -1:
                        # 1's complemented byte
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Byte {n.text} (1C)",
                                ]
                            )
                        self.dasm.append(["", "FCB", self.FormatByte(~n.mag, n.radix)])
                    elif n.sign == -2:
                        # 2's complemented byte
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Byte {n.text} (2C)",
                                ]
                            )
                        self.dasm.append(
                            ["", "FCB", self.FormatByte((0 - n.mag), n.radix)]
                        )
                    else:
                        # byte
                        if self.verbose:
                            self.dasm.append(
                                [
                                    f";; Byte {n.text}",
                                ]
                            )
                        self.dasm.append(["", "FCB", self.FormatByte(n.mag, n.radix)])

    # -------------------------------------------------
    #  CompileCodeBlocks
    #
    def CompileCodeBlocks(self):
        """Create assembly instructions for all code blocks."""

        #  Holds the assembly code for the data blocks
        self.casm = []

        #  Compile each block
        for f in self.codeBlocks:
            name = self.names[f]
            code = self.codeBlocks[f].strip("\n ")
            self.casm.append(
                [name, "", "", f";; Code block '{name}'"]
            )  #  Code block label
            self.casm.append(
                [
                    code,
                ]
            )
            self.Debug(
                2, "CompileCodeBlock " + name + " " + self.RemoveStringFormatting(code)
            )

    # -------------------------------------------------
    #  CompileFunctions
    #
    def CompileFunctions(self):
        """Compile all defined functions."""

        #  Holds the assembly code for the functions
        self.fasm = []

        #  Compile each function
        for f in self.funcs:
            if self.referenced[f]:
                self.loop = []
                self.compare = []
                self.funcName = f  #  Currently compiling
                self.fasm.append(
                    [
                        self.names[f],
                    ]
                )  #  Subroutine label
                for token in self.funcs[f]:
                    self.Token = token  #  Current token
                    if token in self.symtbl:
                        if self.symtbl[token] == "FUNC":
                            if self.functionAddress:
                                #  Push the function address
                                self.fasm.append(["", "LDD", "#" + self.names[token]])
                                self.fasm.append(["", "PSHU", "D"])
                                self.functionAddress = False
                            else:
                                #  Compile a subroutine call
                                if self.verbose:
                                    self.fasm.append(
                                        [
                                            f";; Call to user function '{self.names[token]}'",
                                            "",
                                            "",
                                        ]
                                    )
                                self.fasm.append(["", "JSR", self.names[token]])
                        elif self.symtbl[token] == "VAR":
                            self.CompileVariableRef(
                                token
                            )  #  Compile a variable reference
                        elif self.symtbl[token] == "DATA":
                            self.CompileDataBlockRef(
                                token
                            )  #  Compile a reference to a data block
                        elif self.symtbl[token] == "CODE":
                            self.CompileCodeBlockRef(
                                token
                            )  #  Compile a JSR to a code block
                        elif self.symtbl[token] == "CONST":
                            if self.verbose:
                                self.fasm.append(
                                    [
                                        f";; Reference to constant '{token}'",
                                    ]
                                )
                            self.CompileNumber(
                                self.consts[token]
                            )  #  Compile a constant reference
                        elif self.symtbl[token] == "STR":
                            self.CompileStringConstantRef(
                                token
                            )  #  Compile a reference to a string constant
                        elif self.symtbl[token] == "KWD":
                            self.keywords[token]()  #  Compile a keyword
                        # add inline core functions as used
                        elif self.symtbl[token] == "CORE":
                            self.corewords[token](
                                token
                            )  #  Compile a core (intrinsic) function
                        #
                        elif self.symtbl[token] == "LIB":
                            if self.functionAddress:
                                if self.verbose:
                                    self.fasm.append(
                                        [
                                            f";; Push address of library function '{token}'",
                                            "",
                                            "",
                                        ]
                                    )
                                #  Push the address of the library routine
                                self.fasm.append(["", "LDD", "#" + token])
                                self.fasm.append(["", "PSHU", "D"])
                                self.functionAddress = False
                            else:
                                if self.verbose:
                                    self.fasm.append(
                                        [
                                            f";; Call library function '{token}'",
                                        ]
                                    )
                                #  Compile a library word call
                                self.CompileLibraryRef(
                                    token
                                )  #  Compile a library word reference
                        #
                        else:
                            self.Error(
                                f"Unknown symbol table type: {token} , type = "
                                + str(self.symtbl[token])
                                + f", function = {f}"
                            )
                    elif self.isNumber(token):
                        # LiteralValue push
                        self.CompileNumber(token)
                    # handle inline ASM code
                    elif self.isInlineCode(token):
                        self.fasm.append(
                            [
                                "; Inline ASM code",
                            ]
                        )
                        self.fasm.append(
                            [
                                token[2:-2].strip("\n "),
                            ]
                        )
                        # insert a blank line to force break in scope for local labels
                        self.fasm.append(
                            [
                                "",
                            ]
                        )
                    else:
                        self.Error(f"Unknown token: {token}, function = {f}")
                self.fasm.append(["", "RTS", ""])  #  Ending RTS instruction

    # -------------------------------------------------
    #  pp "Pretty Print"
    #
    def AsmSource(self, f, t):
        """Write an instruction to the assembly output file.
        taking an input list with multiple fields"""

        # print all the entries in the list t, separated by tabs
        [f.write(a.expandtabs(8) + "\t") for a in t]
        f.write("\n")

    # -------------------------------------------------
    #  GenerateAssembly
    #
    def GenerateAssembly(self):
        """Generate the output 6809/6309 assembly code."""

        #  Check for a main function
        if "main" not in self.symtbl:
            self.Error("No main function defined.")
        if self.symtbl["main"] != "FUNC":
            self.Error("No main function defined.")

        #  Open the output assembly source code file
        f = open(self.outname + ".s", "w")

        #  Write the header
        for s in HEADER:
            self.AsmSource(f, [";; " + s])

        #  Enable the lwasm 6809 "convenience instructions"
        #   ASRD, CLRD, COMD, LSLD, LSRD, NEGD, TSTD
        self.AsmSource(f, ["", "PRAGMA", "6809conv"])
        f.write("\n")

        #  Program Variables
        #       self.names[v] is variable name
        #       self.variables[v] is variable size (in bytes)
        #
        #  Find the sum of all of the variable sizes and issue an
        #    ORG so that the variable space ends at varTop

        varBottom = self.varTop - sum(self.variables.values())
        self.AsmSource(f, [f";; program variables start at ${varBottom:04X}"])
        self.AsmSource(f, ["", "ORG", f"${varBottom:04X}"])
        for v in self.variables:
            self.AsmSource(f, [self.names[v], "RMB", f"{self.variables[v]}"])
        self.AsmSource(f, [f";; variables end below ${self.varTop:04X}"])
        f.write("\n")

        # Locate system stack if specified on command line
        if self.sysStackTop:
            self.AsmSource(f, ["sstack", "EQU", f"${self.sysStackTop:04X}"])
            self.AsmSource(
                f,
                [
                    f";; system stack grows downward below ${self.sysStackTop:04X}",
                ],
            )
        else:
            self.AsmSource(
                f,
                [
                    ";; system stack location is unaltered",
                ],
            )

        # Locate user stack (defaults to  program origin)
        self.AsmSource(f, ["ustack", "EQU", f"${self.userStackTop:04X}"])
        self.AsmSource(
            f,
            [
                f";; user stack grows downward below ${self.userStackTop:04X}",
            ],
        )
        f.write("\n")

        # Set program origin
        self.AsmSource(f, ["", "ORG", f"${self.cmdOrigin:04X}"])
        self.AsmSource(f, ["_start", "EQU", "*"])
        f.write("\n")

        #  Main call
        if self.sysStackTop:
            self.AsmSource(
                f, ["", "LDS", "#sstack", "; initialize system stack pointer"]
            )
        self.AsmSource(f, ["", "LDU", "#ustack", "; initialize user stack pointer"])
        self.AsmSource(f, ["", "JMP", self.names["main"]])

        #  Internal Variables
        #        variable name, variable size (in bytes)
        for s in RESERVED:
            self.AsmSource(f, [s, "RMB", f"{RESERVED[s]:<6}"])
        f.write("\n")

        #  Data blocks
        if len(self.dasm) > 0:
            self.AsmSource(f, [";; Data blocks"])
            for s in self.dasm:
                self.AsmSource(f, s)
            f.write("\n")

        #  Code blocks
        if len(self.casm) > 0:
            self.AsmSource(f, [";; Code blocks"])
            for s in self.casm:
                self.AsmSource(f, s)
            f.write("\n")

        #  String constants
        for s in self.strings:
            if self.verbose:
                self.AsmSource(f, [";; String constant '" + self.names[s] + "'"])
            self.AsmSource(f, [self.names[s], "FCN", self.strings[s]])
        if len(self.strings) > 0:
            f.write("\n")

        #  Dependencies
        for s in self.dependencies:
            # g = open(LIB_DIR+s+".s", "r")
            # f.write(g.read())
            # g.close()
            self.Debug(1, "dependency source " + s)
        f.write("\n")

        #  Library routines
        for s in self.lib:
            g = open(LIB_DIR + LIBRARYMAP[s] + ".s", "r")
            f.write(g.read())
            g.close()
            self.Debug(1, "library source " + s)
        f.write("\n")

        #  Functions
        for s in self.fasm:
            self.AsmSource(f, s)
        f.write("\n")

        # end label
        self.AsmSource(f, ["_end", "EQU", "*"])
        f.close()

        # report how many lines of assembly source were written and to where
        if not self.quiet:
            with open(self.outname + ".s") as f:
                print(sum(1 for _ in f), f"lines of source written to {self.outname}.s")

    # -------------------------------------------------
    #  GenerateFinalOutput
    #
    def GenerateFinalOutput(self):
        """Create the final output file."""

        #  Assemble the .s file
        if self.outtype != "asm":
            cmdstring = (
                ASSEMBLER
                + " "
                + self.outname
                + ".s -s --list="
                + self.outname
                + ".l --format="
                + self.outtype
                + " --output="
                + self.outname
                + "."
                + self.outext
            )
            if not self.quiet:
                print(cmdstring)
            result = subprocess.run(cmdstring, capture_output=True)
            if (result.returncode == 0) and not self.quiet:
                print("Successful assembly by lwasm")
            elif not self.quiet:
                print("Errors during assembly by lwasm")
                print(result.stderr.decode("UTF-8"))

    # -------------------------------------------------
    #  Compile
    #
    def Compile(self):
        """Compile the files on the command line."""

        self.funcName = "$MAIN$"  #  Name of currently compiling function
        self.Token = "<na>"  #  Currently compiling token
        self.counter = 0  #  Start labels from zero
        self.stringCount = 0  #  String constant name counter

        self.symtbl = {}  #  Ready the symbol table
        self.names = {}  #  Map names to labels
        self.dependencies = []  #  Library word dependencies
        self.functionAddress = False  #  If true, push a function address
        self.referenced = {}  #  True if that function referenced by another
        #  only functions actually referenced are compiled
        self.ParseCommandLine()  #  Parse the command line arguments
        self.LoadLibraryDependencies()  # [TJL] Load and parse the JSON file of library file dependencies
        self.Keywords()  #  Put all keywords into the symbol table
        self.Corewords()  #  [TJL] put all core (intrinsic) words into the symbol table
        self.LoadFiles()  #  Load all source code into self.src
        self.Tokenize()  #  Break up into tokens in self.tokens
        self.Declarations()  #  Find all variables and constants
        self.ParseDataBlocks()  #  Get all data blocks and their data
        self.ParseCodeBlocks()  #  Get all code blocks
        self.ParseFunctions()  #  Get all the functions and their tokens
        self.TagFunctions()  #  Tag used functions
        self.LocateStringConstants()  #  Locate string constants in functions
        self.LibraryRoutines()  #  Determine all library routines used
        self.CompileDataBlocks()  #  Generate assembly code for all data blocks
        self.CompileCodeBlocks()  #  Generate assembly code for all code blocks
        self.CompileFunctions()  #  Generate assembly code for all functions
        self.GenerateAssembly()  #  Output all assembly code and assemble it
        self.GenerateFinalOutput()  #  Convert the assembler output into the final desired format

    # --------------------------------------------------
    #  __init__
    #
    def __init__(self):
        """Build the compiler object."""

        #  Help message
        # fmt: off
        if len(sys.argv) == 1:
            print(("\nSPL Compiler (" + LAST_MOD + ")\n"))
            print(
                (
                    "Use: spl file1 [file2 ...] [-o outfile] [-t type] [-org n] [-var m] "
                    + "[-usp u] [-ssp s] \n"
                )
            )
            print("Where:\n")
            print("    file1     =  first source code file (.spl extension assumed)")
            print("    file2...  =  additional source code files")
            print("    outfile   =  base file name for output files (NO extension)")
            print("    type      =  output file type passed to lwasm, e.g.")
            print("                     ihex = Intel Hex format")
            print("                     srec = Motorola S19 format")
            print("                     asm  = assembly source only")
            print("    org       =  set the ORIGIN address (default is $1000)")
            print("    var       =  set the VARTOP address (default is $BF00)")
            print("    usp       =  set the USER STACK POINTER (default is ORIGIN address)")
            print("    ssp       =  set the SYSTEM STACK POINTER (default is to leave it alone)")
            print("   warn       =  if present, enables Python traceback warnings (default = off)")
            print("   verbose    =  if present, more commented assembly source is generated (default = off)")
            print("   quiet      =  if present, suppresses normal console output (default = off)")
            sys.exit(1)

        # fmt:on
        # Otherwise, compile the files
        self.Compile()


#  Run if not imported
if __name__ == "__main__":
    app = SPLCompiler()

#
#  end spl.py
#

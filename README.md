# Brainfuck Stack

A small Forth-like dialect on top of Brainfuck, that aims at making working with Brainfuck easier
without removing all the fun and challenge of Brainfuck itself.

This repo contains three useful tools for working with Brainfuck programs:
  - *BFS*: the compiler: from a BS file, create a raw BF file;
  - *BFF*: a code formatter / optimizer for BF;
  - *BFD*: an interpreter / debugger for BF, that respects the semi-standard `#` debug instruction

## Language definition

A BS program is composed of functions, one of which must be called `main`. Each function is
implemented by composing other functions together:

```
def inline gtc() [C,C] -> [B] {
  swapc ltc
}
```

At its most basic, even if you use none of the built-in functions nor the type-checker, it allows to
*name* parts of a Brainfuck program for additional readability:

```
def impure main() {
  init
  while (not_eof) {
    process_line
  }
}

def impure process_line() {
  ----------[
    <+>
    >>>+++[
      ...
    ]
  ]
}
```

### Purity

If a function isn't declared as `impure`, it will be type-checked by the compiler. Here, the `gtc`
function declares its type as follows: `[C,C] -> B`, which means that it expects to find two "chars"
on the top of the stack, and will leave a boolean in their place when it's done. The compiler can
verify that it's properly typed by analyzing the types of the functions within:

```
def swapc() [C,C] -> [C,C] { .. }
def ltc()   [C,C] -> [B]   { .. }
```

Impure functions are allowed to use raw brainfuck instructions. It is recommended to still give an
impure function a type annotation: pure functions are allowed to call impure functions, as long as
the type-checker can verify them.

```
def impure inline ltc() [C,C] -> [B] {
  [<[->>+>+<<<]>>>[-<<<+>>>]< not [-<[-]+<+>>]<<->-]< c_to_b
}
```

### Stack

The underlying model of BFS is a stack, as the name suggests: all functions treat the memory tape as
a stack, by pushing values to the right, and treating all memory further right as empty temporary
buffer. This won't be the best approach for all programs, and switching to a mostly impure program
might sometimes be a better approach.

### Inlining

`inline` is a misleading keyword: raw Brainfuck doesn't have functions, so all the code must be
inlined; however, by default, BFS includes function names in the output to make the structure
obvious: functions labelled `inline` won't have a dedicated block in the output. For instance, consider the following program:

```
def impure f1() [C] -> [C] {
  .
}

def inline impure f2() [C] -> [C] {
  .
}

def main() {
  pushc(0)
  while (readc dupc c_to_b) {
    f1
    f2
    f1
  }
  popc
}
```

BFS will output the following raw Brainfuck:
```
main {
  >[-]
  while
    ,[->+>+<<]>>[-<<+>>]<[>+<[-]]>[-<+>]<
  do [[-]<
    f1 {
      .
    }
    .
    f1 {
      .
    }
    ,[->+>+<<]>>[-<<+>>]<[>+<[-]]>[-<+>]<
  done ]<
  [-]<
}
```

### Prelude

[The Prelude](lib/Prelude.bs) contains a set of functions available to all BS programs. It includes
basic operations on 32 bit ints (type `I`) and bytes (type `C`) as well as a notion of boolean (type
`B`: one cell-wide values that are constrained to 0 or 1).

Some [built-in functions](lib/BuiltIn.hs) are not implemented in terms of the language: those are
all functions that take an argument, such as the `roll` series of functions.

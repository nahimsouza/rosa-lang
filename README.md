# Rosa Programming Language Interpreter

This repo contains an interpreter for Rosa programming language, written in SML.

You need to install an SML interpreter, for instance, [SMLNJ](https://www.smlnj.org/).

The Rosa interpreter was based on the calculator example provided with [ML-Yacc User's Manual](https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html).

To execute this, open the `sml` interactive terminal and use CM to build the program:

> `CM.make("sources.cm");`

After the building, the interpreter can also be executed in the terminal:

> `Rosa.parse();`

Have fun!

## To Do

* implement variable declaration

* implement variable assignment for all types

* implement type checking

* implment function declaration

* implement function calling

    * predefined functions

    * user defined functions

* "print" is broken

* implement if expression (val x = if a then b else c)

* implement conditionals

* implement while


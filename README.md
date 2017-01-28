### Description

This function parser module is intended for applications where a set of
mathematical fortran-style expressions is specified at runtime and is
then evaluated for a large number of variable values. This is done by
compiling the set of function strings into byte code, which is
interpreted efficiently for the various variable values.

### Basic usage

#### Module Import

In all program units where you want to use the function parser
you must import the module by:

```fortran
use function_parser
```

This command imports only 3 public types: `fparser`, `fparser_array`, and
`list_of_errors`, which are explained in the following. The remainder of the
module is hidden to the calling program.

#### Function parsing

A single function string `funcstr` can be parsed (checked and compiled) into
bytecode by calling the `fparser` class method subroutine `parse`:

```fortran
call me%parse(funcstr, var, case_sensitive, error_msg)
```

The variable names as they appear in the string `funcstr` have to be passed
in the one-dimensional string array `var` (zero size of `var` is acceptable).
The number of variables is implicitly passed by the dimension of this array.
For some notes on the syntax of the function string see below.

To parse an array of function strings, you can use the `fparser_array` class
method `parse` in a similar manner.

#### Function evaluation

The function value is evaluated for a specific set of variable values
by calling the `fparser` class method subroutine `evaluate`:

```fortran
call me%evaluate(val, res, error_msg)
```

The variable values are passed in the one-dimensional array `val` which must
have the same dimension as array `var`.

To evaluate an array of function strings, you can use the `fparser_array` class
method `evaluate` in a similar manner.

#### Cleanup

To free the memory and destroy a variable of type `fparser` or `fparser_array`,
use the `destroy` method:

```fortran
call me%destroy()
```

### Error handling

Errors can be reported by both the `parse` and `evaluate`
class methods. To check for errors, use the `error` method, and to print them use the `print_errors` method:

```fortran
 if (me%error()) then
     me%print_errors(output_unit)
 end if
```

An error in the function parsing step leads to a detailed error message
(type and position of error). An error during function evaluation returns a function value of 0.0.

### Function string syntax

Although they have to be passed as array elements of the same declared
length (Fortran restriction), the variable names can be of arbitrary
actual length for the parser. Parsing for variables is case sensitive.

The syntax of the function string is similar to the Fortran convention.
Mathematical Operators recognized are `+,` `-,` `*,` `/,` `**` or alternatively `^,`
whereas symbols for brackets must be `()`.

The function parser recognizes the (single argument) Fortran intrinsic
functions `abs`, `exp`, `log10`, `log`, `sqrt`, `sinh`, `cosh`, `tanh`,
`sin`, `cos`, `tan`, `asin`, `acos`, `atan`. Parsing for intrinsic
functions is case INsensitive.

Operations are evaluated in the correct order:

* `()      `    expressions in brackets first
* `-A      `    unary minus (or plus)
* `A**B A^B`    exponentiation (`A` raised to the power `B`)
* `A*B  A/B`    multiplication and division
* `A+B  A-B`    addition and subtraction

The function string can contain integer or real constants. To be recognized
as explicit constants these must conform to the format

`[+|-][nnn][.nnn][e|E|d|D[+|-]nnn]`

where `nnn` means any number of digits. The mantissa must contain at least
one digit before or following an optional decimal point. Valid exponent
identifiers are 'e', 'E', 'd' or 'D'. If they appear they must be followed
by a valid exponent.

### Notes

* The precision of real numbers can be adapted to the calling program by
  adjusting the KIND parameter `wp` in the module.

### Credits

* This code is based on Fortran 95 function parser v1.1 by Roland Schmehl
  <roland.schmehl@alumni.uni-karlsruhe.de>. The source code is available
  from [here](http://fparser.sourceforge.net).
* The function parser concept is based on a C++ class library written by
  Juha Nieminen <warp@iki.fi> available from [here](http://warp.povusers.org/FunctionParser/).
* The original code has been updated to Fortran 2008 by Jacob Williams. Development
  continues on [GitHub](https://github.com/jacobwilliams/fortran_function_parser).
* Note that another refactoring of the original code is available [here](https://github.com/jacopo-chevallard/FortranParser).

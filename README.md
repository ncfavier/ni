# ni (a <> b) == ni a <> ni b

Ni is a *stack-based*, *concatenative* programming language. Here's how it works:

A **program** is a list of values.

A **value** is either:

- an integer: `42`,
- a double: `3.14159`,
- a boolean: `#true`, `#false`,
- a character: `'c'`,
- a string: `"Hello, World!"`,
- a symbol: `foo`, `!@#&*`,
- a list of values: `[a b c ...]`,
- or an environment.

A program is **evaluated** by converting every value to an action and running the actions, one after another, inside a context.

A **context** is a stack of values (**the stack**), and a stack of environments.

An **environment** is a map from symbols to actions.

A **literal** value pushes itself onto the stack.

Everything that is not a symbol is a literal value.

A symbol prefixed with a backslash (`\`) is literal (the backslash is removed).

A symbol prefixed with a dollar sign (`$`) pops a literal value from the stack and binds it to that symbol (with the dollar sign removed) in the topmost environment. A dollar sign on its own pops a value from the stack without binding it to a symbol.

Any other symbol has the action bound to it in the topmost environment in which it is bound.

## Example

The following program prints the first ten Fibonacci numbers:

    \fib [ $n
        0 1
        [ $x $y x y x + ]
        n times
        const
    ] define

    0 $i [
        i fib print
        i increment $i
    ] 10 times

## Library

The following primitives are available.

Unless otherwise specified, `b a`**`f`** means that `f` pops `a` from the stack, then pops `b` from the stack, then pushes the result onto the stack, if any.

#### **`base`** environment

- `action`**`eval`** evaluates `action` in the current context. Lists are evaluated as programs. Symbols are evaluated by making them into a singleton list, for convenience.
- `name action`**`define`** binds the symbol `name` to the action of evaluating `action`.
- `name`**`unbind`** unbinds `name` in the topmost environment.
- `name`**`new`** creates a new empty environment with name `name`.
- `env`**`use`** pushes `env` onto the stack of environments.
- **`unuse`** pops an environment from the stack of environments and pushes it onto the stack.
- `b a`**`=`** and `b a`**`/=`** are the usual equality and inequality functions, respectively. Equality is defined trivially on all values except environments: two environments are equal if they have the same name.
- `a`**`not`**, `b a`**`and`** and `b a`**`or`** are the usual boolean operations.
- `cond yes no`**`ifelse`** evaluates `yes` if `cond` is `#true`, else `no`.
- `b a`**`+`**, `b a`**`-`**, `b a`**`*`**, `b a`**`/`** and `b a`**`^`** are the usual addition, subtraction, multiplication, division and exponentiation operations, respectively, defined on both integers and doubles. **`+`** is also concatenation on strings and lists.
- `a`**`null?`** tests whether `a` is the empty string or list.
- `vs v`**`cons`** appends `v` to the list or string `vs`.
- `vs`**`uncons`** breaks the list or string `vs` into its head and its tail, pushes its tail onto the stack, then pushes its head onto the stack.

#### **`io`** environment

- `v`**`print`** prints `v` to standard output. Strings and characters are printed literally, other values are converted to their string representation.
- **`printStack`** prints the stack, represented as a list, to standard output.
- **`getChar`** reads a character from standard input.
- **`getLine`** reads a line from standard input.
- **`exit`** terminates the program successfully.

#### Standard library

The file [`src/stdlib.ni`](https://git.monade.li/ni/tree/src/stdlib.ni) is evaluated when Ni starts.

## Parsing

Integer, double, character and string literals are parsed as Haskell literals.

Symbols are any sequence of non-whitespace, non-`[]` characters that fail to parse as any other type of value.

Environments have an output representation (`<environment NAME>`), but no input representation.

## Credits

Ni's syntax and core principles were largely suggested to me by [nitrix](https://github.com/nitrix).

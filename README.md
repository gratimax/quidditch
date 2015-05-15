# quidditch

#### What is this?

This is a toy language compiler I made in a night.
It's essentially like every other lisp you've seen, except the compiler isn't very good
and there are no macros.

It is bootstrapped, the compiled compiler written in quidditch (qd.js) can compile itself and any other quidditch program.

It was very helpful in understanding a lot of how compilers work.

#### Why a transpiler to Javascript?

Javascript _looks_ a lot like LISP.
It has closure, arrays, and anonymous functions.
Node also has an easy-enough-to-use standard library that doesn't get in the way.

#### Why "quidditch"?

For some reason I thought it was a nice name. I don't have anything better, so there.

#### Compiling quidditch

Run the compiler like so:

```bash
$ node --stack-size=10000 qd.js quidditch-compiler.qd > quidditch-bootstrapped.js
```

The `--stack-size=10000` parameter is required because the compiler uses rather deep recursion with
no tail call optimization.

#### Next steps/ideas

- better error handling
- macros
- standard library
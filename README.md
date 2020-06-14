Silly little language, with all the characteristics you will hate.

The main twist is there are no variable names. Variables are referred to by
their type, and type aliases are super easy to introduce.

* Indentation is significant, and needs to be 2 spaces.
* There are just basic types, no generics etc (for now?): primitives and records.
* Very flat/incomplete for now: no lexical scoping for types, no concurrency, not even a stack yet.
* The project includes an interpreter. It's not intended to be fast (it isn't), but to try things out.

I intend to make all the mistakes, some of them on purpose. For example, for now the parser is written 'by hand': I'm even postponing reaching for tools like parser generators for now. Probably not a good idea when we want to do operators and operator precedence and things like that, but OK for now.

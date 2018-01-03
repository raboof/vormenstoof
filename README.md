Silly little language, with all the characteristics you will hate.

* Indentation is significant, and needs to be 2 spaces.
* There are just basic types, no generics etc (for now?): primitives and records.
* For brevity, you may omit variable names (rather than types)
* Very flat for now: no lexical scoping for types, no concurrency, not even a stack yet.

I intend to make all the mistakes, some of them on purpose. For example, for now the parser is written 'by hand': I'm even postponing reaching for tools like parser generators for now. Probably not a good idea when we want to do operators and operator precedence and things like that, but OK for now.
# fp-in-haskell

Worked exercises for the book [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala), this time written in Haskell.

See also my [fp-in-scala](https://github.com/robmurtagh/fp-in-scala) repo.

As per the book's introduction, this "_is not a book about Scala. This book is an introduction to functional programming...we use Scala as the vehicle to get you there._".

I chose this book as I'm really interested in the authors' work on the distibuted [Unison Language](http://unisonweb.org), and have found [Paul Chiusano's blog](https://pchiusano.github.io/2017-01-20/why-not-haskell.html) really interesting. The blog post and Unison together form a really compelling approach to improving code composability by removing the I/O boundaries between systems.

I also keep some notes on FP on my wiki (e.g. [Haskell](https://wiki.robmurtagh.com/haskell/haskell), [Scala](https://wiki.robmurtagh.com/scala))

## Quickstart

For basic list of commands, see [package.json](package.json). You'll need to have [stack](https://docs.haskellstack.org/en/stable/README/) installed (and npm for npm scripts only):

Compile and run test specs in `/test`, in general each spec loosely relates to an exercise:

```bash
npm run test
```

If you wanted to make an executable out of these library functions. You'd use the following command to compile `/app/Main.hs`:

```bash
npm run start
```
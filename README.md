# SWAPI Client

Hello. This is just one of the projects I'm working on just to study Haskell.
I wanted to start with something simple so I can study things from the ground
up with practical experience. I'm also noting down some of the things I learned
or realized in the README; something I realized I should be doing more often
thanks to Fly.io.

## Notes

Dates are formatted in DD-MM-YYYY.

### Day 1 - 25/11/2021

Unfortunately, I did not start taking down notes on day 1. So I might have
forgotten some of the things.

- I ran into a bug when using HLS 1.5 with a fresh project generated with 
`cabal init --interactive`. I've detailed the problem [here](https://gist.github.com/sekunho/72747c20a192e62a6fc9dc9e9660aa0a) 
and the workaround is just to use `gen-hie` to generate a simple `hie.yaml`. The
issue is being tracked [here](https://github.com/haskell/haskell-language-server/issues/2398).
- Tackling JSON encoding/decoding is an interesting first problem to solve, more
enjoyable than HTTP clients for now at least since I wanted to get used to building
domain types.
- There are some challenges I ran into, which is a good thing - I can't use
the autoderiving stuff that's available in `aeson` because of two things I wanted:

    1. Encoding a sum type must not preserve the capitalization of a constructor.
    e.g Encoding `data Foo = Bar | Baz` should not result in `"Bar"` or `"Baz"`
    since the SWAPI documentation requires it to be all lowercase.

    2. Naming convention of the JSON field names and the record field accessor
    differentiate. Former being snake case `hello_world`, and the latter being
    camel case `helloWorld`. I also wanted to prefix my record fields so I don't
    run into the headache of one of the record problems that Haskell has. There's
    probably an extension to enable that I missed, but what's one letter/word as
    a prefix really gonna cost me?

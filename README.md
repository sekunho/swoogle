<p align="center">
  <img src="swapi-haskell.svg" />
</p>

# SWAPI Client

[![License: MIT](https://img.shields.io/badge/license-BSD--3--Clause-yellow)](https://opensource.org/licenses/BSD-3-Clause)

`swapi-client` is a client library (without much bells and whistles) to interact
with SWAPI ([Star Wars API](https://swapi.dev)).

``` haskell
import qualified SwapiClient.Api as Api (getFilm)
import SwapiClient.Id (FilmId (FilmId))

main :: IO ()
main = do
  film <- Api.getFilm (FilmId 1)

  print film
```

## Motivation

Hello! This is the first personal project I'm working on to study Haskell, free
from following a book or tutorial as the main reference because it feels like
I've been stuck in tutorial hell without any way out.  I wanted to start with
something relatively simple so I can study things from the ground up with practical
experience. I'm also noting down some of the things I learned or realized in the
README; something I realized I should be doing more often thanks to Fly.io.

I don't recommend using this as THE tutorial for this sort of thing because I'm
just trying to piece things together like a caveman moments before fire was
discovered.

Making this involves some of the commonly used Haskell libraries like `aeson`,
`containers`, an HTTP client library I have not yet decided on, etc. and other
basic Haskell concepts to be aware of – which makes it a good candidate for a
starter project! Of course it doesn't require a deep dive in the aforementioned
libraries; it's just enough to make things work.

My goals for this project are:

- Explore the different libraries in the ecosystem
- Try out the more common language extensions (new Haskell report when?)
- Make things explicit. e.g instance signatures, kind signatures, deriving, etc.
- Try out the different testing strategies

But of course I can't try out everything and I need to put a stop somewhere else
I risk getting overwhelmed and back in "tutorial" hell again. I'll only read
enough for what I need to know, and I'll stop there. So if you get mad at me
for not using `<INSERT_YOUR_FAVORITE_THING_HERE>`, get a grip!

## Will this be a video

Yeah! So far I've recorded most of each and every session that I worked on this.
I'm still assessing if this should be a per episode video or just put the entire
journey in one video. I don't have an editor so I usually do all the editing
myself (which takes a lot of time). There's a lot of raw footage I'm gonna have
to go through unfortunately.

Whenever it's ready, it'll  be up on
[youtube.com/sekunho](https://www.youtube.com/sekunho/)
when I'm done. Subscribe to stay tuned! :)

## Queryable resources

- [ ] Root
- [x] People
  - [x] Index
  - [x] Search
  - [x] View
- [x] Film
  - [x] Index
  - [x] Search
  - [x] View
- [x] Starship
  - [x] Index
  - [x] Search
  - [x] View
- [ ] Vehicle
- [ ] Species
- [ ] Planet

## List of improvements for swapi.dev

Although this is somewhat unrelated to the library itself, there are some changes
I would like to make, should I get the free time to build a JSON api.

- A collection of things is sometimes encoded as a comma delimited string instead
of using an array

- Numbers are encoded as strings just because the field could be an actual string
like `"n/a"`, or `"none"`

- Inconsistent usage of units. e.g In `Starship`, it has a field called 
`max_atmosphering_speed` with the kilometer unit, and without. Both of which are
numbers, just encoded as strings. What's the unitless number supposed to be?

- `api/<resource>/schema/` does not work

- `Starship`: `crew` has a range value for one, and just a number for the others

- `Starship`: `length`'s number formatting is inconsistent

- `Starship`: `starship_class` character case is inconsistent

These changes won't be implemented here, unless the swapi.dev maintainer decides
to do the same as well. `swapi` is just meant to be compatible with swapi.dev.

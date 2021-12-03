# SWAPI Client

Hello. This is just one of the projects I'm working on just to study Haskell.
I wanted to start with something simple so I can study things from the ground
up with practical experience. I'm also noting down some of the things I learned
or realized in the README; something I realized I should be doing more often
thanks to Fly.io.

## Table of Contents

- [Video](#video)
- [Features](#features)
  - [Queryable resources](#queryable-resources)
  - [Parseable resources](#parseable-resources)
- [Notes](#notes)
  - [Day 8 - 01/12/2021](#day-8---03122021)
  - [Day 7 - 01/12/2021](#day-7---01122021)
  - [Day 6 - 30/11/2021](#day-6---30112021)
  - [Day 5 - 29/11/2021](#day-5---29112021)
  - [Day 4 - 28/11/2021](#day-4---28112021)
  - [Day 3 - 27/11/2021](#day-3---27112021)
  - [Day 2 - 26/11/2021](#day-2---26112021)
  - [Day 1 - 25/11/2021](#day-1---25112021)

## Video

This isn't completed yet so I haven't put together a video to summarize a lot of
what I encountered. But it'll be up in my 
[YouTube channel](https://www.youtube.com/channel/UCUa0_AVebTfZzyzXjfB4dJQ/) 
when I'm done. So subscribe to stay tuned! :)

## Features

### Queryable resources

- [ ] Root
- [ ] People
- [ ] Film
- [ ] Starship
- [ ] Vehicle
- [ ] Species
- [ ] Planet

### Parseable resources

Resources/schemas that can be encoded/decoded to and from JSON respectively.

You play around with it in GHCI if you want to, like so:

``` haskell
ghci> import Data.Aeson (eitherDecode, encode)
ghci> sampleJSON = encode "{\"name\":\"Luke Skywalker\",\"height\":\"172\",\"mass\":\"77\",\"hair_color\":\"blond\",\"skin_color\":\"fair\",\"eye_color\":\"blue\",\"birth_year\":\"19BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\"],\"species\":[],\"vehicles\":[\"https://swapi.dev/api/vehicles/14/\",\"https://swapi.dev/api/vehicles/30/\"],\"starships\":[\"https://swapi.dev/api/starships/12/\",\"https://swapi.dev/api/starships/22/\"],\"created\":\"2014-12-09T13:50:51.644000Z\",\"edited\":\"2014-12-20T21:17:56.891000Z\",\"url\":\"https://swapi.dev/api/people/1/\"}"
Right (Person {pName = PersonName "Luke Skywalker", pHeight = Height 172, pMass = Mass 77.0, pHairColor = HairColors [BlondHair], pSkinColor = SkinColors [FairSkin], pEyeColor = BlueEye, pBirthYear = BBY 19.0, pGender = Male, pHomeworldId = HomeworldId 1, pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 6], pSpeciesIds = [], pVehicleIds = [VehicleId 14,VehicleId 30], pStarshipIds = [StarshipId 12,StarshipId 22], pCreatedAt = 2014-12-09 13:50:51.644 UTC, pEditedAt = 2014-12-20 21:17:56.891 UTC, pId = PersonId 1})
ghci> Right p1 = eitherDecode @Person sampleJSON
ghci> encode p1
"{\"birth_year\":\"19.0BBY\",\"created\":\"2014-12-09T13:50:51.644Z\",\"edited\":\"2014-12-20T21:17:56.891Z\",\"eye_color\":\"blue\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\"],\"gender\":\"male\",\"hair_color\":\"blond\",\"height\":\"172\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"mass\":\"77.0\",\"name\":\"Luke Skywalker\",\"skin_color\":\"fair\",\"species\":[],\"starships\":[\"https://swapi.dev/api/starships/12/\",\"https://swapi.dev/api/starships/22/\"],\"url\":\"https://swapi.dev/api/people/1/\",\"vehicles\":[\"https://swapi.dev/api/vehicles/14/\",\"https://swapi.dev/api/vehicles/30/\"]}"
```

- [ ] Root
- [x] People
- [ ] Film
- [ ] Starship
- [ ] Vehicle
- [ ] Species
- [ ] Planet

## Notes

Dates are formatted in DD-MM-YYYY.

### Day 8 - 03/12/2021

- Same thing as some of the fields, I added a `PersonId` newtype with some
smart constructors to ensure that it's at least `> 0`.
- I changed the smart constructor check from `>= 0` to `> 0` cause I just found
out that it actually starts with `1`.
- Add `edited` and `created` fields. So now it converts an ISO8601 UTC timestamp
into the appropriate domain type

### Day 7 - 01/12/2021

Not much today. Asked the community in the Matrix server (#Haskell), and Twitter
for some feedback. Got nothing yet. I think I'll wait a bit longer. Thinking of
pausing this for a bit while I cover some other material.

- Added the remaining instance signatures. I'm honestly surprised that this isn't
a default extension. It's just too convenient especially to those unfamiliar.

### Day 6 - 30/11/2021

- I didn't have to make individual functions for converting the colors to `Text`
, I could just implement the instances for `TextShow`, and done!
- With the necessary `TextShow` instances, I could just make a function to:

  1. map `TextShow a => [a]` to `[Text]`
  2. intercalate `", "` and concatenate the list of `Text` to a single `Text`

  I could just move the function out somewhere if ever I need it elsewhere.
- Only hair colors and skin colors have multiple values. It's possible for eye
colors to have, heterochromia and all that, but it doesn't seem like that's the
case in the SWAPI db dump. So, I'll keep `EyeColor` singular for now.
- I moved out any color-related type/function/instance to `SwapiClient/Color.hs`
cause it had a bit too much going on.
- I had to add functions that unwrap newtypes because it was a bit inconvenient
to manually pattern match the boxed value. This way I get to compose things
better!

```diff haskell
instance ToJSON (SkinColors :: Type) where
  toJSON :: SkinColors -> Value
- toJSON (SkinColors scs) = String . commaConcat $ scs
+ toJSON = String . commaConcat . unSkinColors
  
unSkinColors :: SkinColors -> [SkinColors]
unSkinColors (SkinColors scs) = scs
```

  It's a possibility that I might end up unboxing it elsewhere, given it's a
  common enough thing to do.

### Day 5 - 29/11/2021

- I added the remaining list of resources associated with `Person`.
- Refactored the colors for hair, skin, and eye, and just made them all sum types
instead. Turns out the colors vary from each other to a certain degree, and it
is pretty odd if I just use a general sum type for all colors.
- Made smart constructors for creating an ID. Can't be negative, ever.
- Fixed the incorrect implementation of the ID newtypes' `ToJSON` instances. It
had to have a trailing forward slash otherwise it would've been parsed 
incorrectly. 
- Refactored the aeson instances for the ID newtypes to use the smart 
constructors.
- In the `parseJSON` implementation for the ID newtypes, I think this is where
monads would be useful, especially since I could avoid the nested cases. For now
I wrote them all manually. I'm a bit sleepy.
- SWAPI sometimes formats a collection of things as a text that is comma 
separated. A bit annoying, but nothing too bad. I did run into an issue though
where I couldn't just simply parse a text value into a list of constructors.
Here's an example:

``` haskell
import Data.Aeson qualified as (withText)
import Data.Text qualified as Text (split)

-- This is just an example, not the complete type.
data HairColor = Red | Blue

instance FromJSON (HairColor :: Type) where
  parseJSON =
    Aeson.withText "HairColor" $
      \(hairColorText :: Text) ->
        let hairColors :: Either String [HairColor]
            hairColors = mapM textToHairColor . Text.split (== ',')
              $ hairColorText
        in case hairColors of
             Right hcs = pure hcs
             Left e -> fail e

textToHairColor :: Text -> Either String HairColor
textToHairColor hct = case hct of
  "red" -> Right Red
  "blue" -> Right Blue
  _ -> Left "ERROR: Unexpected color value/format"
```

  The problem would be that for this instance, it's
  `parseJSON :: Value -> Parser HairColor`, but instead it's going to be
  `Value -> Parser [HairColor]`. Hmm.
  
  The first thing that came to mind was to just simply enable `FlexibleInstances`
  and just swap `FromJSON (HairColor :: Type)` with `FromJSON ([HairColor :: Type])`.
  Easy huh. Well, no, because I quickly ran into another issue about overlapping
  instances! I definitely don't want to override anything, if ever that's what I'm
  doing. I did some digging and found [this](https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list)
  which is pretty handy. It didn't really answer my question but I noticed that
  the asker just wrapped it in a `newtype`, and then used it for the instance.
  A bit annoying that I have to add one more type, but it seems convenient enough.
  There might be a better way, which I'll ask the community in the future. But for
  now, this works just fine.

- I have to reimplement the `aeson` instances for the colors. I've already done
the one for `HairColors`, but I have lots more to go. I should probably start
moving some things into their own modules as well; maybe in the future.

### Day 4 - 28/11/2021

- Moved `*Ids` into an `Id` module
- Films: decode URL to ID; encode ID to URL
- Homeworld: decode URL to ID; encode ID to URL
- There are cases where I had to deal with `Int -> Text`, but there is no such
signature for `show`. I just used `Text.pack . show`, but then wondered if there
was a better way in terms of ergonomics and performance. I found `text-show`,
and tried it out in some cases. Not sure how I feel about it overall, since I'm
not sure if it really makes that big of a difference for this tiny application.
- Decided to make the `*Id` types as opaque types - where I can't touch the data
constructors, and have to rely on the smart constructors. I think it's better
this way, so I can ensure that it's at least a non-negative number.

### Day 3 - 27/11/2021

- I was a bit confused why I was able to use `fail`. I don't know why I was even
confused. I realized that `Parser` has a `MonadFail` instance so that's why.
Cool. To avoid going down a rabbit hole again, I'll avoid the technicalities of
`MonadFail` for now.
- I don't think I should've implemented the `ToJSON` instances cause I don't 
really need them. I'm just decoding JSON to domain types after all. But I guess
this would be useful when I make a replacement API for SWAPI in the future? For
now I'll just leave these here.
- I enabled `KindSignatures` to make things more explicit. Helps me out quite a
bit.
- It isn't really helpful to have `Homeworld` but it's actually just an ID. Later
on I'll be making a `Homeworld` type with the actual data, not just a reference
through an ID. So to make that distinction, I'm gonna suffix the types and field
names with `Id`(s) if they deal with Ids, and none if without.
- I have to find a way to organize the modules that doesn't cause circular 
imports. Right now I have imported `HomeworldId` from `SwapiClient.Homeworld`
and `FilmId` from `SwapiClient.Film`. If I put the `Homeworld` and `Film` records
in the corresponding modules, then I'll have to import some things from other
modules that already have those modules imported. I don't know how to word it 
better, but the idea is that I think I have to put the `Id` types in separate
modules as well. I could also just make a module `Id` that has all of the ID
types instead, that way it won't be too annoying to keep track of? I don't know
yet.

### Day 2 - 26/11/2021

- I just found out that `text` had some handy functions in the `Data.Text.Read`
module that does what I was annoyed with - parsing something like `"20BBY"`! How
I've initially done it was because the time periods `BBY` and `ABY` had 3 chars,
I just took the last 3 characters of the value and checked if it matched either
of the two. `Text.Read.decimal` is convenient because of this:

``` haskell
data BirthYear
  = BBY Double
  | ABY Double
  | UnknownBirthYear
  deriving Show

foo =
  let str = "20BBY"
  in case decimal str of
        Left e -> error "Does not start with a number"
        Right (years, "BBY") -> BBY years
        Right (years, "ABY") -> ABY years
```

It's at least much easier compared to manually taking apart the text and parsing
the number.
- I added the rest of the `Person` fields which wasn't really difficult. It was
just a chore to have to write each instance for each domain type I wrote, but
it helps things stick in my head.
- I was stuck with how to deal with `"unknown"` that SWAPI likes to use. Like
when the mass of a person is unknown so it becomes `"mass": "unknown"` rather
than a `Double`. I initially made it `Maybe Mass` (`newtype Mass = Mass Double`)
but then I ran into issues when implementing the `FromJSON` instance for it.
I couldn't deal with the unknown part of it, and I'm not sure how so I figured
I would just turn it into a sum type, and add an extra constructor `UnknownMass`.
- One of the things that puzzle me is, how do I handle unexpected types in
`parseJSON`? What if I got an `Array a` instead of `String s` for example?
Currently just using `error` which sounds OK in my head because I'm alright with
things failing at the boundary of the library. I think I remember something about
using `throwError` instead, or something like that, because you have to add it 
to the type constraints, but I can't remember exactly what it was called. Although
I did find in the `aeson` docs that they used `fail` cause of `MonadFail`.
- I enabled the `StrictData` language extension cause it seems to be a general
enough consensus to use it.
- Before I wrote the entire instance for `FromJSON Homeworld`, GHC got confused
when I used `toBoundedInteger` cause it couldn't figure out which type should
inhabit `Integral i`, so I used `TypeApplications` to let it know while I'm still
working on it. e.g `toBoundedInteger @Int homeworldId`. A reason why I left it
on in Cabal cause I don't really want to manually enable it again and again.
- I used `newtype`s for stuff like `Mass`, `Height`, and so I was expecting some
issues when decoding it to JSON. What if I had to manually unwrap the contents?
Turns out I didn't have to, which is nice. Then I wondered: Huh? Why didn't it
require me to? Which I quickly, and stupidly, realized that it's because that's
how I implemented the `ToJSON` instances.

I still have some stuff to do, the most annoying one is how I convert `Double`s
back to `String`s. `"19.1BBY"` to `BBY 19.1` to `"19.1BBY"` is expected. But
`"19BBY"` to `BBY 19.0` to `"19.0BBY"` isn't since SWAPI truncates `.0` if it can
be turned to a whole number.

### Day 1 - 25/11/2021

Unfortunately, I did not start taking down notes on day 1. So I might have
forgotten some of the things.

- I ran into a bug when using HLS 1.5 with a fresh project generated with 
`cabal init --interactive`. I've detailed the problem [here](https://gist.github.com/sekunho/72747c20a192e62a6fc9dc9e9660aa0a) 
and the workaround is just to use `gen-hie` to generate a simple `hie.yaml`. The
issue is being tracked [here](https://github.com/haskell/haskell-language-server/issues/2398).
- I placed everything in `SwapiClient.hs` for now because I don't really have
to organize things (yet). I know, I will do so in the future when I start adding
other things as well.
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

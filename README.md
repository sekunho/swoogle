# SWAPI Client

Hello! This is the first personal project I'm working on to study Haskell, free
from following a book or tutorial as the main reference because it feels like
I've been stuck in tutorial hell without any way out.  I wanted to start with 
something relatively simple so I can study things from the ground up with practical 
experience. I'm also noting down some of the things I learned or realized in the 
README; something I realized I should be doing more often thanks to Fly.io.

I don't recommend using this as THE tutorial for this sort of thing because I'm
just trying to piece things together like a caveman moments before fire was 
discovered.

## What's this about?

`swapi` is a client library (without much bells and whistles) for SWAPI
([Star Wars API](https://swapi.dev)). Making this involves some of the commonly
used Haskell libraries like `aeson`, `containers`, an HTTP client library I have
not yet decided on, etc. and other basic Haskell concepts to be aware of – which 
makes it a good candidate for a starter project! Of course it doesn't require a 
deep dive in the aforementioned libraries; it's just enough to make things work.

## Will this be a video?

Yeah! So far I've recorded most of each and every session that I worked on this. 
I'm still assessing if this should be a per episode video or just put the entire 
journey in one video. I don't have an editor so I usually do all the editing
myself (which takes a lot of time). There's a lot of raw footage I'm gonna have
to go through unfortunately.

Whenever it's ready, it'll  be up on 
[youtube.com/sekunho](https://www.youtube.com/sekunho/) 
when I'm done. Subscribe to stay tuned! :)

## Table of Contents

- [Features](#features)
  - [Queryable resources](#queryable-resources)
  - [Parseable resources](#parseable-resources)
- [Notes](#notes)
  - [Day 15 - 16/01/2022](#day-15---16012022)
  - [Day 14 - 14/01/2022](#day-14---14012022)
  - [Day 13 - 15/12/2021](#day-13---15122021)
  - [Day 12 - 14/12/2021](#day-12---14122021)
  - [Day 11 - 11/12/2021](#day-11---11122021)
  - [Day 10 - 08/12/2021](#day-10---08122021)
  - [Day 9 - 06/12/2021](#day-9---06122021)
  - [Day 8 - 03/12/2021](#day-8---03122021)
  - [Day 7 - 01/12/2021](#day-7---01122021)
  - [Day 6 - 30/11/2021](#day-6---30112021)
  - [Day 5 - 29/11/2021](#day-5---29112021)
  - [Day 4 - 28/11/2021](#day-4---28112021)
  - [Day 3 - 27/11/2021](#day-3---27112021)
  - [Day 2 - 26/11/2021](#day-2---26112021)
  - [Day 1 - 25/11/2021](#day-1---25112021)

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

- [x] Root
- [x] People
  - [x] Index
  - [x] View
- [ ] Film
- [ ] Starship
- [ ] Vehicle (Try to use the autoderive)
- [ ] Species
- [ ] Planet

You play around with it in GHCI if you want to, like so:

``` haskell
ghci> import Data.Aeson (eitherDecode, encode)

ghci> sampleJSON = "{\"name\":\"Luke Skywalker\",\"height\":\"172\",\"mass\":\"77\",\"hair_color\":\"blond\",\"skin_color\":\"fair\",\"eye_color\":\"blue\",\"birth_year\":\"19BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\"],\"species\":[],\"vehicles\":[\"https://swapi.dev/api/vehicles/14/\",\"https://swapi.dev/api/vehicles/30/\"],\"starships\":[\"https://swapi.dev/api/starships/12/\",\"https://swapi.dev/api/starships/22/\"],\"created\":\"2014-12-09T13:50:51.644000Z\",\"edited\":\"2014-12-20T21:17:56.891000Z\",\"url\":\"https://swapi.dev/api/people/1/\"}"
Right (Person {pName = PersonName "Luke Skywalker", pHeight = Height 172, pMass = Mass 77.0, pHairColor = HairColors [BlondHair], pSkinColor = SkinColors [FairSkin], pEyeColor = BlueEye, pBirthYear = BBY 19.0, pGender = Male, pHomeworldId = HomeworldId 1, pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 6], pSpeciesIds = [], pVehicleIds = [VehicleId 14,VehicleId 30], pStarshipIds = [StarshipId 12,StarshipId 22], pCreatedAt = 2014-12-09 13:50:51.644 UTC, pEditedAt = 2014-12-20 21:17:56.891 UTC, pId = PersonId 1})
ghci> Right p1 = eitherDecode @Person sampleJSON
ghci> encode p1
"{\"birth_year\":\"19.0BBY\",\"created\":\"2014-12-09T13:50:51.644Z\",\"edited\":\"2014-12-20T21:17:56.891Z\",\"eye_color\":\"blue\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\"],\"gender\":\"male\",\"hair_color\":\"blond\",\"height\":\"172\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"mass\":\"77.0\",\"name\":\"Luke Skywalker\",\"skin_color\":\"fair\",\"species\":[],\"starships\":[\"https://swapi.dev/api/starships/12/\",\"https://swapi.dev/api/starships/22/\"],\"url\":\"https://swapi.dev/api/people/1/\",\"vehicles\":[\"https://swapi.dev/api/vehicles/14/\",\"https://swapi.dev/api/vehicles/30/\"]}"

ghci> indexJSON = "{\"count\":18,\"next\":\"https://swapi.dev/api/people/?search=b&page=2&format=json\",\"previous\":null,\"results\":[{\"name\":\"Beru Whitesun lars\",\"height\":\"165\",\"mass\":\"75\",\"hair_color\":\"brown\",\"skin_color\":\"light\",\"eye_color\":\"blue\",\"birth_year\":\"47BBY\",\"gender\":\"female\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/5/\",\"https://swapi.dev/api/films/6/\"],\"species\":[],\"vehicles\":[],\"starships\":[],\"created\":\"2014-12-10T15:53:41.121000Z\",\"edited\":\"2014-12-20T21:17:50.319000Z\",\"url\":\"https://swapi.dev/api/people/7/\"},{\"name\":\"Biggs Darklighter\",\"height\":\"183\",\"mass\":\"84\",\"hair_color\":\"black\",\"skin_color\":\"light\",\"eye_color\":\"brown\",\"birth_year\":\"24BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"films\":[\"https://swapi.dev/api/films/1/\"],\"species\":[],\"vehicles\":[],\"starships\":[\"https://swapi.dev/api/starships/12/\"],\"created\":\"2014-12-10T15:59:50.509000Z\",\"edited\":\"2014-12-20T21:17:50.323000Z\",\"url\":\"https://swapi.dev/api/people/9/\"},{\"name\":\"Obi-Wan Kenobi\",\"height\":\"182\",\"mass\":\"77\",\"hair_color\":\"auburn, white\",\"skin_color\":\"fair\",\"eye_color\":\"blue-gray\",\"birth_year\":\"57BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/20/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/4/\",\"https://swapi.dev/api/films/5/\",\"https://swapi.dev/api/films/6/\"],\"species\":[],\"vehicles\":[\"https://swapi.dev/api/vehicles/38/\"],\"starships\":[\"https://swapi.dev/api/starships/48/\",\"https://swapi.dev/api/starships/59/\",\"https://swapi.dev/api/starships/64/\",\"https://swapi.dev/api/starships/65/\",\"https://swapi.dev/api/starships/74/\"],\"created\":\"2014-12-10T16:16:29.192000Z\",\"edited\":\"2014-12-20T21:17:50.325000Z\",\"url\":\"https://swapi.dev/api/people/10/\"},{\"name\":\"Chewbacca\",\"height\":\"228\",\"mass\":\"112\",\"hair_color\":\"brown\",\"skin_color\":\"unknown\",\"eye_color\":\"blue\",\"birth_year\":\"200BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/14/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\"],\"species\":[\"https://swapi.dev/api/species/3/\"],\"vehicles\":[\"https://swapi.dev/api/vehicles/19/\"],\"starships\":[\"https://swapi.dev/api/starships/10/\",\"https://swapi.dev/api/starships/22/\"],\"created\":\"2014-12-10T16:42:45.066000Z\",\"edited\":\"2014-12-20T21:17:50.332000Z\",\"url\":\"https://swapi.dev/api/people/13/\"},{\"name\":\"Jabba Desilijic Tiure\",\"height\":\"175\",\"mass\":\"1,358\",\"hair_color\":\"n/a\",\"skin_color\":\"green-tan, brown\",\"eye_color\":\"orange\",\"birth_year\":\"600BBY\",\"gender\":\"hermaphrodite\",\"homeworld\":\"https://swapi.dev/api/planets/24/\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/4/\"],\"species\":[\"https://swapi.dev/api/species/5/\"],\"vehicles\":[],\"starships\":[],\"created\":\"2014-12-10T17:11:31.638000Z\",\"edited\":\"2014-12-20T21:17:50.338000Z\",\"url\":\"https://swapi.dev/api/people/16/\"},{\"name\":\"Boba Fett\",\"height\":\"183\",\"mass\":\"78.2\",\"hair_color\":\"black\",\"skin_color\":\"fair\",\"eye_color\":\"brown\",\"birth_year\":\"31.5BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/10/\",\"films\":[\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/5/\"],\"species\":[],\"vehicles\":[],\"starships\":[\"https://swapi.dev/api/starships/21/\"],\"created\":\"2014-12-15T12:49:32.457000Z\",\"edited\":\"2014-12-20T21:17:50.349000Z\",\"url\":\"https://swapi.dev/api/people/22/\"},{\"name\":\"Bossk\",\"height\":\"190\",\"mass\":\"113\",\"hair_color\":\"none\",\"skin_color\":\"green\",\"eye_color\":\"red\",\"birth_year\":\"53BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/29/\",\"films\":[\"https://swapi.dev/api/films/2/\"],\"species\":[\"https://swapi.dev/api/species/7/\"],\"vehicles\":[],\"starships\":[],\"created\":\"2014-12-15T12:53:49.297000Z\",\"edited\":\"2014-12-20T21:17:50.355000Z\",\"url\":\"https://swapi.dev/api/people/24/\"},{\"name\":\"Lobot\",\"height\":\"175\",\"mass\":\"79\",\"hair_color\":\"none\",\"skin_color\":\"light\",\"eye_color\":\"blue\",\"birth_year\":\"37BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/6/\",\"films\":[\"https://swapi.dev/api/films/2/\"],\"species\":[],\"vehicles\":[],\"starships\":[],\"created\":\"2014-12-15T13:01:57.178000Z\",\"edited\":\"2014-12-20T21:17:50.359000Z\",\"url\":\"https://swapi.dev/api/people/26/\"},{\"name\":\"Ackbar\",\"height\":\"180\",\"mass\":\"83\",\"hair_color\":\"none\",\"skin_color\":\"brown mottle\",\"eye_color\":\"orange\",\"birth_year\":\"41BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/31/\",\"films\":[\"https://swapi.dev/api/films/3/\"],\"species\":[\"https://swapi.dev/api/species/8/\"],\"vehicles\":[],\"starships\":[],\"created\":\"2014-12-18T11:07:50.584000Z\",\"edited\":\"2014-12-20T21:17:50.362000Z\",\"url\":\"https://swapi.dev/api/people/27/\"},{\"name\":\"Nien Nunb\",\"height\":\"160\",\"mass\":\"68\",\"hair_color\":\"none\",\"skin_color\":\"grey\",\"eye_color\":\"black\",\"birth_year\":\"unknown\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/33/\",\"films\":[\"https://swapi.dev/api/films/3/\"],\"species\":[\"https://swapi.dev/api/species/10/\"],\"vehicles\":[],\"starships\":[\"https://swapi.dev/api/starships/10/\"],\"created\":\"2014-12-18T11:26:18.541000Z\",\"edited\":\"2014-12-20T21:17:50.371000Z\",\"url\":\"https://swapi.dev/api/people/31/\"}]}"
ghci> eitherDecode @PersonIndex indexJSON
Right (PersonIndex {pCount = 18, pNextPage = PersonPage 2, pPreviousPage = NoPage, pResults = [Person {pName = PersonName "Beru Whitesun lars", pHeight = Height 165, pMass = Mass 75.0, pHairColor = HairColors [BrownHair], pSkinColor = SkinColors [LightSkin], pEyeColor = BlueEye, pBirthYear = BBY 47.0, pGender = Female, pHomeworldId = HomeworldId 1, pFilmIds = [FilmId 1,FilmId 5,FilmId 6], pSpeciesIds = [], pVehicleIds = [], pStarshipIds = [], pCreatedAt = 2014-12-10 15:53:41.121 UTC, pEditedAt = 2014-12-20 21:17:50.319 UTC, pId = PersonId 7},Person {pName = PersonName "Biggs Darklighter", pHeight = Height 183, pMass = Mass 84.0, pHairColor = HairColors [BlackHair], pSkinColor = SkinColors [LightSkin], pEyeColor = BrownEye, pBirthYear = BBY 24.0, pGender = Male, pHomeworldId = HomeworldId 1, pFilmIds = [FilmId 1], pSpeciesIds = [], pVehicleIds = [], pStarshipIds = [StarshipId 12], pCreatedAt = 2014-12-10 15:59:50.509 UTC, pEditedAt = 2014-12-20 21:17:50.323 UTC, pId = PersonId 9},Person {pName = PersonName "Obi-Wan Kenobi", pHeight = Height 182, pMass = Mass 77.0, pHairColor = HairColors [AuburnHair,WhiteHair], pSkinColor = SkinColors [FairSkin], pEyeColor = BlueGreyEye, pBirthYear = BBY 57.0, pGender = Male, pHomeworldId = HomeworldId 20, pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 4,FilmId 5,FilmId 6], pSpeciesIds = [], pVehicleIds = [VehicleId 38], pStarshipIds = [StarshipId 48,StarshipId 59,StarshipId 64,StarshipId 65,StarshipId 74], pCreatedAt = 2014-12-10 16:16:29.192 UTC, pEditedAt = 2014-12-20 21:17:50.325 UTC, pId = PersonId 10},Person {pName = PersonName "Chewbacca", pHeight = Height 228, pMass = Mass 112.0, pHairColor = HairColors [BrownHair], pSkinColor = SkinColors [UnknownSkinColor], pEyeColor = BlueEye, pBirthYear = BBY 200.0, pGender = Male, pHomeworldId = HomeworldId 14, pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 6], pSpeciesIds = [SpeciesId 3], pVehicleIds = [VehicleId 19], pStarshipIds = [StarshipId 10,StarshipId 22], pCreatedAt = 2014-12-10 16:42:45.066 UTC, pEditedAt = 2014-12-20 21:17:50.332 UTC, pId = PersonId 13},Person {pName = PersonName "Jabba Desilijic Tiure", pHeight = Height 175, pMass = Mass 1358.0, pHairColor = HairColors [NoHairColor], pSkinColor = SkinColors [GreenTanSkin,BrownSkin], pEyeColor = OrangeEye, pBirthYear = BBY 600.0, pGender = Hermaphrodite, pHomeworldId = HomeworldId 24, pFilmIds = [FilmId 1,FilmId 3,FilmId 4], pSpeciesIds = [SpeciesId 5], pVehicleIds = [], pStarshipIds = [], pCreatedAt = 2014-12-10 17:11:31.638 UTC, pEditedAt = 2014-12-20 21:17:50.338 UTC, pId = PersonId 16},Person {pName = PersonName "Boba Fett", pHeight = Height 183, pMass = Mass 78.2, pHairColor = HairColors [BlackHair], pSkinColor = SkinColors [FairSkin], pEyeColor = BrownEye, pBirthYear = BBY 31.5, pGender = Male, pHomeworldId = HomeworldId 10, pFilmIds = [FilmId 2,FilmId 3,FilmId 5], pSpeciesIds = [], pVehicleIds = [], pStarshipIds = [StarshipId 21], pCreatedAt = 2014-12-15 12:49:32.457 UTC, pEditedAt = 2014-12-20 21:17:50.349 UTC, pId = PersonId 22},Person {pName = PersonName "Bossk", pHeight = Height 190, pMass = Mass 113.0, pHairColor = HairColors [NoHairColor], pSkinColor = SkinColors [GreenSkin], pEyeColor = RedEye, pBirthYear = BBY 53.0, pGender = Male, pHomeworldId = HomeworldId 29, pFilmIds = [FilmId 2], pSpeciesIds = [SpeciesId 7], pVehicleIds = [], pStarshipIds = [], pCreatedAt = 2014-12-15 12:53:49.297 UTC, pEditedAt = 2014-12-20 21:17:50.355 UTC, pId = PersonId 24},Person {pName = PersonName "Lobot", pHeight = Height 175, pMass = Mass 79.0, pHairColor = HairColors [NoHairColor], pSkinColor = SkinColors [LightSkin], pEyeColor = BlueEye, pBirthYear = BBY 37.0, pGender = Male, pHomeworldId = HomeworldId 6, pFilmIds = [FilmId 2], pSpeciesIds = [], pVehicleIds = [], pStarshipIds = [], pCreatedAt = 2014-12-15 13:01:57.178 UTC, pEditedAt = 2014-12-20 21:17:50.359 UTC, pId = PersonId 26},Person {pName = PersonName "Ackbar", pHeight = Height 180, pMass = Mass 83.0, pHairColor = HairColors [NoHairColor], pSkinColor = SkinColors [BrownMottleSkin], pEyeColor = OrangeEye, pBirthYear = BBY 41.0, pGender = Male, pHomeworldId = HomeworldId 31, pFilmIds = [FilmId 3], pSpeciesIds = [SpeciesId 8], pVehicleIds = [], pStarshipIds = [], pCreatedAt = 2014-12-18 11:07:50.584 UTC, pEditedAt = 2014-12-20 21:17:50.362 UTC, pId = PersonId 27},Person {pName = PersonName "Nien Nunb", pHeight = Height 160, pMass = Mass 68.0, pHairColor = HairColors [NoHairColor], pSkinColor = SkinColors [GreySkin], pEyeColor = BlackEye, pBirthYear = UnknownBirthYear, pGender = Male, pHomeworldId = HomeworldId 33, pFilmIds = [FilmId 3], pSpeciesIds = [SpeciesId 10], pVehicleIds = [], pStarshipIds = [StarshipId 10], pCreatedAt = 2014-12-18 11:26:18.541 UTC, pEditedAt = 2014-12-20 21:17:50.371 UTC, pId = PersonId 31}]})
```

## Notes

Dates are formatted in DD-MM-YYYY.

### Day 15 - 16/01/2022

LTE was terrible yesterday. I couldn't anything done with 1Kbps. Honestly...

#### `cabal` shenanigans with `--enable-tests`

So turns out that `cabal` doesn't run resolve test dependencies, or something,
by default. Could be understanding it wrong, but that's how it is to me. So,
I had to use this flag `--enable-tests` so that it would actually run the tests.
But, HLS also needed that flag! I don't know how to pass flags to HLS, and it
seemed kinda janky if I were to manually do it all the time. So I had to look into
`cabal.project`, specifically `cabal.project.local`, and explicitly state the
package and its flags.

This is how it looks like at the moment:

``` cabal-config
package swapi-client
  tests: true
```

For some reason, this file is `.gitignore`'d by default. I've seen some repos
with this file though so I'm not sure why this is the case. I removed it, and
added it to version control for sanity.

#### More golden tests for `PersonIndex`

My internet is terrible since I'm just using LTE while waiting for both
electricity and the fiber internet to come back. Emphasis on terrible. So I
downloaded some of the JSON responses for some pages of `/people/`, decoded it
to `PersonIndex`, vomited it to a file, and compared said file to a golden file.
I did this because there are a lot of items per page and swapi.dev does not have
a limit option; I did not want to manually write everything down and maintain it
in the test files so I figured, why not just use golden tests for this? I don't
know how good of an idea my approach is, because I just used
`show :: PersonIndex -> String`. Something something don't use `show` for
serialization but it's ok for this case... I think?

More about golden tests:

I saved the JSON responses for each page in `./testdata/fixtures/person_index`,
with `n.json` (`n` being the page number). I could've manually written a test
case for each file but that's cumbersome. Fortunately, `tasty-golden` has something
for that:

```haskell
findByExtension
  :: [FilePath]     -- List of extensions. e.g [ ".json" ]
  -> FilePath       -- Directory you want to find matches for
  -> IO [FilePath]  -- Result!
```

But I also wanted to be able to name the tests that I generate based on the file
name, that way they won't be muddied together, and I can figure out which file
fails.

``` haskell
test_decodePersonIndices :: IO [TestTree]
test_decodePersonIndices =
  map mkGoldenTest <$> personIndices
  where
    personIndices :: IO [FilePath]
    personIndices =
      Tasty.Golden.findByExtension [".json"] "./testdata/fixtures/person_index"

    mkGoldenTest :: FilePath -> TestTree
    mkGoldenTest filepath =
      let baseName :: FilePath
          baseName = takeBaseName filepath
      in
        Tasty.Golden.goldenVsFile
          ("decode page " <> baseName)
          ("./testdata/person_index/" <> baseName <> ".golden")
          ("./testdata/person_index/" <> baseName <> ".data")
          (readFile filepath >>= (
            \personIndexJSON ->
              let personIndex =
                    Aeson.eitherDecodeStrict @PersonIndex personIndexJSON
              in
                writeFile
                  ("./testdata/person_index/" <> baseName <> ".data")
                  (show personIndex)) . ByteString.pack)

takeBaseName :: FilePath -> FilePath
takeBaseName =
  Text.unpack . head . Text.split (== '.') . last . Text.split (== '/') . Text.pack
```

I should probably use a strict version of `readFile`, but I haven't read up on
the problems of lazy IO yet, so I'll leave it for now.

### Day 14 - 14/01/2022

I'm planning on moving this into a blog but I haven't gotten around on
setting one up yet. Hopefully I'll be able to make each day more detailed rather
than just overviews of what happened. So going deeper into the code and all that
stuff. I'm not sure when that will happen, but I guess if I want to get a job,
I'll need one.

#### Golden test failing due to EOF

This one was an annoying one. I wasn't sure what I was doing wrong prior to
figuring it out, but wow, I feel extremely irate that this was simpler than
expected, and quite obvious too but I ignored my hunch.

So turns out `emacs` autoinserts line endings every time you save a file, which
is normally harmless and actually convenient. But `tasty-golden`, specifically
`Tasty.Golden.goldenVsFile` (`*VsString` as well) takes EOF seriously and treats
them as different things if you compare a file with EOF to a file without EOF.
Fair I guess, but still annoying.

Problem is, I tried making it more consistent in both files with the use of
different text editors like vim, but for some reason, they render it the same
way. I don't know what's up with this; it doesn't show a next line to move the
cursor to in the golden file, unlike emacs.

Easiest solution: Delete golden file, copy test file and rename it as golden
file. It's exactly the same in terms of contents anyway.

Edit: Maybe the term is not EOF but something else that I'm not sure of.

#### Incorrect field-value pairings when decoding root JSON

After working on the golden test, I worked on a spec test for decoding to `Root`.
Then I got some errors saying that it doesn't match, but I noticed that the
field-value pairs are incorrect! For instance, `https://swapi.dev/api/people/`
is supposed to be bounded to `rPeople`, but was instead bounded to some other
field of `Root`. Turns out I just forgot to rearrange it in the right order, since
I previously rearranged the object field order when encoding `Root`.

``` haskell
instance FromJSON (Root :: Type) where
  parseJSON :: Value -> Parser Root
  parseJSON =
    Aeson.withObject "Root" $
      \rootObj ->
        Root
          <$> rootObj .: "films"
          <*> rootObj .: "people"
          <*> rootObj .: "planets"
          <*> rootObj .: "species"
          <*> rootObj .: "starships"
          <*> rootObj .: "vehicles"

instance ToJSON (Root :: Type) where
  toJSON :: Root -> Value
  toJSON root =
    Aeson.object
      [ "films"     .= rFilms root
      , "people"    .= rPeople root
      , "planets"   .= rPlanets root
      , "species"   .= rSpecies root
      , "starships" .= rStarships root
      , "vehicles"  .= rVehicles root
      ]
```

Now I got these two simple tests for `Root`! I was able to catch a mistake that
I just introduced a commit ago thanks to one test case. Awesome.

### Day 13 - 15/12/2021

I wrote more tests for the `Url` module, and discovered some bugs in the process.
I am happy I decided to write tests because without it, I would probably be
frustrated in the future.

Turns out there was an issue in the parsing of `UrlData` to a url. Here's the
old version of `urlDataToUrl :: UrlData -> Text`:

``` haskell
urlDataToUrl :: UrlData -> Text
urlDataToUrl urlData =
  mconcat
    [ baseUrl
    , mconcat . udSubdir $ urlData
    , paramsToUrlParams . udParams $ urlData
    ]
  where
    ...
```

#### Issue #1: It didn't separate and end the subdirectories with `/`

I must've been extremely sleep deprived when I wrote this because there's no way
`mconcat . udSubdir $ urlData` would've worked that way. Anyway, because `baseUrl`
always ends with a `/`, I'm only concerned with interspersing and ending the
subdirectories with `/`.

I decided to use `foldl'`, not `foldr` since I don't have to deal with infinite
data, which I had to make a function for it.


``` diff
urlDataToUrl :: UrlData -> Text
urlDataToUrl urlData =
  mconcat
    [ baseUrl
-   , mconcat . udSubdir $ urlData
+   , subdirToUrlSubdir . udSubdir $ urlData
    , paramsToUrlParams . udParams $ urlData
    ]
  where
+   subdirToUrlSubdir :: [Text] -> Text
+   subdirToUrlSubdir = mconcat . reverse . foldl' (\acc el -> "/":el:acc) []
```

The reason for prepending the next element to the list and then reversing it is
because I didn't want to spend O(n) in each iteration. This way it only happens
during the reverse and the concatenation.

#### Issue #2: I got the logic reversed for parsing URL parameters

This one was also pretty stupid. I have this other helper function in 
`urlDataToUrl`:

``` diff
    paramsToUrlParams :: Map Text Text -> Text
    paramsToUrlParams params
-     | Map.null params =
-       Map.foldlWithKey'
-         (\acc key val -> acc <> key <> "=" <> val)
-         mempty
-         params
-     | otherwise = mempty
+     | Map.null params = mempty
+     | otherwise =
+         Map.foldlWithKey'
+           (\acc key val -> acc <> key <> "=" <> val)
+           mempty
+           params
```

If there are no parameters (empty map), then it's gonna perform the fold, which
is supposed to be for when there are parameters. So yeah, just a simple swap
would do just fine.

#### Issue #3: What about `?` and `&`?

Yep, I forgot about those as well. Fortunately, not too difficult to do.

``` diff
    paramsToUrlParams :: Map Text Text -> Text
    paramsToUrlParams params
      | Map.null params = mempty
      | otherwise =
-         Map.foldlWithKey'
-           (\acc key val -> acc <> key <> "=" <> val)
            mempty
            params
      | Map.null params = mempty
      | otherwise =
+       Text.cons '?' $
+         Text.drop 1 $
+           Map.foldlWithKey'
+             (\acc key val -> acc <> "&" <> key <> "=" <> val)
              mempty
              params
```

#### Issue #4: `[""]` if there are no subdirectories

There was also an issue with `urlToUrlData`. 

``` haskell
urlToUrlData :: Text -> Maybe UrlData
urlToUrlData strippedUrl = do
  urlDataText <- Text.stripPrefix baseUrl strippedUrl

  let (subdirText:paramsText) = Text.splitOn "/?" urlDataText

      subdirs :: [Text]
      subdirs = Text.split (== '/') . Text.dropWhileEnd (== '/') $ subdirText

      params :: Map Text Text
      params = Map.fromList $
        map parseKeyValue $
          concatMap (Text.split (== '&')) paramsText

  pure (UrlData { udSubdir = subdirs, udParams = params })
```

e.g `https://swapi.dev/api/` (I treated this as a base URL), it would end up with
`UrlData { udSubdir = [""], udParams = Map.empty }`. The `[""]` being useless.

`subdirText` is essentially the subdirectories encoded as `Text`. If there are
no subdirectories, then, well, it's going to be `""`. If I parse it 
(look at `subdirs :: [Text]`) then the entire thing is going to end with `[""]`
which makes sense. So a simpler way would be to just first check if what I'm
dealing with is an empty text, and if it is I just give back an empty list.

``` diff haskell
subdirs :: [Text]
- subdirs = Text.split (== '/') . Text.dropWhileEnd (== '/') $ subdirText
+ subdirs =
+   if Text.null subdirText
+   then []
+   else Text.split (== '/') . Text.dropWhileEnd (== '/') $ subdirText
```

Lots of issues in general that I caught with pretty simple tests. Convenient.
This is just the `Url` module though. I wonder what other bugs I'll discover
in the instances.

### Day 12 - 14/12/2021

I'm gonna spend this day writing tests for the existing code. Parts I want to
test are:

* `Url` module
* `aeson` instances

I think it would be cool if I could use `SmallCheck` to handle most of these things
since it would save a lot of time. I have to read on it though.

- I refactored `getId` to use `urlToUrlData`. So now I don't have any janky 
pattern matching, and it's much easier to understand what's happening thanks to
`UrlData` :).
- I decided to change `getId` to evaluate to a `Maybe Int` rather than an 
`Either String Int` just so that I could compose things better. This *might* be
a terrible idea because I lose out on some important errors encoded in `String`.
- Wrote some test cases for `resourceUrl` and `getId`. It's a bit of a pain to
deal with `getId` so I added a note to refactor it with property-based testing.
I used [this](https://github.com/sshine/hs-jq) to check how to work with `Hspec`
with `tasty-hspec`, and it turns out I had to add the `hspec` library too since
it doesn't reexport the types and functions for me. Not that inconvenient though.
- I'm thinking twice about using `SmallCheck` but unfortunately, `validity` is
not a `tasty` provider. So I still have to compare between `SmallCheck` and
`Hedgehog`.

### Day 11 - 11/12/2021

- Did some digging around for the `tasty` providers I'll need. I think 
`tasty-hunit`, and `tasty-golden` are good for now.
- Added `tasty-discover` to make test files less of a pain to manage. Seems quite
similar with `hspec-discover`.
- I just copied the sample test cases from `tasty`'s docs. I'll be replacing it
with my own soon.
- I'll probably start testing the `Url` module first since it's pretty tiny.

### Day 10 - 08/12/2021

- Decided to eventually experiment with deriving `aeson` instances with 
`Generic`, but only for anything related with the `Vehicle` resource. Since I 
still want to practice the basics more.
- I've decided to use `tasty` over `sandwich` just because the former has 
golden tests. Golden tests would be useful for the JSON output in this case since
it's a bad idea to all that in a source file. Also, pretty annoying to modify
if that's the case.
- I *may* include `QuickCheck` but I still have to think for what cases I would
use it for. Although I do see the value in generating tests just based on the
properties I state.

Not really doing much today because this day was intended to be a day off.

### Day 9 - 06/12/2021

- Turns out I missed a few cases of colors
- I wanted an easier time in getting information out of a URL. Manually parsing 
it every time wasn't that convenient so I opted for a new type called `UrlData`
with the following definition:

``` haskell
data UrlData = UrlData
  { udSubdir :: [Text]
  , udParams :: Map Text Text
  }
```

The two key things I noticed I needed a lot was the subdirectory and the URL
parameters. 

#### `udSubdir`

The subdirectory is whatever follows the base URL which in this case
is anything after `https://swapi.dev/api/`. e.g `people/1/` gets parsed into 
`["people", "1"] :: [Text]`. Not the _most_ convenient since I still have to
potentially convert the person ID into a numeric type but I think it's good for
whatever I need it for.

#### `udParams`

This is useful when trying to get/encode information about, well, URL params.
In SWAPI there are two that I noticed (so far), which was `search` for any search
query, and `page` for paginating any indexing/searching of a resource. 

`?search=r2&page=1` gets parsed into `Map.fromList [("search" "r2"), ("page", "1")]`
. Pretty cool.

Having this domain type makes it easier to deal with this sort of thing. I don't
have to care about the base URL. I also don't need to care about how the URL is
structured. I only care about the data I get from it.

### Day 8 - 03/12/2021

- ~~Same thing as some of the fields, I added a `PersonId` newtype with some
smart constructors to ensure that it's at least `> 0`.~~
- ~~I changed the smart constructor check from `>= 0` to `> 0` cause I just found
out that it actually starts with `1`.~~ I thought it would be a better idea to 
expose the data constructors for IDs instead since it would be kinda tedious to 
deal with once the HTTP requests get implemented. I don't think ID really has to 
be validated since it's possible for one to have a negative ID (I think). 
- Added `edited` and `created` fields. So now it converts an ISO8601 UTC timestamp
into the appropriate domain type
- Added `Root` although I'm not sure what this is used for because I've already
added data constructors for the different resources. So, just parsing it as if
the URLs were text.
- I forgot about the index endpoint so that's what I'm working on right now. I 
think it would be cool if I could parse all the URL attributes. So I wouldn't 
have to worry if a param goes first or not. All I have to check is if the 
parameter exists in a map. Maps might be ideal for storing URL attributes in 
this case. I'll think about it for now.
- Made a really ugly solution for parsing a URL. I needed to get the subdir,
and query params. I didn't want to have to use a library for it because:

  1. I have no idea how to use it. Yet another thing to learn.
  2. The use case is a bit simple. 
  
But did it in the end (somewhat) and so here it is:

``` haskell
data UrlData = UrlData
  { udSubdir :: [Text]           -- Contains the list of subdirectories in path.
  , udParams :: Map Text Text    -- Contains query parameters as KV pairs .
  }
  deriving Show

-- In ghci
λ> mkUrlData "https://swapi.dev/api/people/1/?search=r2d2"
Just (UrlData {udSubdir = ["people","1"], udParams = fromList [("search","r2d2")]})

-- But it's not perfect. I'm supposed to URL encode some characters!
λ> mkUrlData "https://swapi.dev/api/people/1/?search=&r2d2"
Just (UrlData {udSubdir = ["people","1"], udParams = fromList [("r2d2",""),("search","")]})
```

I don't really care about the base URL, so I don't *need* that information. But,
I do need the subdirectory and the query parameters especially for down the road
when search and paging are to be implemented. 

Maybe I do need that URL library. Damn it.

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

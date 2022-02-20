module SwoogleWeb.Views.People.Show where

--------------------------------------------------------------------------------

import Data.Coerce                  (coerce)
import Data.Text                    (Text)
import Lucid                        (Html, class_, div_)

--------------------------------------------------------------------------------

import Swapi.Resource.Person        (Person (pName), PersonName (PersonName))
import Swoogle.SearchData           (SearchData (SearchData, sdPage, sdQuery, sdResource))
import SwoogleWeb.Components.Search qualified as Search (searchBar)

--------------------------------------------------------------------------------

content :: Person -> Html ()
content person = do
  div_ [class_ "w-full sm:w-2/3 py-6 flex flex-col mx-auto"] $ do
    Search.searchBar searchData

  where
    searchData :: SearchData
    searchData =
      SearchData
        { sdQuery = coerce @PersonName @Text $ pName person
        , sdPage = 1
        , sdResource = "people"
        }

module SwapiWeb.Views.Home where

import Lucid

content :: Html ()
content = do
  p_ [class_ "text-red-500 bg-blue-100"] "I am Home page"

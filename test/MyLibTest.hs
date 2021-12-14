module MyLibTest
  ( test_Sample
  ) where

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty (testGroup)
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit (testCase)

--------------------------------------------------------------------------------

test_Sample :: TestTree
test_Sample = Tasty.testGroup "Tests"
  [ HUnit.testCase "2 + 2 = 4" $
      2 + 2 @?= (4 :: Integer)
  ]

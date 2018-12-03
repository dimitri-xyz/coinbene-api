import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $
  testCase "\nExample test case" $ do
    -- assertion no. 1 (passes)
    2 + 2 @?= 4
    -- assertion no. 2 (fails)
    assertBool "the list is not empty" $ null [1]
    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    "foo" @?= "bar"

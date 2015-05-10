module HangmanHspec (hangmanHspec) where

import Hangman

import Test.Hspec
import Test.QuickCheck

hangmanHspec :: Spec
hangmanHspec = do
  describe "sd" $ do
    it "sd"
       (showHangmanWord (newHangmanWord "mad") `shouldBe` "---")
  describe "sd" $
    it "sd"
       (property $ \word -> showHangmanWord (newHangmanWord word) == replicate (length word) '-')

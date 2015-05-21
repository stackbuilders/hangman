-- |
--
-- A game for two in which one player tries to guess the letters of a
-- word, and failed attempts are recorded by drawing a gallows and
-- someone hanging on it, line by line.

module Hangman where

import Data.Maybe (fromMaybe)

----------------------------------------------------------------------
-- (Impure) hangman games
----------------------------------------------------------------------

-- | Play hangman games.

playHangman :: Maybe Int -> String -> IO ()
playHangman maybeLives word =
  playHangmanGame (newHangmanGame maybeLives word)

playHangmanGame :: HangmanGame -> IO ()
playHangmanGame currentGame = do
  print currentGame
  putStr "Have you guessed the riddle yet? "
  letters <- getLine
  let nextGame = nextHangmanGame currentGame letters
  case hangmanGameStatus nextGame of
    Playing _ -> playHangmanGame nextGame
    _         -> print nextGame

----------------------------------------------------------------------
-- (Pure) hangman games
----------------------------------------------------------------------

-- | A hangman game.

data HangmanGame = HangmanGame
  { hangmanGameStatus :: HangmanGameStatus -- ^
  , hangmanGameWord   :: HangmanWord       -- ^
  }

instance Show HangmanGame where
  show = showHangmanGame

defaultHangmanGameLives :: Int
defaultHangmanGameLives = 6

nextHangmanGame :: HangmanGame -> [Char] -> HangmanGame
nextHangmanGame = foldl nextHangmanGame'

nextHangmanGame' :: HangmanGame -> Char -> HangmanGame
nextHangmanGame' game@(HangmanGame status word) letter =
  case nextHangmanWord word letter of
    Nothing ->
      game {hangmanGameStatus =
               case status of
                 Playing 1     -> Lost
                 Playing lives -> Playing (lives - 1)
                 otherStatus   -> otherStatus}
    Just nextWord ->
      game {hangmanGameWord   = nextWord
           ,hangmanGameStatus =
             if any not (map snd nextWord) then status else Won}

newHangmanGame :: Maybe Int -> String -> HangmanGame
newHangmanGame maybeLives word =
  HangmanGame (Playing (fromMaybe defaultHangmanGameLives maybeLives))
              (newHangmanWord word)

showHangmanGame :: HangmanGame -> String
showHangmanGame (HangmanGame Lost   word) =
  fmap fst word        ++ " (" ++ show Lost   ++ ")"
showHangmanGame (HangmanGame status word) =
  showHangmanWord word ++ " (" ++ show status ++ ")"

----------------------------------------------------------------------
-- Hangman game statuses
----------------------------------------------------------------------

-- | A hangman game status.

data HangmanGameStatus
  = Lost               -- ^
  | Playing Int        -- ^
  | Won                -- ^

instance Show HangmanGameStatus where
  show Lost            = "Off with their heads!"
  show (Playing 1)     = "I'll fetch the executioner myself... Lives: 1"
  show (Playing lives) = "Are their heads off? Lives: " ++ show lives
  show Won             = "Keep your temper..."

----------------------------------------------------------------------
-- Hangman words
----------------------------------------------------------------------

-- | A hangman word.

type HangmanWord = [(Char,Bool)]

-- |
--
-- >>> nextHangmanWord [('m',False),('a',False),('d',False)] 'a'
-- Just [('m',False),('a',True),('d',False)]
--
-- >>> nextHangmanWord [('m',False),('a',True),('d',False)] 'e'
-- Nothing
--
-- >>> nextHangmanWord [('m',False),('a',True),('d',False)] 'a'
-- Just [('m',False),('a',True),('d',False)]

nextHangmanWord
  :: HangmanWord       -- ^
  -> Char              -- ^
  -> Maybe HangmanWord -- ^
nextHangmanWord = flip matchLetterInHangmanWord

matchLetterInHangmanWord :: Char -> HangmanWord -> Maybe HangmanWord
matchLetterInHangmanWord letter word =
  matchHangmanWord' letter word False []

matchHangmanWord'
  :: Char              -- ^
  -> HangmanWord       -- ^
  -> Bool              -- ^
  -> HangmanWord       -- ^
  -> Maybe HangmanWord -- ^

matchHangmanWord' _      [] False  _           = Nothing
matchHangmanWord' _      [] True   matchedWord = Just (reverse matchedWord)
matchHangmanWord' letter ((l,guessed):ls) matched matchedWord =
  if l == letter
     then matchHangmanWord' letter ls True    ((l,True):matchedWord)
     else matchHangmanWord' letter ls matched ((l,guessed):matchedWord)

-- |
--
-- >>> newHangmanWord "mad"
-- [('m',False),('a',False),('d',False)]
--
-- prop> newHangmanWord word == fmap (\c -> (c,False)) word

newHangmanWord :: String -> HangmanWord
newHangmanWord = fmap (\letter -> (letter,False))

-- |
--
-- >>> showHangmanWord (newHangmanWord "mad")
-- "---"
--
-- >>> showHangmanWord (newHangmanWord "hatter")
-- "------"
--
-- prop> showHangmanWord (newHangmanWord word) == replicate (length word) '-'
--
-- >>> showHangmanWord [('m',True),('a',True),('d',True)]
-- "mad"
--
-- prop> showHangmanWord (fmap (\c -> (c,True)) word) == word
--
-- >>> showHangmanWord [('m',False),('a',True),('d',False)]
-- "-a-"

showHangmanWord :: HangmanWord -> String
showHangmanWord =
  fmap (\(letter,guessed) -> if guessed then letter else '-')

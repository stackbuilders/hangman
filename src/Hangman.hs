-- |
--
-- A game for two in which one player tries to guess the letters of a
-- word, and failed attempts are recorded by drawing a gallows and
-- someone hanging on it, line by line.

module Hangman (playHangman) where

import Data.Maybe (fromMaybe)
import Prelude hiding (words)

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
  let nextGame = reduceHangmanGame currentGame letters
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
defaultHangmanGameLives = 5

reduceHangmanGame :: HangmanGame -> String -> HangmanGame
reduceHangmanGame = foldl nextHangmanGame

nextHangmanGame :: HangmanGame -> Char -> HangmanGame
nextHangmanGame currentGame@(HangmanGame status word) letter =
  case nextHangmanWord word letter of
    Nothing ->
      currentGame
        {hangmanGameStatus =
           case status of
             Playing 1     -> Lost
             Playing lives -> Playing (lives - 1)
             otherStatus   -> otherStatus}
    Just nextWord ->
      currentGame
        {hangmanGameWord = nextWord
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
  deriving Eq

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

nextHangmanWord :: HangmanWord -> Char -> Maybe HangmanWord
nextHangmanWord word c = match False word []
  where
    match :: Bool -> HangmanWord -> HangmanWord -> Maybe HangmanWord
    match b [] nextWord = if b then Just (reverse nextWord) else Nothing
    match b ((sd,b'):rest) hw =
      if sd == c
         then match True rest ((sd,True):hw)
         else match b    rest ((sd,b'):hw)

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

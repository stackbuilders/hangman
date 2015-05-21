-- | TODO.

module Hangman
  (-- * Sd
   -- ** Hangman games
   HangmanGame(..)
  ,defaultHangmanGameLives
  ,newHangmanGame
   -- ** Hangman statuses
  ,HangmanGameStatus(..)
   -- ** Hangman words
  ,HangmanWord
  ,newHangmanWord
   -- * Sd
  ,nextHangmanGame
  ,nextHangmanWord
  ,showHangmanWord
   -- * Sd
  ,playHangman
  ,playHangmanGame)
  where

import Data.Maybe (fromMaybe)

----------------------------------------------------------------------
-- (Impure) hangman games
----------------------------------------------------------------------

-- | Play hangman games.

playHangman :: Maybe Int -> String -> IO ()
playHangman maybeLives word =
  playHangmanGame (newHangmanGame maybeLives word)

-- | TODO.

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

-- | TODO.

defaultHangmanGameLives :: Int
defaultHangmanGameLives = 6

-- | TODO.

nextHangmanGame
  :: HangmanGame -- ^ TODO
  -> [Char]      -- ^ TODO
  -> HangmanGame -- ^ TODO
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

-- | TODO

newHangmanGame
  :: Maybe Int   -- ^ TODO
  -> String      -- ^ TODO
  -> HangmanGame -- ^ TODO
newHangmanGame maybeLives word =
  HangmanGame (Playing (fromMaybe defaultHangmanGameLives maybeLives))
              (newHangmanWord word)

-- | TODO

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
  = Lost               -- ^ TODO
  | Playing Int        -- ^ TODO
  | Won                -- ^ TODO

instance Show HangmanGameStatus where
  show = showHangmanGameStatus

showHangmanGameStatus :: HangmanGameStatus -> String
showHangmanGameStatus Lost            = "Off with their heads!"
showHangmanGameStatus (Playing 1)     =
  "I'll fetch the executioner myself... Lives: 1"
showHangmanGameStatus (Playing lives) =
  "Are their heads off? Lives: " ++ show lives
showHangmanGameStatus Won             =
  "Keep your temper..."

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
  :: HangmanWord       -- ^ TODO
  -> Char              -- ^ TODO
  -> Maybe HangmanWord -- ^ TODO
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

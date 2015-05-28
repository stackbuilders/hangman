# Hangman: Imperative Functional Programming

> In short, Haskell is the world's finest imperative programming
> language.

---Simon Peyton Jones

## Introduction

```
$ ghci
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
Prelude> :set prompt "ghci> "
ghci>
```

## Reverse

```
ghci> reverse []
[]
```

```
ghci> reverse [104,97,115,107,101,108,108]
[108,108,101,107,115,97,104]
```

```
ghci> reverse "haskell"
"lleksah"
```

```
ghci> ['h','a','s','k','e','l','l']
"haskell"
```

```haskell
ghci> :type reverse
reverse :: [a] -> [a]
```

```
ghci> :info []
data [] a = [] | a : [a] 	-- Defined in ‘GHC.Types’
...
```

```haskell
reverse []     = []
```

```haskell
reverse (x:xs) = reverse xs ++ [x]
```

## IO

```
ghci> :type putStrLn
putStrLn :: String -> IO ()
```

```
ghci> putStrLn (reverse "haskell")
lleksah
```

```haskell
main :: IO ()
main = putStrLn (reverse "haskell")
```

```
ghci> :t getLine
getLine :: IO String
```

```haskell
main :: IO ()
main = do
  line <- getLine
  putStrLn (reverse line)
```

```haskell
type IO a = World -> (a,World)
```

## Hangman

```haskell
data HangmanGame = HangmanGame
  { hangmanGameStatus :: HangmanGameStatus
  , hangmanGameWord   :: HangmanWord
  }
```

```haskell
data HangmanGameStatus
  = Lost
  | Playing Int
  | Won
```

```haskell
type HangmanWord = [(Char,Bool)]
```

```haskell
nextHangmanGame :: HangmanGame -> [Char] -> HangmanGame
nextHangmanGame = ...
```

```haskell
nextHangmanWord :: HangmanWord -> Char -> Maybe HangmanWord
nextHangmanWord = ...
```

```
ghci> :info Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Base’
...
```

```haskell
playHangmanGame :: HangmanGame -> IO ()
playHangmanGame = ...
```

```haskell
main :: IO ()
main = do
  ...
  playHangmanGame (HangmanGame ...)
  ...
```

## Conclusion

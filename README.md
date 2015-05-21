# Hangman: Off with their heads!

[![Build Status][build-status-image]][build-status]

## Game

Hangman is “a game for two in which one player tries to guess the
letters of a word, and failed attempts are recorded by drawing a
gallows and someone hanging on it, line by line.”

- Install the dependencies

  ```
  cabal sandbox init
  cabal install --only-dependencies --enable-tests
  ```

  ```
  cabal install doctest
  cabal install hspec
  ```

- TODO

  ```
  cabal configure --enable-tests
  cabal run
  ```

- Run all tests in the test suites

  ```
  cabal test
  ```

  ```
  cabal test doctests
  ```

  ```
  cabal test hspecs
  ```

## Posts

TODO

## Slides

TODO

## Tutorial

TODO

## Words

TODO

## Bibliography

- Bird, Richard (2014). Thinking Functionally with Haskell. Cambridge
  University Press.

- Contorer, Aaron (2013). Haskell, the Language Most Likely to Change
  the Way You Think About Programming.

- Lipovača, Miran (2011). Learn You a Haskell for Great Good! No
  Starch Press.

- Marlow, Simon, ed. (2010). Haskell 2010 Language Report.

- Marlow, Simon and Simon Peyton Jones (2012). The Glasgow Haskell
  Compiler. In: The Architecture of Open Source Applications (Volume
  II).

- O'Sullivan, Bryan, John Goerzen, and Don Stewart (2008). Real World
  Haskell. O'Reilly Media.

- Peyton Jones, Simon (2010). Tackling the Awkward Squad.

- Yorgey, Brent (2013). CIS 194: Introduction to Haskell (Spring
  2013).

- Webster's Dictionary (1913).

[build-status]: https://travis-ci.org/stackbuilders/hangman-off
[build-status-image]: https://travis-ci.org/stackbuilders/hangman-off.svg?branch=master

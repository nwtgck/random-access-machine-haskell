# random-access-machine
[![Build Status](https://travis-ci.com/nwtgck/random-access-machine-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=develop)](https://travis-ci.com/nwtgck/random-access-machine-haskell)

Random Access Machine written in Haskell

## Example

```hs
module Main where

import RandomAccessMachine
import Control.Monad.State

sub1Prog = Program
  [ J(1, 2, 5)
  , S(2)
  , S(0)
  , J(0, 0, 1)
  , E
  ]

main :: IO ()
main = do
  let (res, env) = execProgram sub1Prog [7, 4]   -- 7 - 4
  print res
  -- => 3
  let (res, env) = execProgram sub1Prog [87, 23] -- 87 - 23
  print res
  -- 64
```

## Run Example

```bash
stack build && stack exec random-access-machine-examples
```
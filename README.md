# random-access-machine
[![Build Status](https://travis-ci.com/nwtgck/random-access-machine-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=develop)](https://travis-ci.com/nwtgck/random-access-machine-haskell)

Random Access Machine written in Haskell

## Instructions
This Random Access Machine has 5 instructions.

### `Z(i)`
Set `0` to register `R(i)`

### `S(i)`
Increment register `R(i)` by `1`

### `M(i,j)`
Assign value of `R(j)` to `R(i)`

### `J(i,j,k)`
Jump to `k`th instruction if value of `R(i)` and `R(j)` are equal
(NOTE: instruction is one origin)

### `E`
Halt program

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
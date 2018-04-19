module Main where

import RandomAccessMachine
import Control.Monad.State

program1 = Program
  [ E
  ]

sub1Prog = Program
  [ J 1 2 5
  , S 2
  , S 0
  , J 0 0 1
  , E
  ]


main :: IO ()
main = do
  let (res, env) = execProgram sub1Prog [7, 4] -- 7 - 4
  print res
  let (res, env) = execProgram sub1Prog [87, 23] -- 87 - 23
  print res

module Main where

import RandomAccessMachine
import Control.Monad.State

program1 = Program
  [ E
  ]

sub1Prog = Program
  [ J(1, 2, 5)
  , S(2)
  , S(0)
  , J(0, 0, 1)
  , E
  ]

mulProg = Program
  [ J (2, 3, 9)
  , Z (4)
  , J(1, 4, 7)
  , S (0)
  , S (4)
  , J (0, 0, 3)
  , S (3)
  , J (0, 0, 1)
  , E
  ]

maxProg = Program
  [ M (3, 1)
  , M (4, 2)
  , J (3, 2, 8)
  , J (4, 1, 10)
  , S (3)
  , S (4)
  , J (0, 0, 3)
  , M (0, 2)
  , E
  , M (0, 1)
  , E
  ]

main :: IO ()
main = do
  putStr "7 - 4 = "
  let (res, env) = execProgram sub1Prog [7, 4]
  print res
  putStr "87 - 23 = "  
  let (res, env) = execProgram sub1Prog [87, 23]
  print res
  putStr "2 * 3 = "  
  let (res, env) = execProgram mulProg [2, 3]
  print res

  putStr "max(2, 3) = "  
  let (res, env) = execProgram maxProg [2, 3]
  print res
  putStr "max(0, 0) = "  
  let (res, env) = execProgram maxProg [0, 0]
  print res
  putStr "max(0, 1) = "  
  let (res, env) = execProgram maxProg [0, 1]
  print res
  putStr "max(100, 1) = "  
  let (res, env) = execProgram maxProg [100, 1]
  print res

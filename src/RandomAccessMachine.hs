{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module RandomAccessMachine where

import Control.Monad.State
import Numeric.Natural
import Debug.Trace

-- Register number
newtype RegNum = RegNum Natural
  deriving (Show, Eq, Num, Real, Ord, Enum, Integral)

-- Instruction number
newtype InstrNum = InstrNum Natural
  deriving (Show, Eq, Num, Real, Ord, Enum, Integral)

-- | Instruction
data Instr = 
    Z RegNum
  | S RegNum
  | M RegNum RegNum
  | J RegNum RegNum InstrNum
  | E
  deriving (Show, Eq)

-- | Program
newtype Program = Program [Instr]
  deriving (Show, Eq)

-- | Register
newtype Register = Register [Natural]
  deriving (Show, Eq)

-- | Environement
data Env =
  Env
  { pc           :: InstrNum -- Program counter
  , register     :: Register -- Regisiter
  , isTerminated :: Bool     -- Whether terminated or not
  }
  deriving (Show, Eq)

-- | Initial enviromemnt
initEnv :: [Natural] -> Env
initEnv args = 
  Env
  { pc           = 1
  , register     = Register (0 : args ++ repeat 0)
  , isTerminated = False
  }

-- | Random Access Machine
newtype RandomAccessMachineT m a = 
  RandomAccessMachineT
  { runRandomAccessMachineT :: StateT Env m a
  }
  deriving (Functor, Applicative, Monad, MonadState Env, MonadTrans)

-- | Set nat to regNum
setRegister :: Monad m => RegNum -> Natural -> RandomAccessMachineT m ()
setRegister regNum nat = do
  -- Calc index
  let idx :: Int
      idx = (fromInteger . toInteger) regNum
  -- Set nat to regNum
  modify (\env@Env{register=Register naturals} ->
           let newRegL = take idx     naturals
               newRegR = drop (idx+1) naturals
            in env{register=Register (newRegL ++ [nat] ++ newRegR)}
         )


-- | Get nat by regNum
getNatFromRegister :: Monad m => RegNum -> RandomAccessMachineT m Natural
getNatFromRegister regNum = do
  -- Calc index
  let idx :: Int
      idx = (fromInteger . toInteger) regNum
  Register naturals <- gets register
  -- Return natural
  return (naturals !! idx)

-- | Increment program counter
incPc :: Monad m => RandomAccessMachineT m ()
incPc = modify (\env@Env{pc} -> env{pc=pc+1})

-- | Run an instruction
runInstr :: Monad m => Instr -> RandomAccessMachineT m ()
runInstr (Z regNum) = setRegister regNum 0 >> incPc
runInstr (S regNum) = do
  nat <- getNatFromRegister regNum
  setRegister regNum (nat + 1)
  incPc
runInstr (M regNum1 regNum2) = do
  nat <- getNatFromRegister regNum2
  setRegister regNum1 nat
  incPc
runInstr (J regNum1 regNum2 instrNum) = do
  n1 <- getNatFromRegister regNum1
  n2 <- getNatFromRegister regNum2
  if n1 == n2
    then modify (\env -> env{pc=instrNum})
    else incPc
runInstr (E) = modify (\env -> env{isTerminated=True})


-- | Make random access machine
makeRandomAccessMachine :: Monad m => Program -> RandomAccessMachineT m Natural
makeRandomAccessMachine (Program instrs) = do
  -- Get instruction number
  instrNum <- gets pc
  -- Get index
  let idx = (fromInteger . toInteger) instrNum - 1
  when (idx >= length instrs ) $ 
    error ("Index " ++ show idx ++ ": out of instructions")
  -- Get instruction
  let instr = instrs !! idx
  -- Run the instruction
  runInstr instr
  end <- gets isTerminated
  if end
    then getNatFromRegister 0
    else makeRandomAccessMachine (Program instrs)

-- | Execute a program
execProgram :: Program -> [Natural] -> (Natural, Env)
execProgram program args = 
  runState (runRandomAccessMachineT (makeRandomAccessMachine program)) (initEnv args)

-- TD1 Haskell
module Peano where

-- Nombre de Peano

data Peano  = Zero | Succ Peano deriving (Show, Ord, Eq, Read)

instance Num Peano where 
  a + Zero = a
  a + Succ(b) = Succ(a) + b
  a - Zero = a
  Succ(a) - Succ(b) = a - b
  a * Zero = Zero
  a * Succ(Zero) = a
  a * Succ(b) = (a * b) + a
  signum a = 1
  abs a = a
  fromInteger 0 = Zero
  fromInteger a = Succ(fromInteger(a-1))
                   
                  
instance Enum Peano where
  toEnum a = Zero
  
  fromEnum a = 0

instance Real Peano where
toRational = 0

instance Integral Peano where
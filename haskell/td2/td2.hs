module TD2 where

import Data.Ratio
import Data.List

newtype Prob a = Prob {getProbs :: [(a,Rational)]} deriving (Show)

sameProbability :: [a] -> Prob a
sameProbability l =  Prob (zip l (repeat (1%toInteger(length l))))

instance Functor Prob where
   fmap f (Prob [(a,p)]) = Prob ([(f a,p)])

instance Monad Prob where
  return x = Prob ([(x,1)]) 
  fail s = Prob []
  m >>= f = Prob [(y,p*p') | (x,p) <- getProbs m, (y,p') <- getProbs (f x)]
  
canonize :: Eq a => Prob a -> Prob a
canonize (Prob l) = Prob $ canonize' l  
                where canonize' [] = []
                      canonize' l@((item,_) : _) = 
                        let (identical, different) = partition ((== item) . fst) l
                            s= sum (map snd identical)
                        in (item, s) : canonize' different
                           
-- probability :: Eq a => a -> Prob a -> Rational
-- probability a Prob b = lookup a (getProbs b)
  
dice = sameProbability [1,2,3,4,5,6]
simple = do
  x <- dice
  return $ x < 3

double = do
  x <- dice
  y <- dice
  return $ x == y
  
sick :: Prob Bool
sick = Prob [(True,1%100000),(False,99999%100000)]
       
positive :: Bool -> Prob Bool
positive sick = if sick
                then
                  Prob [(sick,999%1000),(not sick,1%1000)]
                else
                  Prob [(not sick,1%1000),(sick,999%1000)]
                  
results :: Prob Bool
results = renormalize $ do
  isSick <- sick
  isPositif <- positive isSick
  if isPositif
    then 
    return $ isSick
    else
    fail "fail"
    
renormalize :: Prob a -> Prob a
renormalize (Prob l) = Prob [(x, p / total) | (x, p) <- l]
                       where total = sum $ map snd l
  
module Cours1 where
import Prelude 

main = putStrLn "Hello World!"

-- ouvrir mode Haskell : C-C C-L
-- voir : Hoogle
-- load script : :l

facts :: [Integer]
facts = 1 : (zipWith (*) facts [2 ..])

fact :: Int -> Integer
fact = (facts !!)

fibo :: [Integer]
fibo = 1 : 1 : (zipWith (+) fibo (tail fibo))

data B = T | F 

instance Show B where
  show T = "T(rue)"
  show F = "F(alse)"
           
instance Eq B where           
  T == T = True
  F == F = True
  _ == _ = False
  
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
  -- fmap length (Branch (Leaf "bb") (Leaf "aaaaa"))
    
    
-- data Maybe a = Nothing | Just a deriving (Show, Eq)

-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just x) = Just $ f x 
--   --  fmap length "foobar"
-- --  fmap length (Just "foobar")

-- increment :: (Num a, Functor f) => f a -> f a
-- increment = fmap (1+)
-- --  increment (Just 5)
-- --  increment Nothing
-- --  increment [1,2,3]

-- fromJust :: Maybe a -> a
-- fromJust Nothing = error "no value"
-- fromJust (Just x) = x
-- --  fromJust Nothing
-- -- fromJust (Just 17)

-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- bind Nothing _ = Nothing
-- bind (Just x) f = f x

-- class Bindable m where 
--   return :: a -> m a
--   fail :: String -> m a
--   fail = error 
--   (>>=) :: m a -> (a -> m b) -> m b
  
-- instance Bindable Maybe where
--   return x = Just x
--   fail = const Nothing
--   Nothing >>= _ = Nothing
--   (Just x) >>= f = f x
  
-- instance Bindable [] where
--   return x = [x]
--   fail = const []
--   [] >>= _ = []
--   (a : as) >>= f = (f a) ++ (as >>= f)
--   -- (Just 3) >>= return
--   -- [1,2,3] >>= return
--   --  [1,2,3] >>= (\n -> take n $ repeat n)
  
evens l = l >>= (\n -> if (n `mod` 2) == 0 then return n else fail "odd")
  -- evens (Just 2)
-- evens Nothing
-- evens (Just 1)

lengths' :: Monad m => m [a] -> m Int
lengths' x = x >>= (return . length) >>= (return . (1+))
--  lengths' (Just "foobar")

lengths :: Monad m => m [a] -> m Int
lengths x = do
  c <- x
  let l = 1 + length c
  return l

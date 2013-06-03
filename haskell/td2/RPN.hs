module RPN where

import Control.Monad.State
import System.IO 

type Operator s = State [s] ()

binOp :: Integral a => (a->a->a) -> Operator a
binOp f = modify (\(b : a : xs) -> (a `f` b : xs))

parseOp :: (Integral a, Read a) => String -> Operator a
parseOp "+" = binOp (+) 
parseOp "-" = binOp (-)
parseOp "*" = binOp (*)
parseOp "/" = binOp div
parseOp "drop" = modify tail
parseOp "dup" = modify (\(x : xs)->(x:x:xs))
parseOp "swap" = modify (\(a:b:xs)->(b:a:xs))
parseOp "pick" = modify (\(n : xs) -> (xs !! (fromInteger $ toInteger n)) : xs)
parseOp "clear" = put []
parseOp other = modify (read other :)

parse :: (Integral a, Read a) => String -> [Operator a]
parse = (map parseOp) . words 

repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let newstack =  execState (sequence $ parse line) stack
  putStrLn $ show $ reverse newstack
  repl newstack
main = repl []
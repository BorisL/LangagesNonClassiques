module RPN where


import System.IO 
-- Types de base

-- Définition du type Stack comme une liste d'entiers courts
type Stack = [Int]

-- Définition du type Operator comme une fonction 
-- travaillant sur une pile
type Operator = Stack -> Stack   

parsOp :: String -> Operator
parsOp "+" (x1: x2 : xs) =  x1 + x2 : xs
parsOp "-" (x1: x2 : xs) =  x1 - x2 : xs
parsOp "*" (x1: x2 : xs) =  x1 * x2 : xs
parsOp "div" (x1: x2 : xs) =   div x1 x2 : xs
parsOp "dup" (x1: xs) =   x1 : x1 : xs
parsOp "swap" (x1: x2 : xs) =   x2 : x1 : xs
parsOp "drop" (_ : xs) =   xs
parsOp "depth" (x) =  [length (x)] ++ x
parsOp "pick" (x : xs) = [xs!! x] ++ xs
parsOp a (x) =  [(read a) :: Int] ++ x
-- (read "123") :: Int

eval :: Stack -> [Operator] -> Stack
eval x [op] = op x
eval x (op : ops) = eval (op x) ops

parse :: String -> [Operator]
parse x = [parsOp (words x !! 0)]

repl :: Stack -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack
main = repl []

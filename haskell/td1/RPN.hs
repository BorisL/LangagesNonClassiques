module RPN where


import System.IO 
-- Types de base

-- Définition du type Stack comme une liste d'entiers courts
type Stack = [Int]

-- Définition du type Operator comme une fonction 
-- travaillant sur une pile
type Operator = Stack -> Stack   

parseOp :: String -> Operator
parseOp "+" (x1: x2 : xs) =  x1 + x2 : xs
parseOp "-" (x1: x2 : xs) =  x2 - x1 : xs
parseOp "*" (x1: x2 : xs) =  x1 * x2 : xs
parseOp "/" (x1: x2 : xs) =   div x2 x1 : xs
parseOp "dup" (x1: xs) =   x1 : x1 : xs
parseOp "swap" (x1: x2 : xs) =   x2 : x1 : xs
parseOp "drop" (_ : xs) =   xs
parseOp "depth" (x) =  [length (x)] ++ x
parseOp "pick" (x : xs) = [xs!! x] ++ xs
parseOp a (x) =  [(read a) :: Int] ++ x
-- (read "123") :: Int

eval :: Stack -> [Operator] -> Stack
eval x [op] = op x
eval x (op : ops) = eval (op x) ops

parse :: String -> [Operator]
parse x = map parseOp (words x)

repl :: Stack -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack
main = repl []

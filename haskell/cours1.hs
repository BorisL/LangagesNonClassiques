main = putStrLn "Hello World!"

-- ouvrir mode Haskell : C-C C-L

facts :: [Integer]
facts = 1 : (zipWith (*) facts [2 ..])

fact :: Int -> Integer
fact = (facts !!)

fibo :: [Integer]
fibo = 1 : 1 : (zipWith (+) fibo (tail fibo))
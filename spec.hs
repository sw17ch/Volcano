module Main where

data List a = Cons a (List a) | Nil
    deriving (Show)

foo :: List Integer -> Integer
foo list = undefined

check :: [Bool]
check = [check_a, check_b]

check_a, check_b :: Bool
check_a = 6 == foo (Cons 1 (Cons 2 (Cons 3 Nil)))
check_b = 3 == foo (Cons 1 (Cons 1 (Cons 1 Nil)))

add :: Integer -> Integer -> Integer
add = (+)

zero,one :: Integer
zero = 0
one = 1

main = do
    print $ and check

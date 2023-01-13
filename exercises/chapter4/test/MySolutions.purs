module Test.MySolutions where

import Prelude
import Control.Alternative (guard)
import Data.Array ((..), concat, cons, filter, head, null, tail)
import Data.Maybe (fromMaybe)
import Test.Examples

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n = mod n 2 == 0

countEven :: Array Int -> Int
countEven nums =
    if null nums then
        0
    else if (isEven $ fromMaybe 1 $ head nums) then
        1 + countEven (restNumbers nums)
    else
        0 + countEven (restNumbers nums)
    where
        restNumbers :: Array Int -> Array Int
        restNumbers nums = fromMaybe [] $ tail nums

squared :: Array Number -> Array Number
squared nums = map (\n -> n * n) nums

keepNonNegative :: Array Number -> Array Number
keepNonNegative nums = filter (\n -> n >= 0.0) nums

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite nums = (\n -> n >= 0.0) <$?> nums

isPrime :: Int -> Boolean
isPrime num =
    if num == 0 then
        false
    else if num == 1 then
        false
    else
        (length $ factorsV3 num) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
    i <- arr1
    j <- arr2
    pure [ i, j ]

triples :: Int -> Array (Array Int)
triples num = do
    a <- 1 .. num
    b <- a .. num
    c <- b .. num
    guard $ a * a + b * b == c * c
    pure [a, b, c]

primeFactor :: Int -> Array Int
primeFactor num = do
    i <- 2 .. num
    guard $ num `mod` i == 0 && isPrime i
    pure i

primeFactors :: Int -> Array Int
primeFactors num =
    if num == 1 then
        []
    else
        firstFactor `cons` (primeFactors (num / firstFactor))
            where
                firstFactor = fromMaybe num (head $ primeFactor num)

    
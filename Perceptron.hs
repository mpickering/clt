{-# LANGUAGE ViewPatterns #-}


module Main where


import Text.Parsec hiding ((<|>))
import Control.Applicative hiding (many)
import Control.Monad
import Data.List


type Classifier a = a -> Bool

type Example a = a

type Vector = [Double]

main :: IO ()
main = mapM_ (\(x,y) -> do
  print (x,y)
  print =<< (votedPerceptron x y)) (liftM2 (,) [0..9] [0..9])


genp :: ([Vector] -> [Vector] -> Classifier Vector) -> Int -> Int -> IO Double
genp c m n = do
    (posl,postest) <- training 500 <$> readDigits m
    (negl, negtest) <- training 500 <$> readDigits n
    let cv = c posl negl
    return (test cv postest negtest)

perceptron, votedPerceptron :: Int -> Int -> IO Double
votedPerceptron = genp makeVotedPerceptron
perceptron = genp makePerceptron

test :: Classifier Vector -> [Vector] -> [Vector] -> Double
test c ps ns = (testp + testn) / (genericLength ps + genericLength ns)
  where
    testp = genericLength . filter not $ map c ps
    testn = genericLength . filter id $ map c ns



interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) ys = x : interleave ys xs

extend :: Int -> [a] -> [a]
extend n = take n . concat . repeat

training :: Int -> [Vector] -> ([Vector],[Vector])
training = splitAt

makeVotedPerceptron :: [Vector] -> [Vector] -> Classifier Vector
makeVotedPerceptron ps ns c =
  majorityVote (>= 0) $
    map (\v -> dot v c) (genMake (scanr train (repeat 0)) ps ns)

majorityVote :: (a -> Bool) -> [a] -> Bool
majorityVote f xs = if ts > fs then True else False
  where
    (ts, fs) = foldr (\x (p,n) -> if f x then (p+1, n) else (p, n+1)) (0,0) xs :: (Int, Int)

makePerceptron :: [Vector] -> [Vector] -> Classifier Vector
makePerceptron ps ns c = (>= 0) . dot c $ genMake (foldr train (repeat 0)) ps ns

genMake :: ([(Bool, Vector)] -> a) -> [Vector] -> [Vector] -> a
genMake f (zip (repeat True) -> es) (zip (repeat False) -> ces) =
  f (extend 2000 $ interleave es ces)

train :: (Bool, Vector) -> Vector -> Vector
train (l, v) w = if y == l then w else add w (scaler (f y) v)
  where
    f True = -1
    f False = 1
    y = dot v w >= 0

dot :: Vector -> Vector -> Double
dot v w = sum $ zipWith (*) v w

scaler :: Double -> Vector -> Vector
scaler x = map (*x)

add :: Vector -> Vector -> Vector
add = zipWith (+)

readDigits :: Int -> IO [Vector]
readDigits n = do
  parseFile <$> (readFile $ "digits/digit" ++ show n ++ ".txt")

parseFile :: String -> [Vector]
parseFile s = either (error . show) id $ parse (many readVector <* eof) "" s

readVector :: Parsec [Char] () Vector
readVector = replicateM 256 (num <* many whitespace)
  where
    num = read <$> many1 digit
    whitespace = space <|> newline





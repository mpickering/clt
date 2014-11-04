module Main where
import Numeric
import Graphics.Rendering.Chart hiding (label, Point)
import Graphics.Rendering.Chart.Drawing
import Data.Scientific as Scientific
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.List.Split (splitOn)
import Data.Maybe

import Control.Monad
import Data.List
import Control.Arrow
import Control.Applicative
import Debug.Trace
import Data.Function (on)

import Data.Default
import Data.Colour

import Control.Lens

data LabelledPoint a = LabelledPoint {point :: Point, label :: a} deriving (Show)

type Point = (Double, Double)

type Label = Bool

main :: IO()
main = do
  points <- zipWith LabelledPoint <$> (readPoints "trainxy.txt") <*> (readClass "trainc.txt")
  tests <- readPoints ("testxy.txt")
  classification <- readClass "testc.txt"
  let classifier x = classify x points
  mapM_ (\x -> print $ err (classification) (map (classifier x) tests))    [1..10]

  print $ length $ decisionBoundary (classifier 5)

-- Plotting

plotPoints :: AlphaColour Double -> [Point] -> PlotPoints Double Double
plotPoints colour ps =
  def & plot_points_values .~ ps
      & plot_points_style .~ (def & point_color .~ colour)


err :: Eq a => [a] -> [a] -> Double
err xs ys = (genericLength . filter (==True) $ zipWith (/=) xs ys) /  l
  where
    l = genericLength xs

-- Finds what each square would classify as
decisionBoundary :: (Point -> Label) -> [(Point,Label)]
decisionBoundary f = map (\x -> (x, f x)) points
  where
    points = liftA2 (,) [0,0.02 .. 1] [0,0.02 .. 1]

classify :: Int -> [LabelledPoint Label] -> Point -> Label
classify k ls p = knearest k p ls

knearest :: Int -> Point -> [LabelledPoint Label] -> Label
knearest k point =
    majorityVote label
    . take k
    . sortBy (compare `on` distance point)

distance :: Point -> LabelledPoint a -> Double
distance (x,y) (LabelledPoint (x', y') _)=
    sqrt ((x' - x)**2 + (y' - y)**2)


majorityVote :: (a -> Bool) -> [a] -> Bool
majorityVote f xs = if ts > fs then True else False
  where
    (ts, fs) = foldr (\x (p,n) -> if f x then (p+1, n) else (p, n+1)) (0,0) xs :: (Int, Int)


readNumFile :: (String -> a) -> FilePath -> IO [a]
readNumFile parse s = map (parse . dropSpaces) . filter (not. null) . splitOn "\n" <$> readFile s

readClass :: FilePath -> IO [Bool]
readClass = readNumFile (toBool . toRealFloat . read)
  where
    toBool :: Double -> Bool
    toBool n = if n == -1 then False else True

readPoints :: FilePath -> IO [Point]
readPoints  = readNumFile (mapTuple (toRealFloat . read . dropSpaces)
                          . break (==' '))
  where
    mapTuple = join (***)

dropSpaces :: String -> String
dropSpaces = dropWhile (==' ')




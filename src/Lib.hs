module Lib
    ( imageCompressor
    ) where

import System.Environment
import Data.Char
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import Data.Fixed

data Pos = Pos Double Double deriving (Show)

data Color = Color Double Double Double deriving (Show)

data Pixel = Pixel Pos Color deriving (Show)

subsqr :: Double -> Double -> Double
subsqr a b = (a * a - 2 * a * b + b * b)

distance :: Pixel -> [Pixel] -> Int
distance pixel l = doDistance pixel l 0 0 500

doDistance :: Pixel -> [Pixel] -> Int -> Int -> Double -> Int
doDistance pixel [] i index min = index
doDistance (Pixel (Pos x1 y1) (Color r1 g1 b1)) ((Pixel (Pos x2 y2) (Color r2 g2 b2)):l) i index min =
       if min > (sqrt ((subsqr r1 r2) + (subsqr g1 g2) + (subsqr b1 b2)))
          then doDistance (Pixel (Pos x1 y1) (Color r1 g1 b1)) l (i + 1) i (sqrt ((subsqr r1 r2) + (subsqr g1 g2) + (subsqr b1 b2)))
       else doDistance (Pixel (Pos x1 y1) (Color r1 g1 b1)) l (i + 1) index min

displayUsage :: IO ()
displayUsage = do
    putStrLn "USAGE: ./imageCompressor n e IN"
    putStrLn ""
    putStrLn "      n       number of colors in the final image"
    putStrLn "      e       convergence limit"
    putStrLn "      IN      path to the file containing the colors of the pixels"

isNbr :: String -> Bool
isNbr str = rec_isdigit str
    where
        rec_isdigit [] = True
        rec_isdigit (h:t) = if isDigit h == False && h /= '.'
            then False 
            else rec_isdigit t

average :: [Pixel] -> Pixel
average pixels = doAverage pixels (Pixel (Pos 0 0) (Color 0 0 0)) (fromIntegral (length pixels))
    where
        doAverage [] (Pixel (Pos _ _) (Color r g b)) nb = (Pixel (Pos 0 0) (Color (r / nb) (g / nb) (b / nb)))
        doAverage ((Pixel (Pos _ _) (Color r g b)):l) (Pixel (Pos _ _) (Color rc gc bc)) nb = doAverage l (Pixel (Pos 0 0) (Color (r + rc) (g + gc) (b + bc))) nb

-- CONVERGENCE FUNCTIONS

hasColorConverged :: Pixel -> Pixel -> Double -> Bool
hasColorConverged (Pixel (Pos _ _) (Color r1 g1 b1)) (Pixel (Pos _ _) (Color r2 g2 b2)) conv =
    if hcc r1 r2 conv == False && hcc g1 g2 conv == False && hcc b1 b2 conv == False
        then False
        else True
        where
            hcc a b conv = 
                if a - b >= conv || b - a >= conv
                    then True
                    else False

hasConverged :: [Pixel] -> [Pixel] -> Double -> Bool
hasConverged [] [] _ = False
hasConverged (h1:l1) (h2:l2) conv =
    if hasColorConverged h1 h2 conv == False
        then hasConverged l1 l2 conv
        else True

-- APPEND FUNCTIONS

rec_apd :: [[Pixel]] -> Pixel -> Int -> [[Pixel]] -> [[Pixel]]
rec_apd [] _ _ nc = nc
rec_apd (cl:l) pixel 0 nc = rec_apd l pixel 4096 (nc ++ [cl ++ [pixel]])
rec_apd (cl:l) pixel n nc = rec_apd l pixel (n - 1) (nc ++ [cl])

appendToNList :: [[Pixel]] -> Pixel -> Int -> [[Pixel]]
appendToNList clusters pixel n = rec_apd clusters pixel n []

-- SEPARATE USING SPACE

parseLineSpace :: String -> String -> Maybe [String]
parseLineSpace s acc
    | s == [] = Nothing
    | (head s) == ' ' = 
        if (tail s) == []
            then Nothing
        else if (s !! 1) == '('
            then Just ([acc] ++ [(tail s)])
            else parseLineSpace (tail s) (acc ++ [(head s)])
    | otherwise = parseLineSpace (tail s) (acc ++ [(head s)])

sepSpace :: String -> Maybe [String]
sepSpace str = parseLineSpace str ""

-- PARSE PARENTHEIS

tupleParser :: Read a => ReadP [a]
tupleParser = between (char '(') (char ')') sepList
    where
        sepList = sepBy (readS_to_P reads) (char ',')

runParser :: ReadP a -> String -> Maybe a
runParser p input =
  case readP_to_S p input of
    [(a, "")] -> Just a
    _ -> Nothing

parseParentheis :: String -> Maybe [Double]
parseParentheis str = runParser tupleParser str :: Maybe [Double]

-- GET PIXEL

getPixelFromLine :: String -> Maybe Pixel
getPixelFromLine str = do
    sepstr <- sepSpace str
    (x:y:_) <- parseParentheis (head sepstr)
    (r:g:b:_) <- parseParentheis (last sepstr)
    return (Pixel (Pos x y) (Color r g b))

getPixels :: String -> [Maybe Pixel]
getPixels content = fmap getPixelFromLine (lines content)

-- CLUSTERS

getAverageClusters :: [[Pixel]] -> [Pixel]
getAverageClusters clusters = rec_gAC clusters []
    where
        rec_gAC [] acc = acc
        rec_gAC (c:lt) acc = rec_gAC lt (acc ++ [average c])

createClusters :: Int -> [[Pixel]]
createClusters nbC = rec_cC (nbC - 1) [[]]
    where
        rec_cC 0 acc = acc
        rec_cC n acc = rec_cC (n - 1) (acc ++ [[]])

firstFillClusters :: [Pixel] -> [[Pixel]] -> [Pixel] -> [[Pixel]]
firstFillClusters pixels clusters _ = rec_firstFill pixels clusters 0
    where
        rec_firstFill [] c _ = c
        rec_firstFill (pixel:lt) c n =
            if n >= length c
                then rec_firstFill lt (appendToNList c pixel 0) 1
                else rec_firstFill lt (appendToNList c pixel n) (n + 1)

--              Pixels     Clusters    Average   res Clusters
fillClusters :: [Pixel] -> [[Pixel]] -> [Pixel] -> [[Pixel]]
fillClusters [] clusters _ = clusters
fillClusters (pixel:pList) clusters ave = fillClusters pList (appendToNList clusters pixel (distance pixel ave)) ave

doClusters :: ([Pixel] -> [[Pixel]] -> [Pixel] -> [[Pixel]]) -> [Pixel] -> Int -> Double -> [Pixel] -> IO ()
doClusters fun pixels nbC conv [] = do
    let crate = fun pixels (createClusters nbC) []
    doClusters fillClusters pixels nbC conv (getAverageClusters crate)
doClusters fun pixels nbC conv ave = do
    let crate = fun pixels (createClusters nbC) ave
    let nAve = getAverageClusters crate
    if hasConverged nAve ave conv == False
        then displayClusters crate nAve
        else doClusters fillClusters pixels nbC conv nAve

clusterColors :: [String] -> [Pixel] -> IO ()
clusterColors args pixels = do
    let nbC = read (args !! 0) :: Int
    let conv = read (args !! 1) :: Double

    doClusters firstFillClusters pixels nbC conv []


checkPixelErr :: [Maybe Pixel] -> Bool
checkPixelErr [] = True
checkPixelErr lists = rec_cce lists
    where
        rec_cce (h:t) = if isNothing h == True then True else rec_cce t
        rec_cce [] = False

-- MAIN IMAGE COMPRESSOR

startIC :: [String] -> IO ()
startIC args = do
    content <- readFile (args !! 2)
    let pixels = getPixels content -- colors -> [Maybe Color]
    case checkPixelErr pixels of
        True -> putStrLn "An error occured while parsing the file"
        False -> clusterColors args (catMaybes pixels)

imageCompressor :: IO ()
imageCompressor = do
    args <- getArgs
    if length args /= 3
        then do
            displayUsage
            -- exitWith $ exitFailure 84
    else if (isNbr (args !! 0)) == False || (isNbr (args !! 1)) == False
        then do
            displayUsage
            -- exitWith $ exitFailure 84
    else
        startIC args

formatDouble :: Double -> String
formatDouble n =
    if (round (mod' (n * 100) 100)) == 0
        then show (floor n) ++ ".00"
    else if (round (mod' (n * 100) 100)) < 10
        then show (floor n) ++ ".0" ++ show (round (mod' (n * 100) 100))
    else
        show (floor n) ++ "." ++ show (round (mod' (n * 100) 100))

printCluster :: [Pixel] -> IO ()
printCluster [] = putStr ""
printCluster ((Pixel (Pos x y) (Color r g b)):list) = do
    putStrLn ("(" ++ (show $ round x) ++ "," ++ (show $ round y) ++ ") (" ++ (show $ round r) ++ "," ++ (show $ round g) ++ "," ++ (show $ round b) ++ ")")
    printCluster list

displayClusters :: [[Pixel]] -> [Pixel] -> IO ()
displayClusters clusters [] = putStr ""
displayClusters (cl:clList) ((Pixel (Pos _ _) (Color r g b)):list) = do
    putStrLn "--"
    if isNaN r == True || isNaN g == True || isNaN b == True
        then do 
            putStrLn "(0.00,0.00,0.00)\n-"
            displayClusters clList list
        else do
            putStrLn ("(" ++ (formatDouble r) ++ "," ++ (formatDouble g) ++ "," ++ (formatDouble b) ++ ")")
            putStrLn "-"
            printCluster cl
            displayClusters clList list
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Monadic(run, sumPgm)
--import Opt(run, sumPgm)
--import Semantics(run, sumPgm)
import System.IO(hFlush, stdout)
import qualified Data.Map.Strict as Map(lookup,fromList,empty)
import Data.Either(fromRight)

main :: IO ()
main = putStrLn $ show $ Map.lookup "x" $ fromRight Map.empty $ run $ sumPgm 30200100
{--
main :: IO ()
main = do
    --putStr "input size: "
    --hFlush stdout
    --input <- getLine
    --let size   = read input::Int
    let size   = 30200100
        output = run $ sumPgm size
    putStrLn $ show output
--}

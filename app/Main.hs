module Main where

import Semantics(run, sumPgm)
--import Opt(run, sumPgm)
import System.IO(hFlush, stdout)

main :: IO ()
main = do
    --putStr "input size: "
    --hFlush stdout
    --input <- getLine
    --let size   = read input::Int
    let size   = 1200300
        output = run $ sumPgm size
    putStrLn $ show output

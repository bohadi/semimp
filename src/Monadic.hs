{-# LANGUAGE OverloadedStrings #-}

module Monadic (
    run
  , sumPgm
) where

import qualified Data.Map.Strict as Map(Map,lookup,insert,empty)
import           Data.Maybe            (fromJust)
import qualified Data.ByteString as B  (ByteString,concat,pack)
import           Data.ByteString.Char8 (pack)
import           Control.Monad         (Monad, liftM, ap, unless, when)

infix  1 :=
infixl 6 :+:, :-:
infixl 7 :*:, :/:
infixl 8 :<=:, :&&:
infixl 9 :||: 

type Id = B.ByteString
type State = Map.Map Id Int
data AExp = C Int
          | V Id
          | AExp :+: AExp
          | AExp :-: AExp
          | AExp :*: AExp
          | AExp :/: AExp
data BExp = B Bool
          | AExp :<=: AExp
          | BExp :&&: BExp
          | BExp :||: BExp
data Stmt = Id := AExp
          | If BExp Stmt Stmt
          | While BExp Stmt
          | Seq [Stmt]
type Pgm = Stmt

newtype Interpreter a = Interpreter { runInterpreter :: State -> Either B.ByteString (a, State) }
instance Monad Interpreter where
    return x = Interpreter $ \r -> Right (x, r)
    i >>= k  = Interpreter $ \r -> case runInterpreter i r of
                 Left msg      -> Left msg
                 Right (x, r') -> runInterpreter (k x) r'
    fail msg = Interpreter $ \_ -> Left $ pack msg
instance Applicative Interpreter where
    pure  = return
    (<*>) = ap
instance Functor Interpreter where
    fmap = liftM

rd :: Id -> Interpreter Int
rd x = Interpreter $ \r -> case Map.lookup x r of
    Nothing -> Left (B.concat ["unbound var ",x])
    Just v  -> Right (v, r)
wr :: Id -> Int -> Interpreter ()
wr x v = Interpreter $ \r -> Right ((), Map.insert x v r)

aeval :: AExp -> Interpreter Int
aeval (C n)       = do return n
aeval (V x)       = do rd x
aeval (e1 :+: e2) = do v1 <- aeval e1
                       v2 <- aeval e2
                       return (v1 + v2)
aeval (e1 :-: e2) = do v1 <- aeval e1
                       v2 <- aeval e2
                       return (v1 - v2)
aeval (e1 :*: e2) = do v1 <- aeval e1
                       v2 <- aeval e2
                       return (v1 * v2)
aeval (e1 :/: e2) = do v1 <- aeval e1
                       v2 <- aeval e2
                       if v2 == 0
                         then fail "divide by zero"
                         else return (v1 `div` v2)
beval :: BExp -> Interpreter Bool
beval (B b)          = do return b
beval (ae1 :<=: ae2) = do v1 <- aeval ae1
                          v2 <- aeval ae2
                          return (v1 <= v2)
beval (be1 :&&: be2) = do v1 <- beval be1
                          v2 <- beval be2
                          return (v1 && v2)
beval (be1 :||: be2) = do v1 <- beval be1
                          v2 <- beval be2
                          return (v1 || v2)

exec :: Stmt -> Interpreter ()
exec (x := e)       = do v <- aeval e
                         wr x v
exec (If e s1 s2)   = do c <- beval e
                         when c   $ exec s1
                         unless c $ exec s2
exec (While e s)    = do c <- beval e
                         when c $ exec (Seq [s, While e s])
exec (Seq [])       = do return ()
exec (Seq (s : ss)) = do exec s
                         exec $ Seq ss

run :: Pgm -> Either B.ByteString State
run p = case runInterpreter (exec p) Map.empty of
    Left msg      -> Left msg
    Right (_, r') -> Right r'

sumPgm :: Int -> Pgm
sumPgm n = Seq [ "n" := C n
               , "x" := C 0
               , While ((C 0) :<=: (V "n")) $ Seq
                   [ "x" := V "x" :+: V "n"
                   , "n" := V "n" :-: C 1
                   ]
               ]

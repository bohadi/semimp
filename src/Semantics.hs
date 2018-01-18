{-# LANGUAGE LambdaCase #-}
 
module Semantics (
    run
  , sumPgm
) where

import qualified Data.Map.Strict as Map(Map,empty,fromList,member,lookup,insert)
import           Data.Maybe            (isJust,fromJust)

--import Debug.Trace

run :: Pgm -> State
run p = case step p of
    Left p' -> run p'
    --Left p' -> trace (">> " ++ show p') $ run p'
    Right (Stuck s) -> s

-- //sum.imp
-- int n, sum;
-- n = 100;
-- sum = 0;
-- while (0 <= n) {
--   sum = sum + n;
--   n = n + -1;
-- }
sumPgm :: Int -> Pgm
sumPgm io = (ast, s) where
    s   = Map.empty
    ast = Vars ["n", "sum"]
        $ Seq (Assign "n"   $ AVal io)
        $ Seq (Assign "sum" $ AVal 0 )
        $ While (Lte (AVal 0) (AVar "n"))
            $ Seq (Assign "sum" $ Add (AVar "sum") (AVar "n" ))
                  (Assign "n"   $ Add (AVar "n"  ) (AVal (-1)))

type AExp = KItem
type BExp = KItem
type Stmt = KItem
data KItem =
    -- aexp
    AVal Int
  | AVar Id
  | Div AExp AExp
  | Add AExp AExp
  | AThunk
  | AEval AExp KItem
    -- bexp
  | BVal Bool
  | Lte AExp AExp
  | Not BExp
  | And BExp BExp
  | BThunk
  | BEval BExp KItem
    -- stmt
  | Empty
  | Assign Id AExp
  | If BExp Stmt Stmt
  | While BExp Stmt
  | Seq Stmt Stmt
  | Vars [Id] Stmt
    deriving Show

type Id = String
type State = Map.Map Id Int

type Pgm = (KItem, State)

newtype Stuck = Stuck State 

step :: Pgm -> Either Pgm Stuck
-- init
step (Vars ids p  ,s) = Left (p, Map.fromList $ (\id->(id,0)) <$> ids)
-- control
step (Empty       ,s) = Right $ Stuck s
step (Seq fst snd ,s) = Left (snd, run (fst ,s))
step (Assign id (AVal v)  ,s) = Right $ Stuck s' where
    s' = if Map.member id s then Map.insert id v s else s
step (If (BVal True)  p _ ,s) = Right $ Stuck $ run (p ,s)
step (If (BVal False) _ p ,s) = Right $ Stuck $ run (p ,s)
step (w@(While c loop)    ,s) = Left (If c (Seq loop w) Empty  ,s)
-- aexp
step (AVar id ,s) = if cd then is else nt where
    cd = Map.member id s
    is = Left (AVal $ fromJust $ Map.lookup id s ,s)
    nt = Right $ Stuck s
step (Div (AVal i) (AVal j) ,s) = if cd then is else nt where
    cd = j /= 0
    is = Left (AVal $ i `quot` j ,s)
    nt = Right $ Stuck s
step (Add (AVal i) (AVal j) ,s) = Left (AVal $ i+j  ,s)
-- bexp
step (Lte (AVal i) (AVal j) ,s) = Left (BVal $ i<=j ,s)
step (Not (BVal b)          ,s) = Left (BVal $ not b, s)
step (And (BVal True)  b    ,s) = Left (b ,s)
step (And (BVal False) _    ,s) = Left (BVal False ,s)
-- heating
--   athunk
step (Div p q     ,s)
    | notAVal p = Left (AEval p $ Div AThunk q     ,s)
    | notAVal q = Left (AEval q $ Div p AThunk     ,s)
step (Add p q     ,s)
    | notAVal p = Left (AEval p $ Add AThunk q     ,s)
    | notAVal q = Left (AEval q $ Add p AThunk     ,s)
step (Lte p q     ,s)
    | notAVal p = Left (AEval p $ Lte AThunk q     ,s)
    | notAVal q = Left (AEval q $ Lte p AThunk     ,s)
step (Assign id p ,s)
    | notAVal p = Left (AEval p $ Assign id AThunk ,s)
--  bthunk
step (Not b       ,s)
    | notBVal b = Left (BEval b $ Not BThunk       ,s)
step (And p q     ,s)
    | notBVal p = Left (BEval p $ And BThunk q     ,s)
step (If c p q    ,s)
    | notBVal c = Left (BEval c $ If BThunk p q    ,s)
-- cooling
--   athunk
step (AEval p'@(AVal _) (Div AThunk q) ,s) = Left (Div p' q  ,s)
step (AEval p'@(AVal _) (Add AThunk q) ,s) = Left (Add p' q  ,s)
step (AEval p'@(AVal _) (Lte AThunk q) ,s) = Left (Lte p' q  ,s)
step (AEval q'@(AVal _) (Div p AThunk) ,s) = Left (Div p  q' ,s)
step (AEval q'@(AVal _) (Add p AThunk) ,s) = Left (Add p  q' ,s)
step (AEval q'@(AVal _) (Lte p AThunk) ,s) = Left (Lte p  q' ,s)
step (AEval p'@(AVal _) (Assign id AThunk) ,s) = Left (Assign id p' ,s)
--   bthunk
step (BEval b'@(BVal _) (Not BThunk  ) ,s) = Left (Not b'    ,s)
step (BEval p'@(BVal _) (And BThunk q) ,s) = Left (And p' q  ,s)
step (BEval c'@(BVal _) (If BThunk p q) ,s) = Left (If c' p q ,s)
-- heating strictly
step (AEval p kitem ,s) = if cd then is else nt where
    p' = heat p s
    cd = isJust p'
    is = Left (AEval (fromJust p') kitem ,s)
    nt = Right $ Stuck s
step (BEval p kitem ,s) = if cd then is else nt where
    p' = heat p s
    cd = isJust p'
    is = Left (BEval (fromJust p') kitem ,s)
    nt = Right $ Stuck s
-- no rule matched
step (_ ,s) = Right $ Stuck s

heat :: KItem -> State -> Maybe KItem
heat p@(AVal _) _ = Just p
heat p@(BVal _) _ = Just p
heat p          s = case step (p,s) of
    Left  (p' ,_)   ->  heat p' s
    --Left  (p' ,_)   -> trace ("}} " ++ show p') $ heat p' s
    Right (Stuck _) -> Nothing

notAVal :: AExp -> Bool
notBVal :: BExp -> Bool
notAVal = \case (AVal _) -> False
              --(AThunk) -> False
                _        -> True
notBVal = \case (BVal _) -> False
              --(BThunk) -> False
                _        -> True

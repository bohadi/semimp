{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
 
module Opt (
    run
  , sumPgm
) where

import qualified Data.Map.Strict as Map(Map,empty,fromList,member,lookup,insert)
import           Data.Maybe            (fromJust)
import           Data.ByteString       (ByteString)

run :: Ast -> State
run [Vars ids p] = go (init, [p])
    where init = Map.fromList $ (\id->(id,0)) <$> ids
run _ = Map.empty

sumPgm :: Int -> Ast
sumPgm io = [     Vars ["n", "sum"]
                $ Seq (Assign "n"   $ AVal io)
                $ Seq (Assign "sum" $ AVal 0 )
                $ While (Lte (AVal 0) (AVar "n"))
                    $ Seq (Assign "sum" $ Add (AVar "sum") (AVar "n" ))
                          (Assign "n"   $ Add (AVar "n"  ) (AVal (-1)))     ]

type AExp = KItem
type BExp = KItem
type Stmt = KItem
data KItem =
    AVal Int
  | AVar Id
  | Div AExp AExp
  | Add AExp AExp
  | BVal Bool
  | Lte AExp AExp
  | Not BExp
  | And BExp BExp
  | DivL KItem
  | DivR KItem
  | AddL KItem
  | AddR KItem
  | LteL KItem
  | LteR KItem
  | NotF
  | AndL KItem
  | AssignR Id
  | IfC KItem KItem
  | Empty
  | Assign Id AExp
  | If BExp Stmt Stmt
  | While BExp Stmt
  | Seq Stmt Stmt
  | Vars [Id] Stmt
    deriving Show
type Id = ByteString
type State = Map.Map Id Int
type Ast = [KItem]
type Pgm = (State, Ast)

go :: Pgm -> State
go (s,[]) = s
go (s, AVar   id             :xs) = if Map.member id s then go (s, (AVal $ fromJust $ Map.lookup id s):xs) else s
go (s, Assign id (AVal v)    :xs) = if Map.member id s then go (Map.insert id v s,                     xs) else s
go (s, w@(While c loop)      :xs) = if cd then go opt else go naive
    where iters = checkDecrInductLoop c loop s
          cd    = iters >= 0
          s'    = go (s, replicate iters loop)
          opt   = (s', xs)
          naive = (s, If c (Seq loop w) Empty  :xs)
go (s, a) = go (s, step a)

step :: Ast -> Ast
step (Seq fst snd             :xs) =                  fst:snd:xs
step (If (BVal True)  is _    :xs) =                       is:xs
step (If (BVal False) _  nt   :xs) =                       nt:xs
step (Empty                   :xs) =                          xs
step (Div (AVal i) (AVal j)   :xs) = if j/=0 then (AVal $ i `quot` j):xs else []
step (Add (AVal i) (AVal j)   :xs) =     (AVal $ i+j  )      :xs
step (Lte (AVal i) (AVal j)   :xs) =     (BVal $ i<=j )      :xs
step (Not (BVal b)            :xs) =     (BVal $ not b)      :xs
step (And (BVal True)  b      :xs) =                 b       :xs
step (And (BVal False) _      :xs) =      BVal   False       :xs
step (Div p q     :xs) | notAVal p =          p:DivL   q     :xs
                       | notAVal q =          q:DivR p       :xs
step (Add p q     :xs) | notAVal p =          p:AddL   q     :xs
                       | notAVal q =          q:AddR p       :xs
step (Lte p q     :xs) | notAVal p =          p:LteL   q     :xs
                       | notAVal q =          q:LteR p       :xs
step (Assign id p :xs) | notAVal p =          p:AssignR id   :xs
step (Not b       :xs) | notBVal b =          b:NotF         :xs
step (And   p q   :xs) | notBVal p =          p:AndL   q     :xs
step (If  c is nt :xs) | notBVal c =          c:IfC    is nt :xs
step (p'@(AVal _):DivL q      :xs) =            Div p' q     :xs
step (q'@(AVal _):DivR p      :xs) =            Div p  q'    :xs
step (p'@(AVal _):AddL q      :xs) =            Add p' q     :xs
step (q'@(AVal _):AddR p      :xs) =            Add p  q'    :xs
step (p'@(AVal _):LteL q      :xs) =            Lte p' q     :xs
step (q'@(AVal _):LteR p      :xs) =            Lte p  q'    :xs
step (p'@(AVal _):AssignR id  :xs) =            Assign id p' :xs
step (        b' :NotF        :xs) =            Not b'       :xs
step (        p' :AndL q      :xs) =            And p' q     :xs
step (        c' :IfC is nt   :xs) =            If  c' is nt :xs
step _                             = []

-- assume fully eval expr, modulo var lookup
checkDecrInductLoop :: BExp -> Stmt -> State -> Int
checkDecrInductLoop (Lte (AVal v) (AVar id)) l s | l `isDecr` id = iters
    -- assert Map.member id s
    where initial = fromJust $ Map.lookup id s
          iters   = initial - v + 1
checkDecrInductLoop _ _ _ = (-1)

-- assume no nested while
isDecr :: Stmt -> Id -> Bool
isDecr (Assign id (Add (AVar x) (AVal v))) n = id==n && x==n && v==(-1)
isDecr (Assign id (Add (AVal v) (AVar x))) n = id==n && x==n && v==(-1)
isDecr (If (BVal True ) p _)               n = isDecr p n
isDecr (If (BVal False) _ q)               n = isDecr q n
isDecr (Seq p q)                           n = (isDecr p n) || (isDecr q n)
isDecr _                                   _ = False

notAVal :: AExp  -> Bool
notBVal :: BExp  -> Bool
notAVal = \case (AVal _) -> False
                _        -> True
notBVal = \case (BVal _) -> False
                _        -> True

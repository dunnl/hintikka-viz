{-# language OverloadedStrings #-}

module Lib where

import Data.GraphViz.Types.Monadic
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz
import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Colors as A
import Control.Monad
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Logic
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

attrOfPlayer :: Player -> Attributes
attrOfPlayer Verifier =
  [ color Green ]
attrOfPlayer Falsifier =
  [ color Blue ]
attrOfPlayer LeafNode =
  [ color Red ]

data State =
  State { maxNode :: Int -- ^ Maximum node used thus far
        , newSubroot :: Int -- ^ Subroot made by call returning the state
        }

pnode :: Expr -> n -> Dot n
pnode expr n =
  node n $ (attrOfPlayer (playerAt expr)) ++ [textLabel (pretty expr)]
  
-- | A monadic action that returns a pair (ix, root)
-- where ix is the highest *used* value for naming an internal node
-- and root is the name of the root of that subgraph
exprToGraph :: Int -> -- highest used value so far
               Expr ->
               DotM Node State
exprToGraph max arg@(Forall body) = do
  trace ("Called on " ++ show arg) $ do
    pnode arg root
    State newmax _ <- foldM buildSub initState nextWorlds
    return $ State newmax root
  where
    root = max + 1
    -- Possible worlds reachable from root
    nextWorlds = map (\i -> open body i) domain
    -- Initial state before recursive subcalls
    initState = State root (error "The fold shouldn't need this value")
    -- Subaction that performs a subcall,
    -- adds an edge, and returns attributes from the subcall
    buildSub (State max _) subexpr = do
      State max' subroot <- exprToGraph max subexpr
      root --> subroot
      return $ State max' subroot

exprToGraph max arg@(Exists body) = do
  trace ("Called on " ++ show arg) $ do
    pnode arg root
    State newmax _ <- foldM buildSub initState nextWorlds
    return $ State newmax root
  where
    root = max + 1
    -- Possible worlds reachable from root
    nextWorlds = map (\i -> open body i) domain
    -- Initial state before recursive subcalls
    initState = State root (error "The fold shouldn't need this value")
    -- Subaction that performs a subcall,
    -- adds an edge, and returns attributes from the subcall
    buildSub (State max _) subexpr = do
      State max' subroot <- exprToGraph max subexpr
      root --> subroot      
      return $ State max' subroot
      
exprToGraph max arg@(Neg (Atom _ _)) = do
  trace ("Called on " ++ show arg) $ do
    pnode arg (max + 1)
    return $ State (max + 1) (max + 1)
    
exprToGraph max arg@(Neg (Forall body)) =
  exprToGraph max (Exists (Neg body))

exprToGraph max arg@(Neg (Exists body)) =
  exprToGraph max (Forall (Neg body))

exprToGraph max arg@(Neg (Binop Conj l r)) =
  exprToGraph max (Binop Disj (Neg l) (Neg r))
                
exprToGraph max arg@(Neg (Binop Disj l r)) =
  exprToGraph max (Binop Conj (Neg l) (Neg r))
                
exprToGraph max arg@(Neg (Binop Impl l r)) =
  exprToGraph max (Binop Disj l (Neg r))

  
exprToGraph ix arg@(Binop op l r) = do
  trace ("Called on " ++ show arg) $ do
    pnode arg root
    State max' subroot_l <- exprToGraph root l
    root --> subroot_l
    State max'' subroot_r <- exprToGraph max' r
    root --> subroot_r
    return $ State max'' root
      where
        root = ix + 1

exprToGraph ix arg@(Atom _ _) = do
  trace ("Called on " ++ show arg) $ do
    pnode arg (ix + 1)
    return $ State (ix + 1) (ix + 1)

makeGraph :: Expr -> G.DotGraph Int
makeGraph expr =
  digraph (Num (Int 1))
    (exprToGraph 0 expr)

printGraph :: Expr -> IO ()
printGraph expr =
  TL.writeFile "out.dot" $ printDotGraph (makeGraph expr)

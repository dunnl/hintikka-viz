{-# language OverloadedStrings #-}

module Logic where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.List as L

type Var = Int

data Op =
    Conj
  | Disj
  | Impl
  deriving (Show, Eq)

data Player = Verifier
            | Falsifier
            | LeafNode

-- | Compute the other player
oppositeOf :: Player -> Player
oppositeOf Verifier = Falsifier
oppositeOf Falsifier = Verifier

-- | An expression of first-order logic
-- A closed expression with deBruijn indices
data Expr =
    Forall Expr
  | Exists Expr
  | Binop Op Expr Expr
  | Neg Expr
  | Atom Text [Var]
  deriving (Show, Eq)

-- | Compute the player at a particular expression
playerAt :: Expr -> Player
playerAt (Forall _) = Falsifier
playerAt (Binop Conj _ _) = Falsifier
playerAt (Exists _) = Verifier
playerAt (Binop Disj _ _) = Verifier
playerAt (Binop Impl _ _) = Verifier
playerAt (Neg body) = oppositeOf (playerAt body)
playerAt (Atom _ _) = LeafNode

exampleExpr :: Expr
exampleExpr =
  Forall (Binop Conj (Atom "R" [0]) (Exists (Atom "S" [1, 0])))

openLevel :: Expr ->
        Int ->
        Var -> Expr
openLevel expr k val =
  case expr of
    Forall body -> Forall (openLevel body (k+1) val)
    Exists body -> Exists (openLevel body (k+1) val)
    Neg body -> Neg (openLevel body k val)
    Binop op l r -> Binop op (openLevel l k val) (openLevel r k val)
    Atom rel args ->
      let f arg = if arg == k then val else arg
      in Atom rel (f <$> args)

open :: Expr -> Var -> Expr
open expr var = openLevel expr 0 var

(+++) = TL.append

class Pretty a where
  pretty :: a -> Text

instance Pretty Op where
  pretty Conj = " ∧ "
  pretty Disj = " ∨ "
  pretty Impl = " → "

instance Pretty Int where
  pretty = TL.pack . show
  
instance Pretty Expr where
  pretty (Forall exp) = "∀ (" +++ pretty exp +++ ")"
  pretty (Exists exp) = "∃ (" +++ pretty exp +++ ")"
  pretty (Neg exp) = "¬ (" +++ pretty exp +++ ")"
  pretty (Binop op l r) = "(" +++ pretty l
    +++ pretty op +++ pretty r +++ ")"
  pretty (Atom rel args) = "(" +++ rel +++ TL.concat (L.intersperse ", " $ map pretty args) +++ ")"

domain :: [ Var ]
domain = [1..5]

type Node = Int

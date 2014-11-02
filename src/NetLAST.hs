{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module NetLAST where

import Lexer
import Data.Foldable

type Ident = String

data NetL = NetL {inputs :: [Ident], outputs :: [Ident], var :: [Atom], eqtns :: [BaseEqtn]} deriving Show

type BaseEqtn = (Ident,Expr BaseArg)
type IndexedEqtn = (Int,Expr IndexedArg)

-- | is only used for the @var@ field of a @NetL@
data Atom = Wire String | Ribbon String Int deriving Show

-- | Used to represent net-lists as parsed from the @.net@
data BaseArg = BConst Int | BVar Ident deriving Show

-- | Used to represent the values and variables once they have been indexed
data IndexedArg = IdConst Bool |  IdVar Int 

-- | Argument will be either 'BaseArg' or 'IndexedArg' 
data Expr a = BOp Op a a
          | Select Int a
          | Concat a a
          | Id a
          | Slice  Int Int a
          | Ram(Int,Int) a a a a
          | Rom (Int,Int) a
          | Reg a
          | Input
          | Not a
          deriving (Show, Functor, Foldable)



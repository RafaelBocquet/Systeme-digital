module NetLAST where

import Lexer
type Ident = String

data NetL a = NetL {inputs :: [Ident], outputs :: [Ident], var :: [Atom], eqtns :: [BaseEqtn]} deriving Show

type BaseEqtn = (Ident,Expr BaseArg)
type IndexedEqtn = (Int,Expr IndexedArg)

data Atom = Wire String | Ribbon String Int deriving Show


data BaseArg = BConst Int | BVar Ident deriving Show

data IndexedArg = IdConst Bool |  IdVar Int 

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
          deriving Show

instance Functor Expr where
  fmap f (BOp op x y) = BOp op (f x) (f y)
  fmap f (Id x) = Id (f x)
  fmap f (Select x y) = Select x (f y)
  fmap f (Concat x y) = Concat (f x) (f y)
  fmap f (Slice x y z) = Slice x y (f z)
  fmap f (Ram x y1 y2 y3 y4) = Ram x (f y1) (f y2) (f y3) (f y4)
  fmap f (Reg x) = Reg (f x)
  fmap f (Rom x y) = Rom x (f y)
  fmap _ Input = Input
  fmap f (Not a) = Not (f a)

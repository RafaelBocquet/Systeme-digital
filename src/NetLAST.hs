module NetLAST where
import Lexer

type Ident = String

data NetL a = NetL {inputs :: [Ident], outputs :: [Ident], var :: [Atom], eqtns :: [BaseEqtn]} deriving Show

type BaseEqtn = (Ident,Expr BaseArg)
type IndexedEqtn = (Int,Expr IndexedArg)

data Atom = Wire String | Ribbon String Int deriving Show

data BaseArg = BConst Int | BVar Ident deriving Show

data IndexedArg = IdConst Bool |  IdVar Int -- | Multiple Int Int

data Expr a = BOp Op a a
          | Select Int a
          | Concat a a
          | Slice  Int Int a
          | Id a
          deriving Show

instance Functor Expr where
  fmap f (BOp op x y) = BOp op (f x) (f y)
  fmap f (Id x) = Id (f x)
  fmap f (Select x y) = Select x (f y)
  fmap f (Concat x y) = Concat (f x) (f y)
  fmap f (Slice x y z) = Slice x y (f z)

module NetLAST where

type Ident = String

data NetL a = NetL {inputs :: [Ident], outputs :: [Ident], var :: [Atom], eqtns :: [Eqtn a]} deriving Show

type Eqtn a = (Ident,Expr a)

data Atom = Wire String | Ribbon String Integer deriving Show

data BaseArg = Const Integer | Var Ident deriving Show

data IndexedArgs = BConst Bool | Simple Integer | Multiple Integer Integer

data Expr a = BOp Op a a
          | Select Integer Ident 
          | Concat Ident Ident
          | Slice  Integer Integer Ident
          | Id a
          deriving Show

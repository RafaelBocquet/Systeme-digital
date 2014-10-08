{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = a-z

@var = ($digit | $alpha | _ )+

PiliPili :-

   $white+                         ;
   "INPUT"                         {const INPUT}
   "OUTPUT"                        {const OUTPUT}
   "VAR"                           {const VAR}
   "IN"                            {const IN}
   (@var " "* "," " "*)+ @var      {IdentList . (splitOn ',') . (filter (/= ' '))}
   @var                            {Ident}
   "REG"                           {const REG}
   "XOR"                           {const (BoolOp XOR)}
   "AND"                           {const (BoolOp AND)}
   "NAND"                          {const (BoolOp NAND)}
   "OR"                            {const (BoolOp OR)}
   "="                             {const Eq}


{
splitOn _ [] = []
splitOn x l = 
        let (l1,l2) = span (/= x) l in
        l1:(splitOn x (dropWhile (== x) l2))

data Tokens = 
     INPUT | OUTPUT | VAR 
     | IN    | IdentList [String] | Ident String
     | REG   | BoolOp Op | Eq
     deriving (Show,Eq)

data Op = XOR | AND | NAND | OR deriving (Show,Eq)
}

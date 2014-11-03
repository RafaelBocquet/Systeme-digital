{
module Lexer(
 Tokens(..),
 Op(..),
 alexScanTokens)
 where
}

%wrapper "basic"

$digit = 0-9
$alpha = a-z

@ident = ($digit | $alpha | _ )+

PiliPili :-

   $white+                         ;
   ($digit)+                       {Num . read}
   "INPUT"                         {const INPUT}
   "OUTPUT"                        {const OUTPUT}
   "VAR"                           {const VAR}
   "IN"                            {const IN}
   @ident                          {Ident}
   "REG"                           {const REG}
   "XOR"                           {const (BoolOp XOR)}
   "AND"                           {const (BoolOp AND)}
   "NAND"                          {const (BoolOp NAND)}
   "OR"                            {const (BoolOp OR)}
   "="                             {const Eq}
   ":"                             {const Colon}
   ","                             {const Comma}
   "CONCAT"                        {const CONCAT}
   "SLICE"                         {const SLICE}
   "SELECT"                        {const SELECT}
   "RAM"                           {const RAM}
   "ROM"                           {const ROM}
   "NOT"                           {const NOT}
   "MUX"                           {const MUX}
{
data Tokens = 
     INPUT | OUTPUT | VAR 
     | IN    | Ident String
     | REG   | ROM | RAM | BoolOp Op | Eq 
     | Colon | Comma | Num Int
     | CONCAT | SLICE | SELECT
     | NOT | MUX
     deriving (Show,Eq)

data Op = XOR | AND | NAND | OR deriving (Show,Eq)
}

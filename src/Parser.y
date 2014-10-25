{
module Parser where

import Lexer
import NetLAST
}

%token
        in             {IN}
        input          {INPUT}
        output         {OUTPUT}
        var            {VAR}
        ident          {Ident $$}
        op             {BoolOp $$}
        '='            {Eq}
        ':'            {Colon}
        ','            {Comma}
        num            {Num $$}
        concat         {CONCAT}
        slice          {SLICE}
        select         {SELECT}

%name parse
%tokentype { Tokens }

%%


file : input identlist output identlist var atomlist in eqtns  {NetL {inp = $2, out = $4, var = $6, op = $8}}

identlist :          { [] }
          | ident    { [$1] }
          | identlist ',' ident { $3 : $1 }

atom : ident         {Wire $1}
     | ident ':' num   {Ribbon $1 $3}

atomlist : {- empty -}          { [] }
| atom                          { [$1]}
| atomlist ',' atom      { $3 : $1 }

arg : ident         {Var $1}
| num           {Const $1}

expr : op arg arg          {BOp $1 $2 $3}
| select num ident     {Select $2 $3}
| slice num num ident  {Slice $2 $3 $4}
| concat ident ident   {Concat $2 $3}
| arg                  {Id $1}


eqtns : {- empty -}                { [] }
| eqtns ident '=' expr          {($2,$4):$1}




{

happyError = error "coincoin"
}

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
        reg            {REG}
        ram            {RAM}
        rom            {ROM}
        not            {NOT}

%name parse
%tokentype { Tokens }

%%


file : input identlist output identlist var atomlist in eqtns  {NetL {inputs = $2, outputs = $4, var = $6, eqtns = $8}}

identlist :          { [] }
          | ident    { [$1] }
          | identlist ',' ident { $3 : $1 }

atom : ident         {Wire $1}
     | ident ':' num   {Ribbon $1 $3}

atomlist : {- empty -}          { [] }
| atom                          { [$1]}
| atomlist ',' atom      { $3 : $1 }

arg : ident         {BVar $1}
| num               {BConst $1}

expr : op arg arg          {BOp $1 $2 $3}
| select num arg     {Select $2 $3}
| slice num num arg  {Slice $2 $3 $4}
| concat arg arg   {Concat $2 $3}
| arg                  {Id $1}
| reg arg           {Reg $2}
| ram num num arg arg arg arg {Ram ($2,$3) $4 $5 $6 $7}
| rom num num arg             {Rom ($2,$3) $4}
| not arg                     {Not $2}

eqtns : {- empty -}                { [] }
| eqtns ident '=' expr          {($2,$4):$1}




{

happyError = error "coincoin"
}

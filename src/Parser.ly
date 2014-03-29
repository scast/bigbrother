> {
> module Parser (parser) where
> import Lexer
> }

> %left '..'
> %nonassoc BY
> %left '||'
> %left '&&'
> %left '|'
> %left '^'
> %left '&'
> %left '==' '!='
> %left '<=' '<' '>' '>='
> %left '<<' '>>'
> %left '+' '-'
> %left '*' '/' '%'
> %right NEG PLUS '~' '!' '@' '#' AS
> %left '**'
> %left '.' '[' '('

> %name parser
> %tokentype { Lexeme }
> %error { happyError }

> %token
>    NUMBER { L _ LNum $$ }
>    CHAR { L _ LChar $$ }
>    BOOL { L _ LBool $$ }
>    IDENT { L _ LId $$ }
>    STRING { L _ LString $$ }
>    CONST { L _ LConst s }
>    STATIC { L _ LStatic s }
>    FN { L _ LFn s }
>    VAR { L _ LVar s }
>    BY { L _ LBy s }
>    PRINT { L _ LPrint s }
>    GRAB { L _ LGrab s }
>    STRUCT { L _ LStruct s }
>    UNION { L _ LUnion s }
>    ENUM { L _ LEnum s }
>    UNSIGNED { L _ LUnsigned s }
>    AS { L _ LAs s }
>    TYPE { L _ LType s }
>    IF  { L _ LIf s }
>    ELSE { L _ LElse s }
>    LOOP { L _ LLoop s }
>    FOR { L _ LFor s }
>    WHILE { L _ LWhile s }
>    BREAK { L _ LBreak s }
>    CONTINUE { L _ LContinue s }
>    RETURN { L _ LReturn s }
>    '&&' { L _ LAnd $$ }
>    '||' { L _ LOr $$ }
>    '!' { L _ LNot $$ }
>    '^' { L _ LXor $$ }
>    '=' { L _ LEqual s }
>    ':' { L _ LColon s }
>    ';' { L _ LSemicolon s }
>    '[' { L _ LOpenBracket s }
>    ']' { L _ LCloseBracket s }
>    '{' { L _ LOpenCurly s }
>    '}' { L _ LCloseCurly s }
>    '(' { L _ LOpenParenthesis s }
>    ')' { L _ LCloseParenthesis s }
>    '#' { L _ LHash $$ }
>    '%' { L _ LPercentage $$ }
>    ',' { L _ LComma s }
>    '.' { L _ LDot s }
>    '..' { L _ LDots s }
>    '==' { L _ LEquals $$ }
>    '!=' { L _ LNotEquals $$ }
>    '>' { L _ LGreater $$ }
>    '>=' { L _ LGreaterEqual $$ }
>    '<' { L _ LLess $$ }
>    '<=' { L _ LLessEqual $$ }
>    '+' { L _ LPlus $$ }
>    '-' { L _ LMinus $$ }
>    '*' { L _ LMul $$ }
>    '/' { L _ LDiv $$ }
>    '**' { L _ LExp $$ }
> --    '++' { L _ LIncrease s }
> --   '--' { L _ LDecrease s }
>    '>>' { L _ LRShift $$ }
>    '<<' { L _ LLShift $$ }
>    '+=' { L _ LPlusEqual s }
>    '-=' { L _ LMinusEqual s }
>    '*=' { L _ LMulEqual s }
>    '/=' { L _ LDivEqual s }
>    '%=' { L _ LModEqual s }
>    '>>=' { L _ LRShiftEqual s }
>    '<<=' { L _ LLShiftEqual s }
>    '&=' { L _ LBAndEqual s }
>    '|=' { L _ LBOrEqual s }
>    '^=' { L _ LXorEqual s }
>    '&&=' { L _ LAndEqual s }
>    '||=' { L _ LOrEqual s }
>    '|' { L _ LBOr $$ }
>    '&' { L _ LBAnd $$ }
>    '~' { L _ LBNot $$ }
>    '@' { L _  LAt $$ }

> %%

> INICIAL
>   : GLOBAL { [$1] }
>   | INICIAL GLOBAL { $1 ++ [$2] }

> GLOBAL
> --  : GLOBALVAR { $1 }
> --  : STRUCTDEF { $1 }
> : FUNCION { $1 }

> FUNCION
>    : FN IDENT '(' PARLIST ')' ':' TYPEN '{' INSTLIST '}' { Function $2 $4 $7 $9 }
>    | FN IDENT '(' PARLIST ')' '{' INSTLIST '}' { Function $2 $4 (Type "void") $7 }

> PARLIST
>   : PARLIST ',' PARAMETER { $1 ++ [$3] }
>   | PARAMETER { [$1] }

> PARAMETER
>   : IDENT INIT ':' TYPEN { ($1, $2, $4) }


> TYPEN
>   : IDENT { Type $1 }
>   | IDENT DIMENSIONS { ArrayOf $1 $2 }

> DIMENSIONS
>   : '[' EXPR ']' { [Just $2] }
>   | '[' ']' { [Nothing] }
>   | DIMENSIONS '[' EXPR ']' { $1 ++ [Just $3] }
>   | DIMENSIONS '[' ']' { $1 ++ [Nothing] }

> INSTLIST
>   : INST ';' { [$1] }
>   | INSTLIST INST ';' { $1 ++ [$2] }

> INST
>   : DEC { $1 }
>   | RETURNP { $1 }
>   | CONTINUEP { $1 }
>   | BREAKP { $1 }
>   | PRINTP { $1 }
>   | GRABP { $1 }

> DEC
>   : VAR LISTAVARIABLES ':' TYPEN { VarDeclaration VarKind $4 $2 }

> EXPR
>   : CHAR { Char $1 }
>   | NUMBER { Number $1 }
>   | BOOL { Bool $1 }
>   | STRING { Str $1 }
>   | IDENT { Var $1  }
>   | IDENT '(' LISTAONADA ')' { FunctionCall $1 $3 }
>   | EXPR '.' EXPR { B "." $1 $3  }
>   | EXPR AS IDENT { TypeCast $1 $3 }
>   | '(' EXPR ')' { $2 }
>   | EXPR '+' EXPR { B $2 $1 $3 }
>   | EXPR '-' EXPR { B $2 $1 $3 }
>   | EXPR '*' EXPR { B $2 $1 $3 }
>   | EXPR '/' EXPR { B $2 $1 $3 }
>   | EXPR '&&' EXPR { B $2 $1 $3 }
>   | EXPR '||' EXPR { B $2 $1 $3 }
>   | EXPR '^' EXPR { B $2 $1 $3 }
>   | EXPR '==' EXPR { B $2 $1 $3 }
>   | EXPR '!=' EXPR { B $2 $1 $3 }
>   | EXPR '>' EXPR { B $2 $1 $3 }
>   | EXPR '>=' EXPR { B $2 $1 $3 }
>   | EXPR '<' EXPR { B $2 $1 $3 }
>   | EXPR '<=' EXPR { B $2 $1 $3 }
>   | EXPR '**' EXPR { B $2 $1 $3 }
>   | EXPR '>>' EXPR { B $2 $1 $3 }
>   | EXPR '<<' EXPR { B $2 $1 $3 }
>   | EXPR '|' EXPR { B $2 $1 $3 }
>   | EXPR '&' EXPR { B $2 $1 $3 }
>   | EXPR '%' EXPR { B $2 $1 $3 }
>   | EXPR '..' EXPR { R $1 $3 (Number "1") }
>   | EXPR '..' EXPR BY EXPR { R $1 $3 $5  }
>   | '#' EXPR { U $1 $2 }
>   | '@' EXPR { U $1 $2  }
>   | '~' EXPR { U $1 $2 }
>   | '-' EXPR %prec NEG { U $1 $2  }
>   | '+' EXPR %prec PLUS { U $1 $2  }
>   | '!' EXPR { U $1 $2 }
>   | EXPR '[' EXPR ']' { B "[]" $1 $3 }

> LISTAONADA
>  : EXPRLIST { $1 }
>  | { [] }


> EXPRLIST
>   : EXPRLIST ',' IDENT '=' EXPR { $1 ++ [(Just $3, $5)] }
>   | EXPRLIST ',' EXPR { $1 ++ [(Nothing, $3)] }
>   | EXPR { [(Nothing, $1)] }
>   | IDENT '=' EXPR { [(Just $1, $3)] }

> RETURNP : RETURN EXPR { Return $2 }
> CONTINUEP : CONTINUE { Continue }
> BREAKP : BREAK { Break }

> STRUCTDEF
>   : STRUCT IDENT '{' FIELDS '}' ';' { ($2, $4) }

> FIELDS
>   : FIELD { [$1] }
>   | FIELDS ',' FIELD { $1 ++ [$3] }

> FIELD
>   : LISTAIDENT ':' IDENT { ($1, $3) }

> LISTAIDENT
>   : IDENT { [$1] }
>   | LISTAIDENT ',' IDENT { $1 ++ [$3]}

> -- GLOBALVAR
> --  : STATIC LISTAVARIABLES ':' IDENT ';' { LVarDeclaration Static $4 $2 }
> --  | CONST LISTAVARIABLES ':' IDENT ';' { LVarDeclaration Const $4 $2 }

> LISTAVARIABLES
>   : IDENT INIT { [($1, $2)] }
>   | LISTAVARIABLES ',' IDENT INIT { $1 ++ [($3, $4)] }


> INIT
>   : '=' EXPR { Just $2 }
>   | {- empty -} { Nothing }

> PRINTP : PRINT EXPRLIST { Print $2 }
> GRABP : GRAB EXPR { Grab $2 }

> {
> data Type = Type String | ArrayOf String [Maybe Expr] deriving (Show)
> data Global = Function String [(String, Maybe Expr, Type)] Type [Instruction]
>              deriving (Show)
> data Expr = B String Expr Expr
>           | U String Expr
>           | Char String
>           | Number String
>           | Bool String
>           | Str String
>           | Var String
>           | FunctionCall Expr [Expr]
>           | TypeCast Expr String
>           | R Expr Expr Expr
>              deriving (Show)

> data Instruction = VarDeclaration VKind Type [(String, Maybe Expr)]
>                  | Break | Continue | Return Expr
>                  | Print [Expr] | Grab Expr deriving (Show)
> data VKind = VarKind | Const | Static deriving (Show)
> type Initialization = Maybe Expr
> happyError x = error "Error gramatical?"
> }

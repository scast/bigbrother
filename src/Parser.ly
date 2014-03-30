> {
> module Parser (parseTokens, Ident(..), Global(..), VKind(..), Type(..), Instruction(..), Expr(..)) where
> import Lexer
> import Control.Monad.Writer
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
> %monad { Parser }
> %error { happyError }

> %token
>    NUMBER   { L _ LNum $$ }
>    CHAR     { L _ LChar $$ }
>    BOOL     { L _ LBool $$ }
>    IDENT    { L _ LId _ }
>    STRING   { L _ LString $$ }
>    CONST    { L _ LConst s }
>    STATIC   { L _ LStatic s }
>    FN       { L _ LFn s }
>    VAR      { L _ LVar s }
>    BY       { L _ LBy s }
>    PRINT    { L _ LPrint s }
>    GRAB     { L _ LGrab s }
>    STRUCT   { L _ LStruct s }
>    UNION    { L _ LUnion s }
>    ENUM     { L _ LEnum s }
>    AS       { L _ LAs s }
>    TYPE     { L _ LType s }
>    IF       { L _ LIf s }
>    ELSE     { L _ LElse s }
>    LOOP     { L _ LLoop s }
>    FOR      { L _ LFor s }
>    WHILE    { L _ LWhile s }
>    BREAK    { L _ LBreak s }
>    CONTINUE { L _ LContinue s }
>    RETURN   { L _ LReturn s }
>    '&&'     { L _ LAnd $$ }
>    '||'     { L _ LOr $$ }
>    '!'      { L _ LNot $$ }
>    '^'      { L _ LXor $$ }
>    '='      { L _ LEqual s }
>    ':'      { L _ LColon s }
>    ';'      { L _ LSemicolon s }
>    '['      { L _ LOpenBracket s }
>    ']'      { L _ LCloseBracket s }
>    '{'      { L _ LOpenCurly s }
>    '}'      { L _ LCloseCurly s }
>    '('      { L _ LOpenParenthesis s }
>    ')'      { L _ LCloseParenthesis s }
>    '#'      { L _ LHash $$ }
>    '%'      { L _ LPercentage $$ }
>    ','      { L _ LComma s }
>    '.'      { L _ LDot s }
>    '..'     { L _ LDots s }
>    '=='     { L _ LEquals $$ }
>    '!='     { L _ LNotEquals $$ }
>    '>'      { L _ LGreater $$ }
>    '>='     { L _ LGreaterEqual $$ }
>    '<'      { L _ LLess $$ }
>    '<='     { L _ LLessEqual $$ }
>    '+'      { L _ LPlus $$ }
>    '-'      { L _ LMinus $$ }
>    '*'      { L _ LMul $$ }
>    '/'      { L _ LDiv $$ }
>    '**'     { L _ LExp $$ }
>    '>>'     { L _ LRShift $$ }
>    '<<'     { L _ LLShift $$ }
>    '+='     { L _ LPlusEqual s }
>    '-='     { L _ LMinusEqual s }
>    '*='     { L _ LMulEqual s }
>    '/='     { L _ LDivEqual s }
>    '%='     { L _ LModEqual s }
>    '>>='    { L _ LRShiftEqual s }
>    '<<='    { L _ LLShiftEqual s }
>    '&='     { L _ LBAndEqual s }
>    '|='     { L _ LBOrEqual s }
>    '^='     { L _ LXorEqual s }
>    '&&='    { L _ LAndEqual s }
>    '||='    { L _ LOrEqual s }
>    '|'      { L _ LBOr $$ }
>    '&'      { L _ LBAnd $$ }
>    '~'      { L _ LBNot $$ }
>    '@'      { L _  LAt $$ }

> %%

> INICIAL
>   : GLOBAL         { [$1] }
>   | INICIAL GLOBAL { $1 ++ [$2] }

> GLOBAL
>   : DECKIND LISTAVARIABLES ':' TYPESIMPLE ';' { GlobalVar $1 $4 $2 }
>   | TYPE IDENT '=' TYPESIMPLE ';'             { TypeDef (saveIdent $2) $4 }
>   | FUNCION                                   { $1 }
>   | TYPECOMBINE                               { DefCombine $1 }

> FUNCION
>   : FN IDENT '(' PARLISTONADA ')' ':' TYPESIMPLE BLOQUE { Function (saveIdent $2) $4 $7 $8 }
>   | FN IDENT '(' PARLISTONADA ')' BLOQUE { Function (saveIdent $2) $4 (Type $ (Ident "void" (-1) (-1))) $6 }

> PARLISTONADA
>   :                           { [] }
>   | POSITIONALP               { $1 }
>   | WDEFAULTS                 { $1 }
>   | POSITIONALP ',' WDEFAULTS { $1 ++ $3 }

> POSITIONALP
>   : POSITIONALP ',' IDENT ':' TYPESIMPLEREF { $1 ++ [(saveIdent $3, Nothing, $5)] }
>   | IDENT ':' TYPESIMPLEREF                 { [(saveIdent $1, Nothing, $3)] }

> WDEFAULTS
>   : WDEFAULTS ',' IDENT '=' EXPR ':' TYPESIMPLEREF { $1 ++ [(saveIdent $3, Just $5, $7)] }
>   | IDENT '=' EXPR ':' TYPESIMPLEREF               { [(saveIdent $1, Just $3, $5)]}

> TYPESIMPLEREF
>   : TYPESIMPLE     { $1 }
>   | TYPESIMPLE '&' { ReferenceTo $1 }

> TYPEN
>   : TYPESIMPLE  { $1 }
>   | TYPECOMBINE { $1 }

> TYPECOMBINE
>   : STRUCT IDENT '{' FIELDS '}'                 { TypeStruct (saveIdent $2) $4 }
>   | STRUCT IDENT '{' FIELDS '}' DIMENSIONS      { ArrayOf (TypeStruct (saveIdent $2) $4) $6 }
>   | UNION IDENT '{' FIELDS '}'                  { TypeUnion (saveIdent $2) $4 }
>   | UNION IDENT '{' FIELDS '}' DIMENSIONS       { ArrayOf (TypeUnion (saveIdent $2) $4) $6 }
>   | ENUM IDENT '{' LISTAVARIABLES '}'           { TypeEnum (saveIdent $2) $4 }
>   | ENUM IDENT '{' LISTAVARIABLES '}' DIMENSIONS { ArrayOf (TypeEnum (saveIdent $2) $4) $6 }

> TYPESIMPLE
>   : IDENT            { Type $ saveIdent $1 }
>   | IDENT DIMENSIONS { ArrayOf (Type $ saveIdent $1) $2 }

> DIMENSIONS
>   : '[' EXPR ']'            { [Just $2] }
>   | '[' ']'                 { [Nothing] }
>   | DIMENSIONS '[' EXPR ']' { $1 ++ [Just $3] }
>   | DIMENSIONS '[' ']'      { $1 ++ [Nothing] }

> INSTONADA
>   :          { [] }
>   | INSTLIST { $1 }

> INSTLIST
>   : INST          { [$1] }
>   | INSTLIST INST { $1 ++ [$2] }

> BLOQUE : '{' INSTONADA '}' { $2 }

> INST
>   : DEC            { $1 }
>   | ASIGNACION ';' { $1 }
>   | SELECTOR       { $1 }
>   | LOOPING        { $1 }
>   | RETURNP        { $1 }
>   | CONTINUE ';'   { Continue }
>   | BREAK ';'      { Break }
>   | PRINTP         { $1 }
>   | GRABP          { $1 }

> DEC : VAR LISTAVARIABLES ':' TYPESIMPLE ';' { LocalVar $4 $2 }

> DECKIND
>   : VAR    { VarKind }
>   | STATIC { Static }
>   | CONST  { Const }

> SELECTOR
>   : IFANIDADO             { If $1 }
>   | IFANIDADO ELSE BLOQUE { If ($1 ++ [(Nothing, $3)]) }

> IFANIDADO
>   : IF EXPR BLOQUE                { [(Just $2, $3)] }
>   | IFANIDADO ELSE IF EXPR BLOQUE { $1 ++ [(Just $4, $5)] }

> LOOPING
>   : LOOP BLOQUE                             { Loop $2 }
>   | WHILE EXPR BLOQUE                       { While $2 $3 }
>   | FOR TYPESIMPLEREF IDENT ':' EXPR BLOQUE { For $2 (saveIdent $3) $5 $6 }

> ASIGNACION
>   : IDENT '=' EXPR   { Assign "="  (saveIdent $1) $3 }
>   | IDENT '+=' EXPR  { Assign "+"  (saveIdent $1) $3 }
>   | IDENT '-=' EXPR  { Assign "-"  (saveIdent $1) $3 }
>   | IDENT '*=' EXPR  { Assign "*"  (saveIdent $1) $3 }
>   | IDENT '/=' EXPR  { Assign "/"  (saveIdent $1) $3 }
>   | IDENT '%=' EXPR  { Assign "%"  (saveIdent $1) $3 }
>   | IDENT '>>=' EXPR { Assign ">>" (saveIdent $1) $3 }
>   | IDENT '<<=' EXPR { Assign "<<" (saveIdent $1) $3 }
>   | IDENT '&=' EXPR  { Assign "&"  (saveIdent $1) $3 }
>   | IDENT '|=' EXPR  { Assign "|"  (saveIdent $1) $3 }
>   | IDENT '^=' EXPR  { Assign "^"  (saveIdent $1) $3 }
>   | IDENT '&&=' EXPR { Assign "&&" (saveIdent $1) $3 }
>   | IDENT '||=' EXPR { Assign "||" (saveIdent $1) $3 }

> EXPR
>   : CHAR                     { Char $1 }
>   | NUMBER                   { Number $1 }
>   | BOOL                     { Bool $1 }
>   | STRING                   { Str $1 }
>   | IDENT                    { Var (saveIdent $1) }
>   | IDENT '(' LISTAONADA ')' { FunctionCall (saveIdent $1) $3 }
>   | EXPR '.' EXPR            { B "." $1 $3  }
>   | EXPR AS IDENT            { TypeCast $1 (saveIdent $3) }
>   | '(' EXPR ')'             { $2 }
>   | EXPR '+' EXPR            { B $2 $1 $3 }
>   | EXPR '-' EXPR            { B $2 $1 $3 }
>   | EXPR '*' EXPR            { B $2 $1 $3 }
>   | EXPR '/' EXPR            { B $2 $1 $3 }
>   | EXPR '&&' EXPR           { B $2 $1 $3 }
>   | EXPR '||' EXPR           { B $2 $1 $3 }
>   | EXPR '^' EXPR            { B $2 $1 $3 }
>   | EXPR '==' EXPR           { B $2 $1 $3 }
>   | EXPR '!=' EXPR           { B $2 $1 $3 }
>   | EXPR '>' EXPR            { B $2 $1 $3 }
>   | EXPR '>=' EXPR           { B $2 $1 $3 }
>   | EXPR '<' EXPR            { B $2 $1 $3 }
>   | EXPR '<=' EXPR           { B $2 $1 $3 }
>   | EXPR '**' EXPR           { B $2 $1 $3 }
>   | EXPR '>>' EXPR           { B $2 $1 $3 }
>   | EXPR '<<' EXPR           { B $2 $1 $3 }
>   | EXPR '|' EXPR            { B $2 $1 $3 }
>   | EXPR '&' EXPR            { B $2 $1 $3 }
>   | EXPR '%' EXPR            { B $2 $1 $3 }
>   | EXPR '..' EXPR           { R $1 $3 (Number "1") }
>   | EXPR '..' EXPR BY EXPR   { R $1 $3 $5 }
>   | '#' EXPR                 { U $1 $2 }
>   | '@' EXPR                 { U $1 $2 }
>   | '~' EXPR                 { U $1 $2 }
>   | '-' EXPR %prec NEG       { U $1 $2  }
>   | '+' EXPR %prec PLUS      { U $1 $2  }
>   | '!' EXPR                 { U $1 $2 }
>   | EXPR '[' EXPR ']'        { B "[]" $1 $3 }

> LISTAONADA
>   :                        { [] }
>   | POSITIONAL             { $1 }
>   | BYNAMES                { $1 }
>   | POSITIONAL ',' BYNAMES { $1 ++ $3 }

> POSITIONAL
>   : POSITIONAL ',' EXPR { $1 ++ [(Nothing, $3)]}
>   | EXPR                { [(Nothing, $1)] }

> BYNAMES
>   : BYNAMES ',' IDENT '=' EXPR  { $1 ++ [(Just (saveIdent $3), $5)] }
>   | IDENT '=' EXPR              { [(Just (saveIdent $1), $3)] }

> EXPRLIST
>   : EXPRLIST ',' EXPR { $1 ++ [$3] }
>   | EXPR              { [$1] }

> RETURNP
>   : RETURN ';'      { Return Nothing }
>   | RETURN EXPR ';' { Return (Just $2) }

> FIELDS
>   : FIELD            { [$1] }
>   | FIELDS ',' FIELD { $1 ++ [$3] }

> FIELD : LISTAVARIABLES ':' TYPEN { ($3, $1) }

> LISTAVARIABLES
>   : IDENT INIT                    { [(saveIdent $1, $2)] }
>   | LISTAVARIABLES ',' IDENT INIT { $1 ++ [(saveIdent $3, $4)] }

> INIT
>   :          { Nothing }
>   | '=' EXPR { Just $2 }

> PRINTP : PRINT EXPRLIST ';' { Print $2 }

> GRABP : GRAB EXPR ';' { Grab $2 }

> {

> data Type = Type Ident
>           | ArrayOf Type [Maybe Expr]
>           | ReferenceTo Type
>           | TypeStruct Ident ListOfDef
>           | TypeUnion Ident ListOfDef
>           | TypeEnum Ident [Initialization]
>           deriving (Show)

> data Global = GlobalVar VKind Type [Initialization]
>             | TypeDef Ident Type
>             | Function Ident [(Ident, Maybe Expr, Type)] Type [Instruction]
>             | DefCombine Type
>             deriving (Show)

> data Expr = B String Expr Expr
>           | U String Expr
>           | Char String
>           | Number String
>           | Bool String
>           | Str String
>           | Var Ident
>           | FunctionCall Ident [(Maybe Ident, Expr)]
>           | TypeCast Expr Ident
>           | R Expr Expr Expr
>           deriving (Show)

> data Instruction = LocalVar Type [Initialization]
>                  | Assign String Ident Expr
>                  | If [(Maybe Expr, [Instruction])]
>                  | Loop [Instruction]
>                  | While Expr [Instruction]
>                  | For Type Ident Expr [Instruction]
>                  | Break
>                  | Continue
>                  | Return (Maybe Expr)
>                  | Print [Expr]
>                  | Grab Expr
>                  deriving (Show)

> data VKind = VarKind
>            | Const
>            | Static
>            deriving (Show, Ord, Eq)

> type Initialization = (Ident, Maybe Expr)
> type ListOfDef = [(Type, [Initialization])]

> data Ident = Ident { identName :: String, line :: Int, column :: Int }
>             deriving (Show, Ord, Eq)

> saveIdent :: Lexeme -> Ident
> saveIdent (L a l s) = Ident s line col
>     where (AlexPn _ line col) = a

> type ParseError = String
> type Parser = Either ParseError

> showPosn (AlexPn _ line col) = show line ++ ':': show col

> happyError :: [Lexeme] -> Parser a
> happyError tokens = Left $ "Snafu situation " ++ (show tokens)

> parseTokens tokens = parser tokens

> }

> {
> module Parser (Parser(..), parseTokens) where
> import Lexer
> import ParserTypes
> import Control.Monad.Writer
> import Control.Monad.Trans.Either
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
> %monad { Parser } { bindM } { returnM }
> %error { happyError }

> %token
>    NUMBER   { L _ LNum _ }
>    FLOAT   { L _ LFloat _ }
>    CHAR     { L _ LChar _ }
>    BOOL     { L _ LBool _ }
>    IDENT    { L _ LId _ }
>    STRING   { L _ LString _ }
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
>    '&&'     { L _ LAnd _ }
>    '||'     { L _ LOr _ }
>    '!'      { L _ LNot _ }
>    '^'      { L _ LXor _ }
>    '='      { L _ LEqual _ }
>    ':'      { L _ LColon s }
>    ';'      { L _ LSemicolon s }
>    '['      { L _ LOpenBracket s }
>    ']'      { L _ LCloseBracket s }
>    '{'      { L _ LOpenCurly s }
>    '}'      { L _ LCloseCurly s }
>    '('      { L _ LOpenParenthesis s }
>    ')'      { L _ LCloseParenthesis s }
>    '#'      { L _ LHash _ }
>    '%'      { L _ LPercentage _ }
>    ','      { L _ LComma s }
>    '.'      { L _ LDot s }
>    '..'     { L _ LDots _ }
>    '=='     { L _ LEquals _ }
>    '!='     { L _ LNotEquals _ }
>    '>'      { L _ LGreater _ }
>    '>='     { L _ LGreaterEqual _ }
>    '<'      { L _ LLess _ }
>    '<='     { L _ LLessEqual _ }
>    '+'      { L _ LPlus _ }
>    '-'      { L _ LMinus _ }
>    '*'      { L _ LMul _ }
>    '/'      { L _ LDiv _ }
>    '**'     { L _ LExp _ }
>    '>>'     { L _ LRShift _ }
>    '<<'     { L _ LLShift _ }
>    '+='     { L _ LPlusEqual _ }
>    '-='     { L _ LMinusEqual _ }
>    '*='     { L _ LMulEqual _ }
>    '/='     { L _ LDivEqual _ }
>    '%='     { L _ LModEqual _ }
>    '>>='    { L _ LRShiftEqual _ }
>    '<<='    { L _ LLShiftEqual _ }
>    '&='     { L _ LBAndEqual _ }
>    '|='     { L _ LBOrEqual _ }
>    '^='     { L _ LXorEqual _ }
>    '&&='    { L _ LAndEqual _ }
>    '||='    { L _ LOrEqual _ }
>    '|'      { L _ LBOr _ }
>    '&'      { L _ LBAnd _ }
>    '~'      { L _ LBNot _ }
>    '@'      { L _  LAt _ }

> %%

> INICIAL
>   : GLOBAL         {% returnM  ( [$1] ) }
>   | INICIAL GLOBAL {% returnM  ( $1 ++ [$2] ) }

> GLOBAL
>   : DECKIND LISTAVARIABLES ':' TYPESIMPLE ';' {% returnM  ( GlobalVar $1 $4 $2 ) }
>   | TYPE IDENT '=' TYPESIMPLE ';'             {% returnM  ( TypeDef (saveIdent $2) $4 ) }
>   | FUNCION                                   {% returnM  ( $1 ) }
>   | TYPECOMBINE ';'                               {% returnM  ( DefCombine $1 ) }

> FUNCION
>   : FN IDENT '(' PARLISTONADA ')' ':' TYPESIMPLE BLOQUE {% returnM  ( Function (saveIdent $2) $4 $7 $8 ) }
>   | FN IDENT '(' PARLISTONADA ')' BLOQUE {% returnM  ( Function (saveIdent $2) $4 (Type $ (Ident "void" (-1) (-1))) $6 ) }

> PARLISTONADA
>   :                           {% returnM  ( [] ) }
>   | POSITIONALP               {% returnM  ( $1 ) }
>   | WDEFAULTS                 {% returnM  ( $1 ) }
>   | POSITIONALP ',' WDEFAULTS {% returnM  ( $1 ++ $3 ) }

> POSITIONALP
>   : POSITIONALP ',' IDENT ':' TYPESIMPLEREF {% returnM  ( $1 ++ [(saveIdent $3, Nothing, $5)] ) }
>   | IDENT ':' TYPESIMPLEREF                 {% returnM  ( [(saveIdent $1, Nothing, $3)] ) }

> WDEFAULTS
>   : WDEFAULTS ',' IDENT '=' EXPR ':' TYPESIMPLEREF {% returnM  ( $1 ++ [(saveIdent $3, Just $5, $7)] ) }
>   | IDENT '=' EXPR ':' TYPESIMPLEREF               {% returnM  ( [(saveIdent $1, Just $3, $5)]) }

> TYPESIMPLEREF
>   : TYPESIMPLE     {% returnM  ( $1 ) }
>   | TYPESIMPLE '&' {% returnM  ( ReferenceTo $1 ) }

> TYPEN
>   : TYPESIMPLE  {% returnM  ( $1 ) }
>   | TYPECOMBINE {% returnM  ( $1 ) }

> TYPECOMBINE
>   : STRUCT IDENT '{' FIELDS '}'                 {% returnM  ( TypeStruct (saveIdent $2) $4 ) }
>   | STRUCT IDENT '{' error '}'                 {% tell ["Error de reconocimiento (shift) cerca de \"struct " ++ (str $2) ++ " { \" (" ++ (Lexer.showPosn (Lexer.pos $1)) ++ ")"]  >> returnM (TypeStruct (saveIdent $2) []) }
>   | UNION IDENT '{' error '}'                 {% tell ["Error de reconocimiento (shift) cerca de \"union " ++ (str $2) ++ " { \" (" ++ (Lexer.showPosn (Lexer.pos $1)) ++ ")"]  >> returnM (TypeUnion (saveIdent $2) []) }
-- >   | UNION IDENT '{' error '}'                 { TypeUnion (saveIdent $2) [] }
-- >   | STRUCT IDENT '{' FIELDS '}' DIMENSIONS      {% returnM  ( ArrayOf (TypeStruct (saveIdent $2) $4) $6 ) }
>   | UNION IDENT '{' FIELDS '}'                  {% returnM  ( TypeUnion (saveIdent $2) $4 ) }
-- >   | UNION IDENT '{' FIELDS '}' DIMENSIONS       {% returnM  ( ArrayOf (TypeUnion (saveIdent $2) $4) $6 ) }
>   | ENUM IDENT '{' LISTAVARIABLES '}'           {% returnM  ( TypeEnum (saveIdent $2) $4 ) }
>   | ENUM IDENT '{' error '}'                 {% tell ["Error de reconocimiento (shift) cerca de \"enum " ++ (str $2) ++ " { \" (" ++ (Lexer.showPosn (Lexer.pos $1)) ++ ")"]  >> returnM (TypeUnion (saveIdent $2) []) }
-- >   | ENUM IDENT '{' error '}'           { TypeEnum (saveIdent $2) [] }
-- >   | ENUM IDENT '{' LISTAVARIABLES '}' DIMENSIONS {% returnM  ( ArrayOf (TypeEnum (saveIdent $2) $4) $6 ) }

> TYPESIMPLE
>   : IDENT            {% returnM  ( Type $ saveIdent $1 ) }
>   | IDENT DIMENSIONS {% returnM  ( ArrayOf (Type $ saveIdent $1) $2 ) }

> DIMENSIONS
>   : '[' NUMBER ']'            {% returnM  ( [Just (Number (saveExprOp $2) (saveExprPos $2) )] ) }
>   | '[' NUMBER '..' NUMBER ']'            {% returnM  ([Just (R (Number (saveExprOp $2) (saveExprPos $2)) (Number (saveExprOp $4) (saveExprPos $4)) (Number "1" (-1, -1)) (saveExprPos $3) )]) }
-- >   | '[' ']'                 {% returnM  ( [Nothing] ) }
>   | DIMENSIONS '[' NUMBER ']' {% returnM  ( $1 ++ [Just (Number (saveExprOp $3) (saveExprPos $3))] ) }
>   | DIMENSIONS '[' NUMBER '..' NUMBER ']' {% returnM  ( $1 ++ [Just (R (Number (saveExprOp $3) (saveExprPos $3)) (Number (saveExprOp $5) (saveExprPos $5)) (Number "1" (-1, -1)) (saveExprPos $4) ) ] ) }
-- >   | DIMENSIONS '[' ']'      {% returnM  ( $1 ++ [Nothing] ) }

> INSTONADA
>   :          {% returnM  ( [] ) }
>   | INSTLIST {% returnM  ( $1 ) }

> INSTLIST
>   : INST          {% returnM  ( [$1] ) }
>   | INSTLIST INST {% returnM  ( $1 ++ [$2] ) }

> BLOQUE : '{' INSTONADA '}' {% returnM  ($2 ) }

> INST
>   : DEC ';'        {% returnM  ( $1 ) }
>   | ASIGNACION ';' {% returnM  ( $1 ) }
>   | SELECTOR       {% returnM  ( $1 ) }
>   | LOOPING        {% returnM  ( $1 ) }
>   | RETURNP ';'    {% returnM  ( $1 ) }
>   | CONTINUE ';'   {% returnM  ( Continue ) }
>   | BREAK ';'      {% returnM  ( Break ) }
>   | PRINTP ';'     {% returnM  ( $1 ) }
>   | GRABP ';'      {% returnM  ( $1 ) }
>   | VOIDCALL ';'   {% returnM  ( $1 ) }

> VOIDCALL : IDENT '(' LISTAONADA ')' {% returnM  ( VoidCall (saveIdent $1) $3 ) }

> DEC : VAR LISTAVARIABLES ':' TYPESIMPLE {% returnM  ( LocalVar $4 $2 ) }

> DECKIND
>   : VAR    {% returnM  ( VarKind ) }
>   | STATIC {% returnM  ( Static ) }
>   | CONST  {% returnM  ( Const ) }

> SELECTOR
>   : IFANIDADO             {% returnM  ( If $1 ) }
>   | IFANIDADO ELSE BLOQUE {% returnM  ( If ($1 ++ [(Nothing, $3)]) ) }

> IFANIDADO
>   : IF EXPR BLOQUE                {% returnM  ( [(Just $2, $3)] ) }
>   | IF error BLOQUE                {% tell ["Error de reconocimiento (shift) cerca de \"if\" (" ++ (Lexer.showPosn (Lexer.pos $1)) ++  ")"] >> returnM [(Nothing, $3)] }
>   | IFANIDADO ELSE IF EXPR BLOQUE {% returnM  ( $1 ++ [(Just $4, $5)] ) }

> LOOPING
>   : LOOP BLOQUE                             {% returnM  ( Loop $2 ) }
>   | WHILE EXPR BLOQUE                       {% returnM  ( While $2 $3 ) }
>   | WHILE error BLOQUE                       {% tell ["Error de reconocimiento (shift) cerca de \"while\" (" ++ (Lexer.showPosn (Lexer.pos $1)) ++  ")"] >> returnM (While (Str "error" (-1, -1) ) $3) }
>   | FOR TYPESIMPLEREF IDENT ':' EXPR BLOQUE {% returnM  ( For $2 (saveIdent $3) $5 $6 ) }

> ASIGNACION
>   : EXPR '=' EXPR   {% returnM  ( Assign "="  $1 $3 (saveExprPos $2) ) }
>   | EXPR '+=' EXPR  {% returnM  ( Assign "+"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '-=' EXPR  {% returnM  ( Assign "-"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '*=' EXPR  {% returnM  ( Assign "*"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '/=' EXPR  {% returnM  ( Assign "/"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '%=' EXPR  {% returnM  ( Assign "%"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '>>=' EXPR {% returnM  ( Assign ">>" $1 $3 (saveExprPos $2) ) }
>   | EXPR '<<=' EXPR {% returnM  ( Assign "<<" $1 $3 (saveExprPos $2) ) }
>   | EXPR '&=' EXPR  {% returnM  ( Assign "&"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '|=' EXPR  {% returnM  ( Assign "|"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '^=' EXPR  {% returnM  ( Assign "^"  $1 $3 (saveExprPos $2) ) }
>   | EXPR '&&=' EXPR {% returnM  ( Assign "&&" $1 $3 (saveExprPos $2) ) }
>   | EXPR '||=' EXPR {% returnM  ( Assign "||" $1 $3 (saveExprPos $2) ) }

> EXPR
>   : CHAR                     {% returnM  ( Char (saveExprOp $1) (saveExprPos $1) ) }
>   | NUMBER                   {% returnM  ( Number (saveExprOp $1) (saveExprPos $1) ) }
>   | FLOAT                   {% returnM  ( Float (saveExprOp $1) (saveExprPos $1) ) }
>   | BOOL                     {% returnM  ( Bool (saveExprOp $1) (saveExprPos $1) ) }
>   | STRING                   {% returnM  ( Str (saveExprOp $1) (saveExprPos $1) ) }
>   | IDENT                    {% returnM  ( Var (saveIdent $1) ) }
>   | IDENT '(' LISTAONADA ')' {% returnM  ( FunctionCall (saveIdent $1) $3 ) }
>   | EXPR '.' IDENT           {% returnM  ( Field $1 (saveIdent $3) ) }
>   | EXPR AS IDENT            {% returnM  ( TypeCast $1 (saveIdent $3) ) }
>   | '(' EXPR ')'             {% returnM  ( $2 ) }
>   | EXPR '+' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '-' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '*' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '/' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '&&' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '||' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '^' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '==' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '!=' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '>' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '>=' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '<' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '<=' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '**' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '>>' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '<<' EXPR           {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '|' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '&' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | EXPR '%' EXPR            {% returnM  ( B (saveExprOp $2) $1 $3 (saveExprPos $2)) }
>   | NUMBER '..' NUMBER       {% returnM  ( R (Number (saveExprOp $1) (saveExprPos $1)) (Number (saveExprOp $3) (saveExprPos $3)) (Number "1" (-1, -1)) (saveExprPos $2) ) }
>   | '#' EXPR                 {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | '@' EXPR                 {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | '~' EXPR                 {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | '-' EXPR %prec NEG       {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | '+' EXPR %prec PLUS      {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | '!' EXPR                 {% returnM  ( U (saveExprOp $1) $2 (saveExprPos $1) ) }
>   | EXPR '[' EXPR ']'        {% returnM  ( B "[]" $1 $3 (getPos $1) ) }
-- >   | '(' error ')' { Str "error" }

> LISTAONADA
>   :                        {% returnM  ( [] ) }
>   | POSITIONAL             {% returnM  ( $1 ) }
-- >   | BYNAMES                {% returnM  ( $1 ) }
-- >   | POSITIONAL ',' BYNAMES {% returnM  ( $1 ++ $3 ) }

> POSITIONAL
>   : POSITIONAL ',' EXPR {% returnM  ( $1 ++ [(Nothing, $3)]) }
>   | EXPR                {% returnM  ( [(Nothing, $1)] ) }

> BYNAMES
>   : BYNAMES ',' IDENT '=' EXPR  {% returnM  ( $1 ++ [(Just (saveIdent $3), $5)] ) }
>   | IDENT '=' EXPR              {% returnM  ( [(Just (saveIdent $1), $3)] ) }

> EXPRLIST
>   : EXPRLIST ',' EXPR {% returnM  ( $1 ++ [$3] ) }
>   | EXPR              {% returnM  ( [$1] ) }

> RETURNP
>   : RETURN      {% returnM  ( Return Nothing ) }
>   | RETURN EXPR {% returnM  ( Return (Just $2) ) }

> FIELDS
>   : FIELD            {% returnM  ( [$1] ) }
>   | FIELDS ',' FIELD {% returnM  ( $1 ++ [$3] ) }

> FIELD : LISTAVARIABLES ':' TYPEN {% returnM  ( ($3, $1) ) }

> LISTAVARIABLES
>   : IDENT INIT                    {% returnM  ( [(saveIdent $1, $2)] ) }
>   | LISTAVARIABLES ',' IDENT INIT {% returnM  ( $1 ++ [(saveIdent $3, $4)] ) }

> INIT
>   :          {% returnM  ( Nothing ) }
>   | '=' EXPR {% returnM  ( Just $2 ) }

> PRINTP : PRINT EXPRLIST {% returnM  ( Print $2 ) }

> GRABP : GRAB EXPR {% returnM   ( Grab $2 ) }

> {


> returnM = return
> bindM = (>>=)

> saveIdent :: Lexeme -> Ident
> saveIdent (L a l s) = Ident s line col
>     where (AlexPn _ line col) = a

> saveExprPos :: Lexeme -> (Int, Int)
> saveExprPos (L a l s) = (line, col)
>     where (AlexPn _ line col) = a

> saveExprOp :: Lexeme -> String
> saveExprOp (L a l s) = s

> type ParseError = String
> type Parser = EitherT (ParseError, [Lexeme]) (Writer [ParseError])

> showPosn (AlexPn _ line col) = show line ++ ':': show col

> happyError :: [Lexeme] -> Parser a
> happyError tokens = failM ("Error en reconocimiento: lexema inesperado", tokens)

> failM a = EitherT $ return (Left a)

> parseTokens tokens = parser tokens

> }

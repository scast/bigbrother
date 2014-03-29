{
module Lexer (Lexeme(..), LexemeClass(..), AlexPosn(..), lexer) where
import Control.Monad.Writer
import Data.Either
}


%wrapper "monad"

$alpha = [a-zA-Z]
$digits = 0-9

tokens :-
  "//" .* ;
  $white+ ;
  $digits+ { mkL LNum  } -- common integer
  [0-9][0-9_]* { mkL LNum } -- readability
  [0-9][0-9\.]* "e" [0-9]+ { mkL LNum } -- scientific notation
  "0b" [0-7]+ { mkL LNum } -- octal notation
  "0x" [0-9abcdef]+ { mkL LNum } -- hex notation
  $digits+ "." $digits+ { mkL LNum } -- floating point
  "'" . "'" { mkL LChar }
  "true" { mkL LBool }
  "false" { mkL LBool  }
  [\"] .* [\"] { mkL LString }
  "fn" { mkL LFn }
  ":" { mkL LColon }
  ";" { mkL LSemicolon }
  "[" { mkL LOpenBracket }
  "]" { mkL LCloseBracket }
  "{"  { mkL LOpenCurly }
  "}" { mkL LCloseCurly }
  "(" { mkL LOpenParenthesis }
  ")" { mkL LCloseParenthesis }
  "by" { mkL LBy }
  "const" { mkL LConst }
  "static" { mkL LStatic }
  "var" { mkL LVar }
  "print!" { mkL LPrint }
  "grab!" { mkL LGrab }
  "struct" { mkL  LStruct }
  "union" { mkL LUnion }
  "enum" { mkL LEnum }
  "#" { mkL LHash}
  "%" { mkL LPercentage }
--  "unsigned" { mkL LUnsigned }
  "," { mkL LComma }
  "as" { mkL LAs }
  "type" { mkL LType }
  ".." { mkL LDots }
  "." { mkL LDot}
  "if" { mkL LIf }
  "else" { mkL LElse }
  "loop" { mkL LLoop }
  "for" { mkL LFor }
  "while" { mkL LWhile }
  "break" { mkL LBreak }
  "continue" { mkL LContinue }
  "==" { mkL LEquals }
  "!=" { mkL LNotEquals }
  "=" { mkL LEqual }
  "return" { mkL LReturn }
  "&&=" { mkL LAndEqual }
  "&&" { mkL LAnd }
  "and" { mkL LAnd }
  "||=" { mkL LOrEqual }
  "||" { mkL LOr }
  "or" { mkL LOr }
  "!" { mkL LNot }
  "not" { mkL LNot }
  ">>=" { mkL LRShiftEqual }
  ">>" { mkL LRShift }
  "<<=" { mkL LLShiftEqual }
  "<<" { mkL LLShift }
  ">=" { mkL LGreaterEqual }
  "<=" { mkL LLessEqual  }
  "<" { mkL LGreater }
  ">" { mkL LLess }
  -- "++" { mkL LIncrease }
  "+=" { mkL LPlusEqual }
  -- "--" { mkL LDecrease }
  "+" { mkL LPlus }
  "-=" { mkL LMinusEqual }
  "-" { mkL LMinus }
  "*=" { mkL LMulEqual }
  "**" { mkL LExp }
  "*" { mkL LMul }
  "/=" { mkL LDivEqual }
  "/" { mkL LDiv }
  "&=" { mkL LBAndEqual }
  "&" { mkL LBAnd }
  "|=" { mkL LBOrEqual }
  "|" { mkL LBOr }
  "^=" { mkL LXorEqual }
  "^" { mkL LXor }
  "~" { mkL LBNot }
  "@" { mkL LAt }
  [$digits $alpha _][$digits $alpha _!]* { mkL LId }

  . { mkE }

{

data Lexeme = L { pos :: AlexPosn,
                  lclass :: LexemeClass,
                  str :: String }
            deriving (Show)

data LexemeClass = LNum | LChar | LBool
           | LId  | LString  | LFn | LColon | LSemicolon
           | LOpenBracket | LCloseBracket | LOpenCurly | LCloseCurly
           | LOpenParenthesis | LCloseParenthesis
           | LConst | LBy | LStatic | LVar | LPrint | LGrab
           | LStruct | LUnion | LEnum | LHash | LPercentage | LUnsigned
           | LComma | LAs | LType | LDot | LDots | LIf | LElse
           | LLoop | LFor | LWhile | LBreak | LContinue | LEqual
           | LReturn | LAnd | LOr | LNot | LXor | LEquals | LNotEquals
           | LGreater | LGreaterEqual | LLess | LLessEqual | LPlus | LMinus
           | LMul | LDiv | LExp | LIncrease | LDecrease
           | LRShift | LLShift | LPlusEqual | LMinusEqual | LMulEqual
           | LDivEqual | LModEqual | LRShiftEqual | LLShiftEqual
           | LBAndEqual | LBOrEqual | LXorEqual | LAndEqual | LOrEqual
           | LBOr | LBAnd | LBNot | LAt | LEof
           deriving (Show, Read, Eq)


type LexError = String
mkL :: LexemeClass -> AlexInput -> Int -> Alex (Either LexError Lexeme)
mkL c (p,_,_,str) len = return (Right (L p c (take len str)))

showPosn (AlexPn _ line col) = show line ++ ':': show col

mkE :: AlexInput -> Int -> Alex (Either LexError Lexeme)
mkE (pos,_,_,str) len = return (Left ("Error lexico: encontrado '"++ (take len str) ++ "' (" ++ (showPosn pos) ++ ")"))

alexEOF :: Alex (Either LexError Lexeme)
alexEOF = return (Right (L undefined LEof ""))


type Lexer = WriterT [Either LexError Lexeme] Alex

getToken :: Lexer (Either LexError Lexeme)
getToken = do
  inp <- lift alexGetInput
  sc <- lift alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> do eof <- lift alexEOF
                  return eof
    AlexError inp ->
      error "Really unexpected error... Can't recover. Mayday and all that jazz."
    AlexSkip inp' len -> do
      lift (alexSetInput inp')
      getToken
    AlexToken inp' len action -> do
      lift (alexSetInput inp')
      lexm <- lift $ action (ignorePendingBytes inp) len
      return lexm

scanner :: Lexer ()
scanner =  do
  tkn <- getToken
  case tkn of
    Right (L _ LEof _) -> return ()
    _ -> do tell [tkn]
            scanner -- go again

lexer :: String -> ([LexError], [Lexeme])
lexer input  = partitionEithers $ runAlex input $ execWriterT scanner

}

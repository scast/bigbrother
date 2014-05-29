module ParserTypes ( Type(..)
                   , Global(..)
                   , Expr(..)
                   , VKind(..)
                   , Instruction(..)
                   , Initialization(..)
                   , ListOfDef(..)
                   , Ident(..)
                   , getPos
                   ) where
data Type = Type Ident
          | ArrayOf Type [Maybe Expr]
          | ReferenceTo Type
          | TypeStruct { typeIdent :: Ident, listDef :: ListOfDef}
          | TypeUnion { typeIdent :: Ident, listDef :: ListOfDef}
          | TypeEnum { typeIdent :: Ident, init :: [Initialization]}
          deriving (Show)

data Instruction = LocalVar Type [Initialization]
                 | Assign String Expr Expr
                 | If [(Maybe Expr, [Instruction])]
                 | Loop [Instruction]
                 | While Expr [Instruction]
                 | For Type Ident Expr [Instruction]
                 | Break
                 | Continue
                 | Return (Maybe Expr)
                 | Print [Expr]
                 | Grab Expr
                 | VoidCall Ident [(Maybe Ident, Expr)]
                 deriving (Show)


data Global = GlobalVar VKind Type [Initialization]
            | TypeDef Ident Type
            | Function Ident [(Ident, Maybe Expr, Type)] Type [Instruction]
            | DefCombine Type
            deriving (Show)

data Expr = B String Expr Expr (Int, Int)
          | U String Expr (Int, Int)
          | Field Expr Ident
          | Char String
          | Number String
          | Float String
          | Bool String
          | Str String
          | Var Ident
          | FunctionCall Ident [(Maybe Ident, Expr)]
          | TypeCast Expr Ident
          | R Expr Expr Expr (Int, Int)
          deriving (Show)

data VKind = VarKind
           | Const
           | Static | EnumVar | ExtendedTypeVar
           deriving (Show, Ord, Eq)

type Initialization = (Ident, Maybe Expr)
type ListOfDef = [(Type, [Initialization])]

data Ident = Ident { identName :: String
                   , line :: Int
                   , column :: Int }
           deriving (Show, Ord, Eq)

getPos :: Expr -> (Int, Int)
getPos (B _ _ _ x) = x
getPos (U _ _ x) = x
getPos (Field _ e) = (line e, column e)
getPos (Var e) = (line e, column e)
getPos (FunctionCall ident _) = (line ident, column ident)
getPos (TypeCast _ ident) = (line ident, column ident)
getPos _ = ((-1), (-1))

module ParserTypes ( Type(..)
                   , Global(..)
                   , Expr(..)
                   , VKind(..)
                   , Instruction(..)
                   , Initialization(..)
                   , ListOfDef(..)
                   , Ident(..)
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

data Expr = B String Expr Expr
          | U String Expr
          | Field Expr Ident
          | Char String
          | Number String
          | Bool String
          | Str String
          | Var Ident
          | FunctionCall Ident [(Maybe Ident, Expr)]
          | TypeCast Expr Ident
          | R Expr Expr Expr
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

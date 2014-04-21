{-# LANGUAGE TemplateHaskell #-}
module Table ( buildM
             , buildTable
             , empty
             , lookup
             , GeneratorState(..)
             , Symbol(..)
             , Table(..)
             ) where
import qualified ParserTypes as P
import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes, maybe)
import Control.Monada.RWS
import Control.Lens -- have no fear.


data Extended = Struct | Enum | Union
              deriving (Show, Eq, Ord)

data Symbol = TypeDeclaration { ident :: P.Ident
                              , {- Type -} table :: Table }
            | Variable { ident :: P.Ident
                       , vkind :: P.VKind } -- Type}
            | Function { ident :: P.Ident
                       ,  {- Type -} table :: Table }
            deriving (Show, Eq, Ord)


data Table = Table { _mapping :: M.Map String Symbol
                   , _sons :: [Table] }
             deriving (Show, Ord, Eq)

data GeneratorState = GeneratorState { _current :: Table
                                     , _path :: [Table] }
                      deriving (Show, Eq, Ord)

type Generator b a = RWS b [String] GeneratorState  a

makeLenses ''Table
makeLenses ''GeneratorState

-- An empty symbol table.
empty :: Table
empty = Table { _mapping = M.empty
              , _sons = [] }

-- Look up a symbol on the set of active tables
lookup :: String -> [Table] -> Maybe Symbol
lookup k xs = xs ^? traverse.mapping.ix k

-- Checks if a symbol is in the track of tables
member :: String -> [Table] -> Bool
member k xs = maybe False (\x -> True) $ lookup k xs

-- Default symbols in the language.
languageSymbols = M.fromList [
  --- Integers
  ("int8", TypeDeclaration (P.Ident "int8" (-1) (-1))  empty),
  ("int16", TypeDeclaration (P.Ident "int16" (-1) (-1)) empty),
  ("int32", TypeDeclaration (P.Ident "int32" (-1) (-1)) empty),
  ("int64", TypeDeclaration (P.Ident "int64" (-1) (-1)) empty),
  ("int",  TypeDeclaration (P.Ident "int" (-1) (-1)) empty),
  ("long", TypeDeclaration  (P.Ident "long" (-1) (-1)) empty),

  --- Float
  ("float32", TypeDeclaration (P.Ident "float32" (-1) (-1)) empty),
  ("float64", TypeDeclaration (P.Ident "float64" (-1) (-1)) empty),
  ("float", TypeDeclaration  (P.Ident "float" (-1) (-1)) empty),
  ("double", TypeDeclaration  (P.Ident "double" (-1) (-1)) empty),

  -- Characters
  ("char", TypeDeclaration (P.Ident "char" (-1) (-1)) empty),

  -- Booleans
  ("bool", TypeDeclaration (P.Ident "bool" (-1) (-1)) empty),
  ("void", TypeDeclaration (P.Ident "void" (-1) (-1)) empty),

  -- String
  ("string", TypeDeclaration (P.Ident "string" (-1) (-1)) empty)
  ]

languageTable :: Table
languageTable = Table { _mapping = languageSymbols
                      , _sons = [] }

-- Print position where this symbol was defined
showPosition :: Symbol -> String
showPosition sym = let id = ident sym
                   in show (P.line id) ++ ":" ++ show (P.column id)

-- Get a type from the tables using its name or store an error.
findType :: String -> Generator b (Maybe Symbol)
findType k = do
  st <- get
  case lookup k (st^.current:st^.path) of
    result@(Just (TypeDeclaration _ _)) -> return result
    x -> tell ["Unknown type " ++ k] >> return x

-- Get a type from the tables using its type structure or store an
-- error.
typeSymbol :: P.Type -> Generator b (Maybe Symbol)
typeSymbol = findType . typename
  where typename (P.Type (P.Ident x _ _)) = x
        typename (P.ArrayOf x _) = typename x
        typename (P.ReferenceTo x ) = typename x
        typename (P.TypeStruct (P.Ident x _ _) _) = x
        typename (P.TypeEnum (P.Ident x _ _) _) = x
        typename (P.TypeUnion (P.Ident x _ _) _) = x


-- Check if a symbol exists otherwise stores an error.
checkExists :: String -> Generator b ()
checkExists name = do
  st <- get
  if name `member` (st^.current:st^.path)
  then return ()
  else tell ["Symbol " ++ (name) ++ "has not been defined."]

-- Check if a symbol has already been defined.
checkNotExists :: String -> Generator b () -> Generator b ()
checkNotExists k cb = do
  st <- get
  case lookup k (st^.current:st^.path) of
    Just symbol -> tell ["Symbol " ++ k ++ "has already been defined at "
                         ++ showPosition symbol]
    Nothing  -> cb

-- Check if an expression is OK.
checkExpr :: P.Expr -> Generator b ()
checkExpr (P.B "." l r) = checkExpr l
checkExpr (P.B _ l r) = checkExpr l >> checkExpr r
checkExpr (P.U _ u) = checkExpr u
checkExpr (P.Char _) = return ()
checkExpr (P.Number _) = return ()
checkExpr (P.Bool _) = return ()
checkExpr (P.Str _) = return ()
checkExpr (P.Var (P.Ident name _ _ )) = checkExists name
checkExpr (P.FunctionCall (P.Ident name _ _) _) = checkExists name
checkExpr (P.TypeCast e (P.Ident name _ _)) = checkExpr e >> checkExists name
checkExpr (P.R l r step) = checkExpr l >> checkExpr r >> checkExpr step


-- Check that `ptype` exists and check current scope for the existence
-- of the new symbol name. If `ptype` exists and the symbol name
-- doesn't, then go ahead and create the new symbol.
check_ :: String -> P.Type -> Generator b () -> Generator b ()
check_ name ptype callback = do
  st <- get
  stype <- typeSymbol ptype
  -- Check if ptype exists
  case (stype, st^.current.mapping.at name) of
    (_, Just symbol) -> tell ["Symbol " ++ name ++ "has already been defined at "
                              ++ showPosition symbol]
    (Just symbol, Nothing) -> callback
    (_, Nothing) -> return ()

-- Checks whether or not a symbol has been defined already in the
-- current scope.
check :: String -> Generator b () -> Generator b ()
check name callback = do
  st <- get
  case st^.current.mapping.at name of
    Just symbol -> tell ["Symbol " ++ name ++ "has already been defined at "
                         ++ showPosition symbol]
    Nothing -> callback

-- Add a new variable to the current table or store an error on failure.
addVariable :: P.Ident -> P.VKind -> P.Type -> Generator b ()
addVariable ident@(P.Ident name _ _) kind ptype =
  check_ name ptype $ do
    let newSymbol = Variable ident kind
    current.mapping.at name ?= newSymbol

-- Adds a new typedef to the current table or stores an error on failure.
addTypedef :: P.Ident -> P.Type -> Generator b ()
addTypedef ident@(P.Ident name _ _) ptype =
  check_ name ptype $ do
    let newSymbol = TypeDeclaration ident empty
    current.mapping.at name ?= newSymbol

-- Adds a new extended function (shallow pass) to the current table or
-- stores an error on failure.
addShallowFunction :: P.Ident -> P.Type -> Generator b ()
addShallowFunction ident@(P.Ident name _ _) ptype =
  check_ name ptype $ do
    let newSymbol = Function ident empty
    current.mapping.at name ?= newSymbol

-- Adds a new extended type (shallow pass) to the current table or
-- stores an error on failure.
addShallowType :: P.Type -> Generator b ()
addShallowType ptype = do
  let (ident, ext) = case ptype of
        P.TypeStruct id _ -> (id, Struct)
        P.TypeEnum id _ -> (id, Enum)
        P.TypeUnion id _ -> (id, Union)
  checkNotExists (P.identName ident) $ do
    let newSymbol = TypeDeclaration ident empty
        name = P.identName ident
    current.mapping.at name ?= newSymbol

-- Do a shallow pass over the global definitions and adds them to the
-- table
shallowPass :: Generator [P.Global] ()
shallowPass = do
  tree <- ask
  forM_ tree $ \x ->
    case x of
      P.GlobalVar kind tipo lista ->
        forM_ lista $ \(ident, init) -> do
          addVariable ident kind tipo
          case init of
            Nothing -> return ()
            Just expr -> checkExpr expr

      P.TypeDef ident tipo ->
        addTypedef ident tipo

      P.Function ident _ tipo _ ->
        addShallowFunction ident tipo

      P.DefCombine tipo ->
        addShallowType tipo

-- Build a new table in the new scope.
trackAndBuild :: Table -> [P.Instruction] -> Generator b ()
trackAndBuild table tree = do
  st <- get
  -- Create a new table
  let action = do
        insts <- ask
        forM_ insts handleInstruction
      (final, acc) =
        buildTable action tree (GeneratorState table (st^.current:st^.path))
  tell acc
  current.sons <>= [final^.current]

-- Handle each kind of instruction.
handleInstruction :: P.Instruction -> Generator b ()
handleInstruction inst = case inst of
  -- Local variable declaration (and initialization)
  P.LocalVar tipo inits ->
    forM_ inits $ \(ident, init) -> do
      addVariable ident P.VarKind tipo
      case init of
        Nothing -> return ()
        Just expr -> checkExpr expr

  -- Assignment
  P.Assign _ left expr ->
    void (checkExpr left >> checkExpr expr)

  -- Grab instruction
  P.Grab expr ->
    checkExpr expr

  -- Print instruction
  P.Print exprs ->
    forM_ exprs checkExpr

  -- Conditional
  P.If blocks -> forM_ blocks $ \block ->
    case block of
      (Just e, insts) -> checkExpr e >> trackAndBuild empty insts
      (Nothing, insts) -> trackAndBuild empty insts

  -- While loop
  P.While expr insts ->
    checkExpr expr >> trackAndBuild empty insts

  -- For loop
  P.For ptype ident@(P.Ident name _ _) expr insts -> do
    typeSymbol ptype
    let newTable = M.singleton name (Variable ident P.VarKind)
    checkExpr expr
    trackAndBuild (Table newTable []) insts

  -- Return instruction
  P.Return (Just expr) -> checkExpr expr

  -- A function call
  P.VoidCall (P.Ident name _ _) inits -> do
    checkExists name
    forM_ inits $ \(_, expr) -> checkExpr expr

  -- Everything else doesn't matter.
  _ -> return ()


handleFunction :: P.Global -> Generator [P.Global] ()
handleFunction (P.Function ident parameters ptype insts) = do
  forM_ parameters $ \(pident, init, ptype) -> do
    -- Check and add this symbol.
    check (P.identName pident) $ do
      let newSymbol = Variable pident P.VarKind
      current.mapping.at (P.identName pident) ?= newSymbol

    -- Check possible initializations.
    case init of
      Nothing -> return ()
      Just expr -> checkExpr expr

    -- Perform checks on the return type.
    typeSymbol ptype
    case ptype of
      P.ArrayOf tipo dims -> do let ndims = catMaybes dims
                                mapM_ checkExpr ndims
      _ -> return ()

    -- Handle each instruction of this function.
    forM_ insts handleInstruction

handleComplexType :: P.Type -> Generator b () -> Generator b ()
handleComplexType tipo callback = do
  st <- get
  case tipo of
    P.Type _ -> callback

    P.ArrayOf x dims -> do
      handleComplexType x callback
      let ndims = catMaybes dims
      mapM_ checkExpr ndims

    _ -> do
      addShallowType tipo
      let initial = (GeneratorState empty (st^.current:st^.path))
          (s, w) = buildTable (handleType tipo) [] initial
      tell w
      current.sons <>= [s^.current]
      let tident = P.typeIdent tipo
          name = P.identName tident
      current.mapping.at name ?= (TypeDeclaration tident (s^.current))
      callback


handleListDef :: [(P.Type, [P.Initialization])] -> Generator b ()
handleListDef xs =
  forM_ xs $ \(tipo, inits) -> do
    handleComplexType tipo $ do
      forM_ inits $ \(tident@(P.Ident name _ _), init) -> do
        addVariable tident P.ExtendedTypeVar tipo


handleType :: P.Type -> Generator b ()
handleType ptype = case ptype of
  P.TypeEnum ident inits ->
    forM_ inits $ \(tident@(P.Ident name _ _), init) -> do
      checkNotExists name $
        addVariable tident P.EnumVar (P.Type (P.Ident "int32" (-1) (-1)))
  P.TypeStruct ident xs -> handleListDef xs
  P.TypeUnion ident xs -> handleListDef xs


-- Build a new table from a given a starting tree, a monadic action to
-- perform and the starting state.
buildTable :: Generator a () -> a -> GeneratorState -> (GeneratorState, [String])
buildTable = execRWS

addGlobals :: Table -> Generator b ()
addGlobals newTable = do
  st <- get
  let shallow = M.foldlWithKey f M.empty (newTable^.mapping)
      f accum key x@(Variable _ P.EnumVar) = M.insert key x accum
      f accum key x@(TypeDeclaration _ _) = M.insert key x accum
      f accum _ _ = accum
  current.mapping .= M.union (st^.current.mapping) shallow
  mapM_ addGlobals (newTable^.sons)

handleGlobal :: P.Global -> Generator b ()
handleGlobal global@(P.Function ident@(P.Ident name _ _) vars ptype insts) = do
  st <- get
  let initial = GeneratorState empty [st^.current, languageTable]
      (s, w) = buildTable (handleFunction global) [global] initial
  tell w
  current.sons <>= [s^.current]
  current.mapping.at name ?= (Function ident (s^.current))

handleGlobal global@(P.DefCombine tipo) = do
  st <- get
  let initial = GeneratorState empty [st^.current, languageTable]
      (s, w) = buildTable (handleType tipo) [global] initial
  tell w
  current.sons <>= [s^.current]
  let name = (P.identName (P.typeIdent tipo))
  current.mapping.at name ?= (TypeDeclaration (P.typeIdent tipo) (s^.current))
  addGlobals (s^.current)

handleGlobal _ = return ()


-- Builds our symbol table recursively.
buildM :: Generator [P.Global] ()
buildM = do
  tree <- ask
  -- Perform a shallow pass to recollect
  -- global level symbols
  current .= empty
  path .= [languageTable]
  shallowPass

  -- Perform a full (recursive) pass now.
  st <- get
  forM_ tree handleGlobal

{-# LANGUAGE TemplateHaskell #-}
module Table ( buildM
             , buildTable
             , empty
             , lookup
             , sons
             , current
             , mapping
             , GeneratorState(..)
             , Symbol(..)
             , Table(..)
             ) where
import qualified ParserTypes as P
import qualified Data.Map as M
import qualified TypeChecking as T
import Data.List (partition)
import Control.Arrow (first)
import Prelude hiding (lookup)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes, maybe, fromJust)
import Control.Monad.RWS
import Control.Lens hiding (mapping) -- have no fear.


data Extended = Struct | Enum | Union
              deriving (Show, Eq, Ord)

data Symbol = TypeDeclaration { ident :: P.Ident
                              , table :: Table
                              , stype :: T.Type }
            | Variable { ident :: P.Ident
                       , vkind :: P.VKind
                       , stype :: T.Type
                       }
            | Function { ident :: P.Ident
                       , table :: Table
                       , stype :: T.Type
                       }
            deriving (Show, Eq, Ord)


data Table = Table { _mapping :: M.Map String Symbol
                   , _sons :: [Table] }
             deriving (Show, Ord, Eq)

data GeneratorState = GeneratorState { _current :: Table
                                     , _path :: [Table] }
                      deriving (Show, Eq, Ord)

type Generator b a = RWS b [String] GeneratorState  a

-- Lens me maybe.
makeLenses ''Table
makeLenses ''GeneratorState

-- | An empty symbol table.
empty :: Table
empty = Table { _mapping = M.empty
              , _sons = [] }

-- | Look up a symbol on the set of active tables
lookup :: String -> [Table] -> Maybe Symbol
lookup k xs = xs ^? traverse.mapping.ix k

-- | Checks if a symbol is in the track of tables
member :: String -> [Table] -> Bool
member k xs = maybe False (\x -> True) $ lookup k xs

-- | Default symbols in the language.
languageSymbols = M.fromList [
  --- Integers
  ("int",  TypeDeclaration (P.Ident "int" (-1) (-1)) empty T.Int32),
  --- Float
  ("float", TypeDeclaration  (P.Ident "float" (-1) (-1)) empty T.Float),
  -- Characters
  ("char", TypeDeclaration (P.Ident "char" (-1) (-1)) empty T.Char),
  -- Booleans
  ("bool", TypeDeclaration (P.Ident "bool" (-1) (-1)) empty T.Bool),
  -- Void
  ("void", TypeDeclaration (P.Ident "void" (-1) (-1)) empty T.Void)
  ]

-- | Default language table.
languageTable :: Table
languageTable = Table { _mapping = languageSymbols
                      , _sons = [] }

-- | Print position where this symbol was defined
showPosition :: Symbol -> String
showPosition sym = let id = ident sym
                   in show (P.line id) ++ ":" ++ show (P.column id)

-- | Get a type from the tables using its name or store an error.
findType :: String -> Generator b (Maybe Symbol)
findType k = do
  st <- get
  case lookup k (st^.current:st^.path) of
    result@(Just (TypeDeclaration _ _ _)) -> return result
    x -> tell ["Unknown type " ++ k] >> return x

-- | Get a type from the tables using its type structure or store an
-- error.
typeSymbol :: P.Type -> Generator b (Maybe Symbol)
typeSymbol = findType . typename

typename (P.Type (P.Ident x _ _)) = x
typename (P.ArrayOf x _) = typename x
typename (P.ReferenceTo x ) = typename x
typename (P.TypeStruct (P.Ident x _ _) _) = x
typename (P.TypeEnum (P.Ident x _ _) _) = x
typename (P.TypeUnion (P.Ident x _ _) _) = x


-- | Check if a symbol exists otherwise stores an error.
checkExists :: String -> Generator b ()
checkExists name = do
  st <- get
  if name `member` (st^.current:st^.path)
  then return ()
  else tell ["Symbol " ++ (name) ++ "has not been defined."]

-- | Check if a symbol has already been defined.
checkNotExists :: String -> Generator b () -> Generator b ()
checkNotExists k cb = do
  st <- get
  case lookup k (st^.current:st^.path) of
    Just symbol -> tell ["Symbol " ++ k ++ "has already been defined at "
                         ++ showPosition symbol]
    Nothing  -> cb

-- | Check if an expression is OK.
checkExpr :: P.Expr -> Generator b ()
checkExpr (P.Field e (P.Ident name _ _ )) = checkExpr e -- >> checkExists name
checkExpr (P.B _ l r) = checkExpr l >> checkExpr r
checkExpr (P.U _ u) = checkExpr u
checkExpr (P.Char _) = return ()
checkExpr (P.Number _) = return ()
checkExpr (P.Float _) = return ()
checkExpr (P.Bool _) = return ()
checkExpr (P.Str _) = return ()
checkExpr (P.Var (P.Ident name _ _ )) = checkExists name
checkExpr (P.FunctionCall (P.Ident name _ _) _) = checkExists name
checkExpr (P.TypeCast e (P.Ident name _ _)) = checkExpr e >> checkExists name
checkExpr (P.R l r step) = checkExpr l >> checkExpr r >> checkExpr step


-- | Check that `ptype` exists and check current scope for the existence
-- of the new symbol name. If `ptype` exists and the symbol name
-- doesn't, then go ahead and create the new symbol.
check_ :: String -> P.Type -> (Symbol -> Generator b ()) -> Generator b ()
check_ name ptype callback = do
  st <- get
  stype <- typeSymbol ptype
  -- Check if ptype exists
  case (stype, st^.current.mapping.at name) of
    (_, Just symbol) -> tell ["Symbol " ++ name ++ "has already been defined at "
                              ++ showPosition symbol]
    (Just symbol, Nothing) -> callback symbol
    (_, Nothing) -> return ()

-- | Checks whether or not a symbol has been defined already in the
-- current scope.
check :: String -> Generator b () -> Generator b ()
check name callback = do
  st <- get
  case st^.current.mapping.at name of
    Just symbol -> tell ["Symbol " ++ name ++ "has already been defined at "
                         ++ showPosition symbol]
    Nothing -> callback

-- | Add a new variable to the current table or store an error on failure.
addVariable :: P.Ident -> P.VKind -> P.Type -> Generator b ()
addVariable ident@(P.Ident name _ _) kind ptype =
  check_ name ptype $ \foundSymbol -> do
    case foundSymbol of
      TypeDeclaration _ _ stype -> do
        let newSymbol = Variable ident kind stype
        current.mapping.at name ?= newSymbol
      _ -> tell ["Tipo desconocido"] -- FIXME: better description

-- | Adds a new typedef to the current table or stores an error on failure.
addTypedef :: P.Ident -> P.Type -> Generator b ()
addTypedef ident@(P.Ident name _ _) ptype =
  check_ name ptype $ \foundSymbol -> do
    case foundSymbol of
      TypeDeclaration _ _ stype -> do
        let newSymbol = TypeDeclaration ident empty (T.TypeDef stype)
        current.mapping.at name ?= newSymbol
      _ -> tell ["Tipo desconocido"] -- FIXME: better description

-- | Adds a new extended function (shallow pass) to the current table or
-- stores an error on failure.
addShallowFunction :: P.Ident -> P.Type -> Generator b ()
addShallowFunction ident@(P.Ident name _ _) ptype =
  check_ name ptype $ \_ -> do
    let newSymbol = Function ident empty (T.Function [T.Void] T.Void)
    current.mapping.at name ?= newSymbol

-- | Adds a new extended type (shallow pass) to the current table or
-- stores an error on failure.
addShallowType :: P.Type -> Generator b ()
addShallowType ptype = do
  let (ident, ext, nt) = case ptype of
        P.TypeStruct id _ -> (id, Struct, T.Record [])
        P.TypeEnum id _ -> (id, Enum, T.Enum [])
        P.TypeUnion id _ -> (id, Union, T.Union [])
  checkNotExists (P.identName ident) $ do
    let newSymbol = TypeDeclaration ident empty nt
        name = P.identName ident
    current.mapping.at name ?= newSymbol

-- | Do a shallow pass over the global definitions and adds them to the
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

-- | Build a new table in the new scope of the instruction.
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

-- | Handle each kind of instruction and perform static checks.
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
    let newTable = M.singleton name (Variable ident P.VarKind T.Int32) -- FIXME: change to iterating type
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


-- | Handle a function at the global level.
handleFunction :: P.Global -> Generator [P.Global] ()
handleFunction (P.Function ident parameters ptype insts) = do
  forM_ parameters $ \(pident, init, ptype) -> do
    -- Check and add this symbol.
    check (P.identName pident) $ do
      let newSymbol = Variable pident P.VarKind T.Int32 -- FIXME: change to correct type
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

-- | Handle a complex (extended) type declaration.
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
      oldPath <- use path
      path <>= [s^.current]
      sst <- computeFields tipo
      path .= oldPath
      current.mapping.at name ?= (TypeDeclaration tident (s^.current) sst) -- FIXME: FIXME
      callback

-- | Handle the list of definitions in a type (struct or union)
-- declaration.
handleListDef :: [(P.Type, [P.Initialization])] -> Generator b ()
handleListDef xs =
  forM_ xs $ \(tipo, inits) -> do
    handleComplexType tipo $ do
      forM_ inits $ \(tident@(P.Ident name _ _), init) -> do
        addVariable tident P.ExtendedTypeVar tipo

-- | Handle a type declaration.
handleType :: P.Type -> Generator b ()
handleType ptype = case ptype of
  P.TypeEnum ident inits ->
    forM_ inits $ \(tident@(P.Ident name _ _), init) -> do
      checkNotExists name $
        addVariable tident P.EnumVar (P.Type (P.Ident "int" (-1) (-1)))
  P.TypeStruct ident xs -> handleListDef xs
  P.TypeUnion ident xs -> handleListDef xs


-- | Build a new table from a given a starting tree, a monadic action to
-- perform and the starting state.
buildTable :: Generator a () -> a -> GeneratorState -> (GeneratorState, [String])
buildTable = execRWS

-- | Adds the new symbols from the declaration of types or enumeration
-- variables.
addGlobals :: Table -> Generator b ()
addGlobals newTable = do
  st <- get
  let shallow = M.foldlWithKey f M.empty (newTable^.mapping)
      f accum key x@(Variable _ P.EnumVar _) = M.insert key x accum
      f accum key x@(TypeDeclaration _ _ _) = M.insert key x accum -- FIXME
      f accum _ _ = accum
  current.mapping .= M.union (st^.current.mapping) shallow
  mapM_ addGlobals (newTable^.sons)

f' (ptype, defs) = do
  sstype <- typeSymbol ptype
  return [(P.identName ident, stype (fromJust sstype)) | (ident, _) <- defs]

computeFields ::  P.Type -> Generator b T.Type
computeFields (P.TypeStruct ident defs) = do
  types <- mapM f' defs
  return (T.Record (concat types))

computeFields (P.TypeUnion ident defs) = do
  types <- mapM f' defs
  return (T.Union (concat types))

computeFields (P.TypeEnum _ inits) = do
  let fields = map (P.identName . fst) inits
  return (T.Enum fields)

-- Record (P.identName ident) fields
  -- where fields =


-- | Handle a global instruction. Only functions and extended types
-- need to be considered since typedefs and variable declarations were
-- handled on shallow pass.
handleGlobal :: P.Global -> Generator b ()
handleGlobal global@(P.Function ident@(P.Ident name _ _) vars ptype insts) = do
  st <- get
  let initial = GeneratorState empty [st^.current, languageTable]
      (s, w) = buildTable (handleFunction global) [global] initial
  tell w
  current.sons <>= [s^.current]
  leMapping <- use (current.mapping)
  let funcActual = fromJust (M.lookup name leMapping)
  current.mapping.at name ?= (funcActual { table = s^.current })

-- | In the special case of type declarations, it must link typenames
-- and enum variables to the global scope.
handleGlobal global@(P.DefCombine tipo) = do
  st <- get
  let initial = GeneratorState empty [st^.current, languageTable]
      (s, w) = buildTable (handleType tipo) [global] initial
  tell w
  current.sons <>= [s^.current]
  let name = (P.identName (P.typeIdent tipo))
  oldPath <- use path
  path <>= [s^.current]
  sst <- computeFields tipo
  path .= oldPath
  current.mapping.at name ?= (TypeDeclaration (P.typeIdent tipo) (s^.current) sst) -- TODO: stress test
  addGlobals (s^.current)

handleGlobal _ = return ()

fixSignatures  = mapM fix where
  fix func@(P.Function ident@(P.Ident name _ _) vars ptype _) = do
    returnType <- typeSymbol ptype
    domain <- mapM (\(_, _, x) -> typeSymbol x) vars
    current.mapping.at name ?= (Function ident empty (T.Function
                                                     (map (maybe T.Void stype) domain)
                                                     (maybe T.Void stype returnType)))
    return func

isTypeGlobal (P.DefCombine _)  = True
isTypeGlobal _ = False

isFunctionGlobal (P.Function _ _ _ _)  = True
isFunctionGlobal _ = False

-- | Builds our symbol table recursively. Initially, performs a
-- shallow pass to gather global symbols which allows calling
-- functions and global types not declared yet. Then, performs a full
-- pass over the parsed tree.
buildM :: Generator [P.Global] ()
buildM = do
  tree <- ask
  -- Perform a shallow pass to recollect
  -- global level symbols
  current .= empty
  path .= [languageTable]
  shallowPass

  -- Perform a full (recursive) pass now.
  -- First deal with types
  let (typeGlobal, restGlobal) = partition isTypeGlobal tree
  forM_ typeGlobal handleGlobal

  -- Before dealing with functions, we must fix their signatures since
  -- we stored a placeholder.
  fixedRest <- fixSignatures (filter isFunctionGlobal restGlobal)
  forM_ fixedRest handleGlobal

  -- -- Make sure the language table has the current table (the global
  -- -- table) as a son
  track <- use path
  path .= []
  globalTable <- use current
  current .= head track
  current.sons <>= [globalTable]

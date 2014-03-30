module Table (build, emptyTable, GeneratorState(..), Symbol(..), Table(..)) where
import qualified Parser
import qualified Data.Map as M
import Control.Monad.RWS
import Control.Monad
import Data.Maybe (mapMaybe, listToMaybe)


data Extended = Struct | Enum | Union
              deriving (Show, Eq, Ord)

-- data Type = Primitive Parser.Ident
--           | Typedef Parser.Ident Type
--           | Combined Parser.Ident Extended Table
--           | Array Type
--           | Reference Type
--           deriving (Show, Eq, Ord)

data Symbol = TypeDeclaration Parser.Ident {- Type -} Table
            | Variable Parser.Ident Parser.VKind -- Type
            | Function Parser.Ident {- Type -} Table
            deriving (Show, Eq, Ord)




data Table = Table { mapping :: M.Map String Symbol
                   , sons :: [Table] }
             deriving (Show, Ord, Eq)

data GeneratorState = GeneratorState { current :: Table
                                     , path :: [Table] }
                      deriving (Show, Eq, Ord)

type Generator b a = RWS b [String] GeneratorState  a

emptyTable = Table M.empty []
standardSymbols = [("int8", TypeDeclaration (Parser.Ident "int8" (-1) (-1))
                             emptyTable)
                ,("int16", TypeDeclaration (Parser.Ident "int16" (-1) (-1))
                            emptyTable)
                ,("int32", TypeDeclaration (Parser.Ident "int32" (-1) (-1))
                            emptyTable)
                ,("int64", TypeDeclaration (Parser.Ident "int64" (-1) (-1))
                            emptyTable)
                ,("float32", TypeDeclaration (Parser.Ident "float32" (-1) (-1))
                              emptyTable)
                ,("float64", TypeDeclaration (Parser.Ident "float64" (-1) (-1))
                              emptyTable)
                ,("char", TypeDeclaration (Parser.Ident "char" (-1) (-1))
                          emptyTable)
                ,("bool", TypeDeclaration (Parser.Ident "bool" (-1) (-1))
                          emptyTable)
                ,("void", TypeDeclaration (Parser.Ident "void" (-1) (-1))
                          emptyTable)
                ,("int",  TypeDeclaration (Parser.Ident "int" (-1) (-1))
                          emptyTable)
                ,("long", TypeDeclaration  (Parser.Ident "long" (-1) (-1))
                          emptyTable)
                ,("float", TypeDeclaration  (Parser.Ident "float" (-1) (-1))
                           emptyTable)
                ,("double", TypeDeclaration  (Parser.Ident "double" (-1) (-1))
                            emptyTable)
                ,("string", TypeDeclaration (Parser.Ident "string" (-1) (-1))
                            emptyTable)
                ]
-- standardSymbols = map f standardTypes
--   where f (s, t) = (s, TypeDeclaration t)

-- Adds to the table the default language symbols
addLangSymbols :: Generator b ()
addLangSymbols = do
  let langTable = M.fromList standardSymbols
  modify (\s -> s { current = Table langTable [] })

-- buildType :: Parser.Type -> (Maybe Symbol) -> (Maybe Type)
-- buildType _ Nothing = Nothing
-- buildType (Parser.Type _) (Just (TypeDeclaration x)) = Just x
-- buildType (Parser.ArrayOf _ _) (Just (TypeDeclaration x) ) = Just (Array x)

getTypeOrError :: String -> Generator b (Maybe Symbol)
getTypeOrError typename = do
  table <- gets (current)
  track <- gets (path)
  case symbolLookup ((mapping table):(map mapping track)) typename of
    x@(Just (TypeDeclaration _ _)) -> return x
    x -> tell ["Tipo desconocido " ++ typename] >> return x

-- performs a check on the current scope and runs callback if none exists.
check :: String -> Generator b () -> Generator b ()
check name callback = do
  table <- gets (current)
  case M.lookup name (mapping table) of
    Just symbol -> tell ["Simbolo " ++ (name) ++ " ya definido en este alcance"]
    Nothing -> callback

--
checkExists :: String -> Generator b ()
checkExists name = do
  table <- gets (current)
  track <- gets (path)
  case symbolLookup ((mapping table):(map mapping track)) name of
    Just _ -> return ()
    Nothing -> tell ["Simbolo " ++ (name) ++ " no ha sido definido"]

checkExpr :: Parser.Expr -> Generator b ()
checkExpr (Parser.B _ l r) = checkExpr l >> checkExpr r
checkExpr (Parser.U _ u) = checkExpr u
checkExpr (Parser.Char _) = return ()
checkExpr (Parser.Number _) = return ()
checkExpr (Parser.Bool _) = return ()
checkExpr (Parser.Str _) = return ()
checkExpr (Parser.Var (Parser.Ident name _ _ )) = checkExists name
checkExpr (Parser.FunctionCall (Parser.Ident name _ _) _) = checkExists name
checkExpr (Parser.TypeCast e (Parser.Ident name _ _)) = checkExpr e
                                                        >> checkExists name
checkExpr (Parser.R l r step) = checkExpr l >> checkExpr r >> checkExpr step

-- performs a check for type and id, stores error if any, and runs
-- callback on everything ok
checkAndPerform :: String -> Parser.Type -> (Table -> Generator b ())
                   -> Generator b ()
checkAndPerform name ptype callback= do
  table <- gets (current)
  track <- gets (path)
  let typename = getSimpleTypename ptype
  ttipo <- getTypeOrError typename
  case (ttipo, M.lookup name (mapping table)) of
    (_, Just _) -> tell ["Simbolo " ++ name ++ " ya definido en este alcance."]
    (Just tipo, Nothing) -> callback table
    (_, Nothing) -> return ()

-- Adds a new variable to the current table or stores error.
addVariable :: Parser.Ident -> Parser.VKind -> Parser.Type -> Generator b ()
addVariable ident@(Parser.Ident name _ _) kind ptype =
  checkAndPerform name ptype $ \table -> do
  let newSymbol = Variable ident kind
  modify (\s -> s { current = table { mapping = M.insert name newSymbol
                                                (mapping table)} })

-- Adds a new typedef to the current table or stores error.
addTypedef :: Parser.Ident -> Parser.Type -> Generator b ()
addTypedef ident@(Parser.Ident name _ _) ptype =
  checkAndPerform name ptype $ \table -> do
    let newSymbol = TypeDeclaration ident emptyTable
    modify (\s -> s { current = table { mapping = M.insert name newSymbol
                                                  (mapping table)} })


-- adds a new function (shallow pass) to the current table or stores the error.
addShallowFunction :: Parser.Ident -> Parser.Type -> Generator b ()
addShallowFunction ident@(Parser.Ident name _ _) ptype =
  checkAndPerform name ptype $ \table -> do
    let newSymbol = Function ident emptyTable
    modify (\s -> s { current = table { mapping = M.insert name newSymbol
                                                  (mapping table)} })

-- adds a new extended type (shallow pass) to the current table or
-- stores the error.
addShallowType :: Parser.Type -> Generator b ()
addShallowType ptype = do
  table <- gets (current)
  track <- gets (path)
  let (ident, ext) = case ptype of
        Parser.TypeStruct id _ -> (id, Struct)
        Parser.TypeEnum id _ -> (id, Enum)
        Parser.TypeUnion id _ -> (id, Union)
  check (Parser.identName ident) $ do
    let newSymbol = TypeDeclaration ident emptyTable
        name = Parser.identName ident
    modify (\s -> s { current = table { mapping = M.insert name newSymbol
                                                  (mapping table)} })

symbolLookup :: [M.Map String Symbol] -> String -> Maybe Symbol
symbolLookup tables symbolname = listToMaybe $ mapMaybe (M.lookup symbolname) tables

getSimpleTypename (Parser.Type (Parser.Ident x _ _)) = x
getSimpleTypename (Parser.ArrayOf x _) = getSimpleTypename x
getSimpleTypename (Parser.ReferenceTo x ) = getSimpleTypename x

-- Do a shallow pass over the global definitions and add them to the table
shallowPass :: Generator [Parser.Global] ()
shallowPass = do
  table <- gets (current)
  track <- gets (path)
  tree <- ask
  forM_ tree $ \x -> do
    case x of
      Parser.GlobalVar kind tipo lista -> do
        -- there's nothing else to check, just add it to the table now.
        forM_ lista $ \(ident, init) -> do
          addVariable ident kind tipo
          case init of
            Nothing -> return ()
            Just expr -> checkExpr expr

      Parser.TypeDef ident tipo -> do
        -- there's nothing else to check, just add it to the table now.
        addTypedef ident tipo
      Parser.Function ident _ tipo _ ->
        addShallowFunction ident tipo
      Parser.DefCombine tipo ->
        addShallowType tipo

-- Builds a table given the current track and the initial table
buildTableGlobal :: [Parser.Global] -> GeneratorState
                    -> (GeneratorState, [String])
buildTableGlobal = execRWS buildTableGlobal'

buildTableGlobal' :: Generator [Parser.Global] ()
buildTableGlobal' = do
  [global] <- ask
  case global of
    Parser.Function _ _ _  _ -> handleFunction global
    _ -> error "Snafu Situation. This really shouldn't happen..."

handleFunction :: Parser.Global -> Generator [Parser.Global] ()
handleFunction (Parser.Function ident parameters ptype instList) = do
  -- add parameters if at all possible.
  forM_ parameters $ \(pident, init, ptype) -> do
    check (Parser.identName pident) $ do
      table <- gets (current)
      let newSymbol = Variable pident Parser.VarKind
      modify (\s -> s { current = table {
                           mapping = M.insert (Parser.identName pident)
                                     newSymbol (mapping table)}
                      }
             )
    case init of
      Nothing -> return ()
      Just expr -> checkExpr expr
    getTypeOrError (getSimpleTypename ptype)
  -- Handle each instruction
  forM_ instList $ \inst -> handleInstruction inst

recursiveInstructionBuild ntable instlist = do
  -- create a new table
  track <- gets (path)
  table <- gets (current)
  let (final, acc) =
        buildTableInstruction instlist (GeneratorState ntable (table:track))
  tell acc
  -- set this new table as son of this current table.
  let newSon = current final
      currentSons = sons table
  modify (\s -> s { current = table { sons = currentSons ++ [newSon]  } })


handleInstruction inst = do
  case inst of
    Parser.LocalVar tipo inits ->
      forM_ inits $ \(ident, init) -> do
        addVariable ident Parser.VarKind tipo
        case init of
          Nothing -> return ()
          Just expr -> checkExpr expr
    Parser.Assign _ (Parser.Ident name _ _) expr ->
      void (checkExists name >>  checkExpr expr)
    Parser.Grab expr ->
      checkExpr expr
    Parser.Print exprlist ->
      forM_ exprlist $ \expr -> checkExpr expr
    Parser.If blocks -> forM_ blocks $ \block -> do
      case block of
        (Just e, instlist) -> checkExpr e
                              >> recursiveInstructionBuild emptyTable instlist
        (Nothing, instlist) -> recursiveInstructionBuild emptyTable instlist
    Parser.While expr instlist -> do
      checkExpr expr
      recursiveInstructionBuild emptyTable instlist
    Parser.Loop  instlist -> recursiveInstructionBuild emptyTable instlist
    Parser.For ptype ident@(Parser.Ident name _ _) expr instlist -> do
      getTypeOrError (getSimpleTypename ptype)
      let newTable = M.insert name (Variable ident Parser.VarKind) M.empty
      checkExpr expr
      recursiveInstructionBuild (Table newTable []) instlist
    Parser.Return (Just expr) -> checkExpr expr
    Parser.VoidCall (Parser.Ident name _ _) _ -> checkExists name
    _ -> return ()



buildTableInstruction :: [Parser.Instruction] -> GeneratorState
                         -> (GeneratorState, [String])
buildTableInstruction = execRWS buildTableInstruction'

buildTableInstruction' = do
  instList <- ask
  forM_ instList $ \inst -> handleInstruction inst

-- Build the symbol table
build :: Generator [Parser.Global] ()
build = do
  addLangSymbols
  tree <- ask
  langTable <- gets (current)
  -- make a shallow pass to collect symbols on global level
  modify (\s -> s { current = Table M.empty [], path = [langTable] })
  shallowPass

  -- full pass now.
  forM_ tree $ \global -> do
    case global of
      Parser.Function ident@(Parser.Ident name _ _) vars ptype instlist -> do
        globalTable <- gets (current)
        let (s, w) = buildTableGlobal [global] (GeneratorState emptyTable [globalTable, langTable])
        tell w
        let newSon = current s
            currentSons = sons globalTable
        modify (\s -> s { current =
                             globalTable { sons = currentSons ++ [newSon ]
                                         , mapping =
                                              M.insert name
                                              (Function ident newSon)
                                              (mapping globalTable)}
                        }
               )
      _ -> return ()
  -- make the global table a son of langtable
  globalTable <- gets (current)
  track <- gets (path)
  modify (\s -> s { path = [] , current = (track !! 0) { sons = [globalTable ]}})

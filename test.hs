import Language.C
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.System.GCC

import System.Exit
import Data.List

{-
parseC :: InputStream -> Position -> Either ParseError CTranslUnit
parseC input initialPosition =
  fmap fst $ execParser translUnitP input initialPosition builtinTypeNames (namesStartingFrom 0)

parseExtDecl :: InputStream -> Position -> Either ParseError CExtDecl
parseExtDecl input initialPosition =
  fmap fst $ execParser extDeclP input initialPosition builtinTypeNames (namesStartingFrom 0)

parseStmt :: InputStream -> Position -> Either ParseError CStat
parseStmt input initialPosition =
  fmap fst $ execParser statementP input initialPosition builtinTypeNames (namesStartingFrom 0)

readExt = do
    source <- readInputStream "./ext.c"
    print $ parseExtDecl source $ initPos "./ext.c"
-}

data EntryType = IdDecl | IdRef | IdCall | IdLabel deriving (Eq, Show)
-- Not meaningful, just in case of sorting for searching
instance Ord EntryType where
    IdDecl  `compare` _         = LT
    IdRef   `compare` IdDecl    = GT
    IdRef   `compare` _         = LT
    IdCall  `compare` IdLabel   = LT
    IdCall  `compare` _         = GT
    IdLabel `compare` _         = GT

-- | IdEntryVal stores the information about a symbol:
--  (file, row, column, type)
type IdEntryVal = (String, Int, Int, EntryType)
-- | (ident, key)
type IdEntry = (String, IdEntryVal)

identToEntry :: Ident -> EntryType -> IdEntry
identToEntry ident entry_type =
    let id_name = (identToString ident) in
    let id_file = case fileOfNode ident of
                    Nothing -> ""
                    Just p -> p in
    let id_pos = posOfNode $ nodeInfo ident in
    let id_row = posRow $ id_pos in
    let id_col = posColumn $ id_pos in
    (id_name, (id_file, id_row, id_col, entry_type))

-- Just use linear search as the size of the local list should be handy
inLocalList :: [IdEntry] -> String -> Bool
inLocalList _ "true" = True
inLocalList _ "false" = True
inLocalList [] id_name = False
inLocalList ((e_name, _):xs) id_name =
    if e_name == id_name
        then True
        else inLocalList xs id_name

parseDeclList :: [IdEntry] -> [IdEntry] -> [(Maybe (CDeclarator a0), b0, c0)] ->
    ([IdEntry], [IdEntry])
parseDeclList gl ll [] = (gl, ll)
parseDeclList gl ll ((cDeclr, cInit, cExp):xs) = case cDeclr of
    Nothing -> (gl, ll)
    Just (CDeclr (Just ident) _ _ _ _) ->
        case ll of
            [] -> let gl' = (identToEntry ident IdDecl) : gl in parseDeclList gl' ll xs
            _ -> let ll' = (identToEntry ident IdDecl) : ll in parseDeclList gl ll' xs

parseCSU :: [IdEntry] -> [IdEntry] -> CStructureUnion a -> ([IdEntry], [IdEntry])
parseCSU gl ll (CStruct _ mident mdecl _ _) = case mident of
    Just ident ->
        -- struct variable declarations are always indexed
        let gl' = ((identToEntry ident IdDecl) : gl) in
        case mdecl of
            Just declL -> (parseStructDeclList gl' declL, ll)
            _ -> (gl', ll)
    _ -> (gl, ll) -- TODO: this needs to be fixed as anoymous struct can have fields
    where
        -- struct fields are always indexed
        parseStructDeclList gl' [] = gl'
        parseStructDeclList gl' (x:xs) =
            let (dl, _) = (parseDecl gl' [] x) in parseStructDeclList dl xs

parseCType :: [IdEntry] -> [IdEntry] -> [CDeclarationSpecifier a] ->
    [(Maybe (CDeclarator a0), b0, c0)] -> ([IdEntry], [IdEntry])
parseCType gl ll [] _ = (gl, ll)
parseCType gl ll (cType:xs) declList = case cType of
    -- struct or union
    CTypeSpec (CSUType (csu) _) ->
        let (gl', ll') = parseCSU gl ll csu in
        case declList of
            [] -> (gl', ll')
            _ -> parseDeclList gl' ll' declList
    -- other types
    _ -> parseDeclList gl ll declList

--parseDecl :: CDeclaration a -> IdEntry
-- parseDecl gl (CDecl cTypes [] info) = gl -- void function arguments
parseDecl :: [IdEntry] -> [IdEntry] -> (CDeclaration a) ->
    ([IdEntry], [IdEntry])
parseDecl gl ll (CDecl cTypeList declrList _) =
    parseCType gl ll cTypeList declrList

-- expr and stmt won't introduce new symbols
parseExprList gl ll [] _ = gl
parseExprList gl ll (expr:xs) id_type =
    let gl' = parseExpr gl ll expr id_type in
    parseExprList gl' ll xs id_type

parseExpr gl ll expr id_type = case expr of
    CComma exprList _ -> parseExprList gl ll exprList IdRef
    CAssign _ expr1 expr2 _ ->
        parseExpr2 gl ll (expr1, IdRef) (expr2, IdRef)
    CCond expr1 Nothing expr2 _ ->
        parseExpr2 gl ll (expr1, IdRef) (expr2, IdRef)
    CCond expr1 (Just expr2) expr3 _ ->
        parseExpr3 gl ll (expr1, IdRef) (expr2, IdRef) (expr3, IdRef)
    CBinary _ expr1 expr2 _ ->
        parseExpr2 gl ll (expr1, IdRef) (expr2, IdRef)
    CCast _ expr _ -> parseExpr gl ll expr IdRef
    CUnary _ expr _ -> parseExpr gl ll expr IdRef
    CSizeofExpr expr _ -> parseExpr gl ll expr IdRef
    CIndex expr1 expr2 _ ->
        parseExpr2 gl ll (expr1, IdRef) (expr2, IdRef)
    CCall expr exprList _ ->
        -- callee must be defined so ll can't be changed
        let gl' = parseExpr gl ll expr IdCall in
        parseExprList gl' ll exprList IdRef
    CMember struct field _ _ -> -- field :: Ident is always indexed
        let gl' = parseExpr gl ll struct IdRef in
        (identToEntry field IdRef) : gl'
    CVar ident _ ->
        -- if the ident is a local variable, just discard it
        if inLocalList ll (identToString ident)
            then gl
            else (identToEntry ident id_type) : gl
    _ -> gl

parseExpr2 gl ll (expr1, t1) (expr2, t2) =
    let gl' = parseExpr gl ll expr1 t1 in
    parseExpr gl' ll expr2 t2

parseExpr3 gl ll exprt1 exprt2 (expr3, t3)  =
    let gl' = parseExpr2 gl ll exprt1 exprt2 in
    parseExpr gl' ll expr3 t3

parseStmt :: [IdEntry] -> [IdEntry] -> (CStatement a) -> [IdEntry]
parseStmt gl ll stmt = case stmt of
    CLabel label stmt _ _ ->
        let gl' = (identToEntry label IdLabel) : gl in
        parseStmt gl' ll stmt
    CCase expr stmt _ ->
        let gl' = parseExpr gl ll expr IdRef in
        parseStmt gl' ll stmt
    CCases expr1 expr2 stmt _ ->
        let gl' = parseExpr2 gl ll (expr1, IdRef) (expr2, IdRef) in
        parseStmt gl' ll stmt
    CDefault stmt _ ->
        parseStmt gl ll stmt
    CExpr (Just expr) _ ->
        parseExpr gl ll expr IdRef
    CCompound label compoundItems _ ->
        parseCompound gl ll label compoundItems
    CIf expr stmt Nothing _ ->
        let gl' = parseExpr gl ll expr IdRef in
        parseStmt gl' ll stmt
    CIf expr stmt1 (Just stmt2) _ ->
        let gl' = parseExpr gl ll expr IdRef in
        let gl'' = parseStmt gl' ll stmt1 in
        parseStmt gl'' ll stmt2
    CSwitch expr stmt _ ->
        let gl' = parseExpr gl ll expr IdRef in
        parseStmt gl' ll stmt
    CWhile expr stmt _ _ ->
        let gl' = parseExpr gl ll expr IdRef in
        parseStmt gl' ll stmt
    CFor _ _ _ _ _ -> parseCFor stmt
    CGoto label _ ->
        (identToEntry label IdLabel) : gl
    CReturn (Just expr) _ ->
        parseExpr gl ll expr IdRef
    _ -> gl
    where
        mParseExpr gl ll mexpr = case mexpr of
            Nothing -> Just gl
            Just expr -> Just (parseExpr gl ll expr IdRef)
        parseCFor (CFor (Left mexpr1) (mexpr2) (mexpr3) stmt _) =
             case mParseExpr gl ll mexpr1 >>= \gl1 ->
                  (mParseExpr gl1 ll) mexpr1 of
                Nothing -> gl
                Just gl3 -> parseStmt gl3 ll stmt
        parseCFor (CFor _ _ _ _ _) = gl

-- dummy local list used to indicate we are in local scope
-- dummyll :: [IdEntry]
-- dummyll = [("", "", 0, 0, IdDecl)]

-- C code compound, gl is global symbol list, ll is local symbol list
-- Updates to a local symbol in a compound is discarded when the compound
-- is parsed
parseCompound :: [IdEntry] -> [IdEntry] -> [Ident] -> [CCompoundBlockItem a]
    -> [IdEntry]
parseCompound gl ll labels [] = gl -- end of parsing, ll is discarded
parseCompound gl ll labels (blockItem:xs) = case blockItem of
    CBlockStmt stmt -> -- Stmt won't introduce new symbols
        let gl' = parseStmt gl ll stmt in
        parseCompound gl' ll labels xs
    CBlockDecl decl -> -- TODO: this needs to fix as decl can refer to global symbols
        let (gl', ll') = parseDecl gl ll decl in
        parseCompound gl' ll' labels xs
    CNestedFunDef (_) -> gl -- GNU C nested function is not supported

parseFunDeclr :: [IdEntry] -> (CDerivedDeclarator a) -> [IdEntry]
parseFunDeclr ll (CFunDeclr (Left _) _ _ ) = ll -- old-style function declaration is not supported
parseFunDeclr ll (CFunDeclr (Right (cDecls, _)) _ _) =
    forEachCDecl ll cDecls
    where
    forEachCDecl :: [IdEntry] -> [CDeclaration a] -> [IdEntry]
    forEachCDecl rl [] = rl
    forEachCDecl rl (cDecl:xs) =
        let (new_rl, _) = (parseDecl rl [] cDecl) in forEachCDecl new_rl xs

-- Function definitions
parseDef :: [IdEntry] -> (CFunctionDef a) -> [IdEntry]
parseDef gl (CFunDef cType cDeclr _ cCompound _) = case cDeclr of
    (CDeclr (Just ident) [cFunDeclr] _ _ _) ->
        let gl' = (identToEntry ident IdDecl) : gl in -- add function name to global list
        let ll = parseFunDeclr [] cFunDeclr in -- add function arguments to local list
        case cCompound of
            (CCompound labels items _) ->
                parseCompound gl' ll labels items

--parseTranslUnit ::
parseTranslUnit gl [] = gl
parseTranslUnit gl (x:xs) = case x of
    CDeclExt decl -> let (dl, _) = (parseDecl gl [] decl) in parseTranslUnit dl xs
    CFDefExt def -> let dl = parseDef gl def in parseTranslUnit dl xs
    _ -> gl

readHello f = do
    source <- readInputStream f
    case parseC source $ initPos f of
        Right tu ->
            case tu of
                CTranslUnit l _ -> mapM_ print $ parseTranslUnit [] l
        Left err -> print err

readWithPrep input_file = do
    ast <- errorOnLeftM "Parse Error" $
        parseCFile (newGCC "gcc") Nothing [""] input_file
    case ast of
        CTranslUnit l _ -> mapM_ print $ parseTranslUnit [] l

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg

-- Testing facilities

-- testExpected1 :: [IdEntry]
-- testExpected1 = [
--     ("dead","./hello.c",47,27,IdRef),
--     ("bar","./hello.c",47,16,IdRef),
--     ("foo","./hello.c",47,8,IdRef),
--     ("func2","./hello.c",47,2,IdCall),
--     ("fieldm","./hello.c",42,13,IdRef),
--     ("st3","./hello.c",42,6,IdCall),
--     ("m","./hello.c",42,2,IdRef),
--     ("fieldi","./hello.c",41,11,IdRef),
--     ("st2","./hello.c",41,6,IdRef),
--     ("test_label","./hello.c",40,1,IdLabel),
--     ("fieldk","./hello.c",38,10,IdRef),
--     ("st1","./hello.c",38,6,IdRef),
--     ("only_false","./hello.c",37,16,IdRef),
--     ("cond2","./hello.c",37,6,IdRef),
--     ("else_false","./hello.c",36,24,IdRef),
--     ("if_true","./hello.c",36,14,IdRef),
--     ("cond1","./hello.c",36,6,IdRef),
--     ("global2","./hello.c",34,6,IdRef),
--     ("func3","./hello.c",30,6,IdDecl),
--     ("field2","./hello.c",4,31,IdDecl),
--     ("field1","./hello.c",4,18,IdDecl),
--     ("st1","./hello.c",4,8,IdDecl),
--     ("gloabl2","./hello.c",2,5,IdDecl),
--     ("global0","./hello.c",1,5,IdDecl),
--     ("cond1","./hello.c",43,6,IdRef),
--     ("global1","./hello.c",44,7,IdRef),
--     ("beef","./hello.c",46,7,IdRef)
--     ]
-- 
-- stringDef :: String
-- stringDef = "\ESC[0m"
-- 
-- stringRed :: String
-- stringRed = "\ESC[31m"
-- 
-- stringGreen :: String
-- stringGreen = "\ESC[32m"
-- 
-- stringColor :: String -> String -> String
-- stringColor color str = color ++ str ++ stringDef
-- 
-- runTest :: String -> [IdEntry] -> IO ()
-- runTest input_file expected = do
--     ast <- errorOnLeftM "Parse Error" $
--         parseCFile (newGCC "gcc") Nothing [""] input_file
--     case ast of
--         CTranslUnit l _ ->
--             let res = parseTranslUnit [] l in
--             if hasDup res && length res == length expected
--                 then die $ "has duplicate entries"
--                 else checkRes res
--     where
--         hasDup xs = length (nub xs) /= length xs
--         checkRes [] = putStrLn $ stringColor stringGreen "Passed!"
--         checkRes (x:xs) =
--             case find (\e -> e == x) expected of
--                 Just _ -> checkRes xs
--                 _ -> do
--                     putStrLn $ stringColor
--                                 stringRed
--                                 ((show x) ++ " not found in expected")
--                     checkRes xs



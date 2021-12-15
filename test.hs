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

data EntryType = IdDecl | IdRef | IdCall | Label
instance Show EntryType where
    show IdDecl = "IdDecl"
    show IdRef = "IdRef"
    show IdCall = "IdCall"
    show Label = "Label"

-- | IdEntry stores the information about a symbol:
-- (symbolName, filePath, line)
type IdEntry = (String, String, Int, Int, EntryType)

--parseIdent :: Ident -> IdEntry
--parseIdent (Ident id _ info) =

identToEntry :: Ident -> EntryType -> IdEntry
identToEntry ident entry_type =
    let id_name = (identToString ident) in
    let id_file = case fileOfNode ident of
                    Nothing -> ""
                    Just p -> p in
    let id_pos = posOfNode $ nodeInfo ident in
    let id_row = posRow $ id_pos in
    let id_col = posColumn $ id_pos in
    (id_name, id_file, id_row, id_col, entry_type)

-- Just use linear search as the size of the local list should be handy
inLocalList :: [IdEntry] -> String -> Bool
inLocalList _ "true" = True
inLocalList _ "false" = True
inLocalList [] id_name = False
inLocalList ((e_name, _, _, _, _):xs) id_name =
    if e_name == id_name
        then True
        else inLocalList xs id_name

parseDeclList :: [IdEntry] -> [IdEntry] -> [(Maybe (CDeclarator a0), b0, c0)] ->
    ([IdEntry], [IdEntry])
parseDeclList gl ll [] = (gl, ll)
parseDeclList gl ll ((cDeclr, cInit, cExp):xs) =
    case cDeclr of
        Nothing -> (gl, ll)
        Just (CDeclr (Just ident) _ _ _ _) ->
            case ll of
                [] -> let gl' = (identToEntry ident IdDecl) : gl in parseDeclList gl' ll xs
                _ -> let ll' = (identToEntry ident IdDecl) : ll in parseDeclList gl ll' xs

parseCSU :: [IdEntry] -> [IdEntry] -> CStructureUnion a -> ([IdEntry], [IdEntry])
parseCSU gl ll (CStruct _ mident mdecl _ _) =
    case mident of
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
parseCType gl ll (cType:xs) declList =
    case cType of
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

parseExpr gl ll expr id_type =
    case expr of
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
    where
        parseExpr2 gl ll (expr1, t1) (expr2, t2) =
            let gl' = parseExpr gl ll expr1 t1 in
            parseExpr gl' ll expr2 t2
        parseExpr3 gl ll exprt1 exprt2 (expr3, t3)  =
            let gl' = parseExpr2 gl ll exprt1 exprt2 in
            parseExpr gl' ll expr3 t3

parseStmt gl ll stmt =
    case stmt of
        CCase expr stmt _ ->
            let gl' = parseExpr gl ll expr IdRef in
            parseStmt gl' ll stmt
        CExpr (Just expr) _ -> parseExpr gl ll expr IdRef
        _ -> gl

-- dummy local list used to indicate we are in local scope
dummyll :: [IdEntry]
dummyll = [("", "", 0, 0, IdDecl)]

-- C code compound, gl is global symbol list, ll is local symbol list
parseCompound :: [IdEntry] -> [IdEntry] -> [Ident] -> [CCompoundBlockItem a]
    -> [IdEntry]
parseCompound gl ll labels [] = gl -- end of parsing, ll is discarded
parseCompound gl ll labels (blockItem:xs) =
    case blockItem of
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
parseDef gl (CFunDef cType cDeclr _ cCompound _) =
    case cDeclr of
        (CDeclr (Just ident) [cFunDeclr] _ _ _) ->
            let gl' = (identToEntry ident IdDecl) : gl in -- add function name to global list
            let ll = parseFunDeclr [] cFunDeclr in -- add function arguments to local list
            case cCompound of
                (CCompound labels items _) ->
                    parseCompound gl' ll labels items

--parseTranslUnit ::
parseTranslUnit gl [] = gl
parseTranslUnit gl (x:xs) =
    case x of
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

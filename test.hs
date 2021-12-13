import Language.C
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.System.GCC

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

data EntryType = IdDecl | IdRef | IdCall
instance Show EntryType where
    show IdDecl = "IdDecl"
    show IdRef = "IdRef"
    show IdCall = "IdCall"

-- | IdEntry stores the information about a symbol:
-- (symbolName, filePath, line)
type IdEntry = (String, String, Int, Int, EntryType)

--parseIdent :: Ident -> IdEntry
--parseIdent (Ident id _ info) =

identToEntry ident entry_type =
    let id_name = (identToString ident) in
    let id_file = case fileOfNode ident of
                    Nothing -> ""
                    Just p -> p in
    let id_pos = posOfNode $ nodeInfo ident in
    let id_row = posRow $ id_pos in
    let id_col = posColumn $ id_pos in
    (id_name, id_file, id_row, id_col, entry_type)

--parseDeclList :: [a] -> [b]
parseDeclList gl [] = gl
parseDeclList gl ((cDeclr, cInit, cExp):xs) =
    case cDeclr of
        Nothing -> gl
        Just (CDeclr (Just ident) _ _ _ _) ->
            let il = (identToEntry ident IdDecl) : gl in parseDeclList il xs

parseCSU gl (CStruct _ mident mdecl _ _) =
    case mident of
        Just ident -> let il = ((identToEntry ident IdDecl) : gl) in
            case mdecl of
                Just declL -> parseStructDeclList il declL
                _ -> il
        _ -> gl -- this needs to be fixed as anoymous struct can have fields
    where
        parseStructDeclList gl' [] = gl'
        parseStructDeclList gl' (x:xs) =
            let dl = (parseDecl gl' x) in parseStructDeclList dl xs

parseCTypeList gl [] _ = gl
parseCTypeList gl (cType:xs) declList =
    case cType of
        -- struct or union
        CTypeSpec (CSUType (csu) _) ->
            let cl = (parseCSU gl csu) in parseCTypeList cl xs declList
        -- other types
        _ ->
            let dl = (parseDeclList gl declList) in parseCTypeList dl xs declList

--parseDecl :: CDeclaration a -> IdEntry
parseDecl gl (CDecl cType declList info) =
    parseCTypeList gl cType declList

-- Function definitions
--parseDef
parseDef gl (CFunDef cType cDeclr _ cStmt _) =
    case cDeclr of
        (CDeclr (Just ident) _ _ _ _) ->
            (identToEntry ident IdDecl) : gl

--parseTranslUnit ::
parseTranslUnit gl [] = gl
parseTranslUnit gl (x:xs) =
    case x of
        CDeclExt decl -> let dl = (parseDecl gl decl) in parseTranslUnit dl xs
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

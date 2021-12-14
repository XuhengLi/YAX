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

parseCType gl [] _ = gl
parseCType gl (cType:xs) declList =
    case cType of
        -- struct or union
        CTypeSpec (CSUType (csu) _) ->
            let dl = (parseCSU gl csu) in parseCType dl xs declList
        -- other types
        _ ->
            parseDeclList gl declList

--parseDecl :: CDeclaration a -> IdEntry
parseDecl gl (CDecl cTypes [] info) = gl -- void function arguments
parseDecl gl (CDecl cTypes declrList info) =
    parseCType gl cTypes declrList

-- C code compound, gl is global symbol list, ll is local symbol list
parseCompound gl ll (CCompound labels (blockItem:xs) _) =
    case blockItem of
        CBlockStmt stmt -> gl -- placeholder
        CBlockDecl (_) -> gl -- placeholder
        CNestedFunDef (_) -> gl -- GNU C nested function is not supported

parseFunDeclr gl (CFunDeclr (Left _) _ _ ) = gl -- old-style function declaration is not supported
parseFunDeclr gl (CFunDeclr (Right (cDecls, _)) _ _) =
    forEachCDecl gl cDecls
    where
    forEachCDecl rl [] = rl
    forEachCDecl rl (cDecl:xs) = let new_rl = (parseDecl rl cDecl) in forEachCDecl new_rl xs

-- Function definitions
--parseDef
parseDef gl (CFunDef cType cDeclr _ cCompound _) =
    case cDeclr of
        (CDeclr (Just ident) [cFunDeclr] _ _ _) ->
            let gl' = (identToEntry ident IdDecl) : gl in
            let ll = parseFunDeclr gl' cFunDeclr in
            parseCompound gl' ll cCompound

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

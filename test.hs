import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Data.Ident

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

-- | IdEntry stores the information about a symbol:
-- (symbolName, filePath, line)
type IdEntry = (String, String, Int)


--parseIdent :: Ident -> IdEntry
--parseIdent (Ident id _ info) =

--parseDeclList :: [a] -> [b]
parseDeclList gl [] = gl
parseDeclList gl ((cDeclr, cInit, cExp):xs) =
    case cDeclr of
        Nothing -> gl
        Just (CDeclr (Just ident) _ _ _ _) -> let il = (identToString ident) : gl in parseDeclList il xs

parseCSU gl (CStruct _ mident _ _ _) =
    case mident of
        Just ident -> (identToString ident) : gl
        _ -> []

parseCTypeList gl [] _ = gl
parseCTypeList gl (cType:xs) declList =
    case cType of
        -- struct or union
        CTypeSpec (CSUType (csu) _) -> let cl = (parseCSU gl csu) in parseCTypeList cl xs declList
        -- other types
        _ -> let dl = (parseDeclList gl declList) in parseCTypeList dl xs declList

--parseDecl :: CDeclaration a -> IdEntry
parseDecl gl (CDecl cType declList info) =
    parseCTypeList gl cType declList

--parseTranslUnit ::
parseTranslUnit gl [] = gl
parseTranslUnit gl (x:xs) =
    case x of
        CDeclExt decl -> let dl = (parseDecl gl decl) in parseTranslUnit dl xs
        _ -> gl

readHello = do
    source <- readInputStream "./hello.c"
    case parseC source $ initPos "./hello.c" of
        Right tu ->
            case tu of
                CTranslUnit l _ -> print $ parseTranslUnit [] l
        _ -> error "Syntax error"



import Language.C
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.System.GCC

import System.IO
import System.Environment
import System.Directory
import System.Exit
import Data.List
import qualified Data.Map.Strict as Map

import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files

import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq

import Data.Map.Internal.Debug

data EntryType = IdDecl | IdRef | IdCall | IdLabel | IdLocal deriving (Eq, Show)
-- Not meaningful, just in case of sorting for searching
instance Ord EntryType where
    IdDecl  `compare` _         = LT
    IdRef   `compare` IdDecl    = GT
    IdRef   `compare` _         = LT
    IdCall  `compare` IdLabel   = LT
    IdCall  `compare` _         = GT
    IdLabel `compare` _         = GT
    IdLocal `compare` _         = EQ

-- | IdEntryVal stores the information about a symbol:
--  (file, row, column, type)
type IdEntryVal = (String, Int, Int, EntryType)
-- | (ident, key)
-- type IdEntry = (String, IdEntryVal)
-- type IdDB = [IdEntry]

type IdEntry = IdEntryVal
type IdDB = Map.Map String [IdEntry]

-- dummy entry for local symbols to avoid unnecessary GC
dummyEntry :: IdEntry
dummyEntry = ("", 0, 0, IdLocal)

identToEntry :: Ident -> EntryType -> IdEntry
identToEntry ident entry_type =
    -- let id_name = (identToString ident) in
    let id_file = case fileOfNode ident of
                    Nothing -> ""
                    Just p -> p in
    let id_pos = posOfNode $ nodeInfo ident in
    let id_row = posRow $ id_pos in
    let id_col = posColumn $ id_pos in
    (id_file, id_row, id_col, entry_type)

-- Just use linear search as the size of the local list should be handy
inLocalList :: IdDB -> String -> Bool
-- "true" and "false" are excluded since they are widely used as keywords
inLocalList _ "true" = True
inLocalList _ "false" = True
inLocalList db id_name =  case Map.lookup id_name db of
    Just _ -> True
    _ -> False

addEntry :: Ident -> EntryType -> IdDB -> IdDB
addEntry ident IdLocal gl =
    let id_name = (identToString ident) in
    Map.insert id_name [dummyEntry] gl
addEntry ident t gl =
    let id_name = (identToString ident) in
    let id_entry = identToEntry ident t in
    Map.insertWith mergeEntry id_name [id_entry] gl
    where
    mergeEntry :: [IdEntry] -> [IdEntry] -> [IdEntry]
    mergeEntry [n] o = n : o
    mergeEntry _ o = o -- we know new_value must be a singleton list

parseDeclList :: IdDB -> IdDB -> [(Maybe (CDeclarator a0), b0, c0)] ->
    (IdDB, IdDB)
parseDeclList gl ll [] = (gl, ll)
parseDeclList gl ll ((cDeclr, cInit, cExp):xs) = case cDeclr of
    Nothing -> (gl, ll)
    Just (CDeclr (Just ident) _ _ _ _) ->
        case null ll of
            False -> let gl' = addEntry ident IdDecl gl in parseDeclList gl' ll xs
            _ -> let ll' = addEntry ident IdLocal ll in parseDeclList gl ll' xs -- TODO: this haven't been addressed yet

parseCSU :: IdDB -> IdDB -> CStructureUnion a -> (IdDB, IdDB)
parseCSU gl ll (CStruct _ mident mdecl _ _) = case mident of
    Just ident ->
        -- struct variable declarations are always indexed
        let gl' = addEntry ident IdDecl gl in
        case mdecl of
            Just declL -> (parseStructDeclList gl' declL, ll)
            _ -> (gl', ll)
    _ -> (gl, ll) -- TODO: this needs to be fixed as anoymous struct can have fields
    where
        -- struct fields are always indexed
        parseStructDeclList gl' [] = gl'
        parseStructDeclList gl' (x:xs) =
            let (dl, _) = (parseDecl gl' Map.empty x) in parseStructDeclList dl xs

parseCType :: IdDB -> IdDB -> [CDeclarationSpecifier a] ->
    [(Maybe (CDeclarator a0), b0, c0)] -> (IdDB, IdDB)
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
parseDecl :: IdDB -> IdDB -> (CDeclaration a) ->
    (IdDB, IdDB)
parseDecl gl ll (CDecl cTypeList declrList _) =
    parseCType gl ll cTypeList declrList

-- expr and stmt won't introduce new symbols so local DB is always discarded
parseExprList :: IdDB -> IdDB -> [CExpression a] -> EntryType -> IdDB
parseExprList gl ll [] _ = gl
parseExprList gl ll (expr:xs) id_type =
    let gl' = parseExpr gl ll expr id_type in
    parseExprList gl' ll xs id_type

parseExpr :: IdDB -> IdDB -> (CExpression a) -> EntryType -> IdDB
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
        addEntry field IdRef gl'
    CVar ident _ ->
        -- if the ident is a local variable, just discard it
        if inLocalList ll (identToString ident)
            then gl
            else addEntry ident id_type gl
    _ -> gl

parseExpr2 :: IdDB -> IdDB -> (CExpression a, EntryType) ->
    (CExpression a, EntryType) -> IdDB
parseExpr2 gl ll (expr1, t1) (expr2, t2) =
    let gl' = parseExpr gl ll expr1 t1 in
    parseExpr gl' ll expr2 t2

parseExpr3 :: IdDB -> IdDB -> (CExpression a, EntryType) ->
    (CExpression a, EntryType) -> (CExpression a, EntryType) -> IdDB
parseExpr3 gl ll exprt1 exprt2 (expr3, t3)  =
    let gl' = parseExpr2 gl ll exprt1 exprt2 in
    parseExpr gl' ll expr3 t3

parseStmt :: IdDB -> IdDB -> (CStatement a) -> IdDB
parseStmt gl ll stmt = case stmt of
    CLabel label stmt _ _ ->
        let gl' = addEntry label IdLabel gl in
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
        addEntry label IdLabel gl
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

-- C code compound, gl is global symbol DB, ll is local symbol DB
-- Updates to a local symbol in a compound is discarded when the compound
-- is parsed
parseCompound :: IdDB -> IdDB -> [Ident] -> [CCompoundBlockItem a]
    -> IdDB
parseCompound gl ll labels [] = gl -- end of parsing, ll is discarded
parseCompound gl ll labels (blockItem:xs) = case blockItem of
    CBlockStmt stmt -> -- Stmt won't introduce new symbols
        let gl' = parseStmt gl ll stmt in
        parseCompound gl' ll labels xs
    CBlockDecl decl -> -- TODO: this needs to fix as decl can refer to global symbols
        let (gl', ll') = parseDecl gl ll decl in
        parseCompound gl' ll' labels xs
    CNestedFunDef (_) -> gl -- GNU C nested function is not supported

parseFunDeclr :: IdDB -> (CDerivedDeclarator a) -> IdDB
parseFunDeclr ll (CFunDeclr (Left _) _ _ ) = ll -- old-style function declaration is not supported
parseFunDeclr ll (CFunDeclr (Right (cDecls, _)) _ _) =
    forEachCDecl ll cDecls
    where
    forEachCDecl :: IdDB -> [CDeclaration a] -> IdDB
    forEachCDecl rl [] = rl
    forEachCDecl rl (cDecl:xs) =
        let (new_rl, _) = (parseDecl rl Map.empty cDecl) in forEachCDecl new_rl xs

-- Function definitions
parseDef :: IdDB -> (CFunctionDef a) -> IdDB
parseDef gl (CFunDef cType cDeclr _ cCompound _) = case cDeclr of
    (CDeclr (Just ident) [cFunDeclr] _ _ _) ->
        let gl' = addEntry ident IdDecl gl in -- add function name to global list
        let ll = parseFunDeclr Map.empty cFunDeclr in -- add function arguments to local list
        case cCompound of
            (CCompound labels items _) ->
                parseCompound gl' ll labels items

--parseTranslUnit ::
parseTranslUnit gl [] = gl
parseTranslUnit gl (x:xs) = case x of
    CDeclExt decl -> let (dl, _) = (parseDecl gl Map.empty decl) in parseTranslUnit dl xs
    CFDefExt def -> let dl = parseDef gl def in parseTranslUnit dl xs
    _ -> gl

readHello f = do
    source <- readInputStream f
    case parseC source $ initPos f of
        Right tu ->
            case tu of
                CTranslUnit l _ -> mapM_ print $ parseTranslUnit Map.empty l
        Left err -> print err

parseAST :: CTranslationUnit a -> IdDB
parseAST (CTranslUnit l _) = parseTranslUnit Map.empty l

readWithPrep :: String -> IO ()
readWithPrep input_file = do
    ast <- errorOnLeftM "Parse Error" $
        parseCFile (newGCC "gcc") Nothing [""] input_file
    mapM_ print $ parseAST ast

readWithPrep' :: String -> IO IdDB
readWithPrep' input_file = do
    ast <- errorOnLeftM "Parse Error" $
        parseCFile (newGCC "gcc") Nothing [""] input_file
    return $ parseAST ast

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg

usage :: IO ()
usage = do
    prog <- getProgName
    die $ "Usage: " ++ prog ++ " <filename>|<directory> -p|-s"

-- Borrowed from https://stackoverflow.com/a/23822913
traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top exclude = do
  ds <- getDirectoryContents top
  paths <- forM (filter (not.exclude) ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then traverseDir path exclude
      else return [path]
  return (concat paths)

filesToStreamList :: [FilePath] -> IO [(InputStream, FilePath)]
filesToStreamList fs = sequence $ map (\f -> do
                                        s <- readInputStream f
                                        return (s, f))
                                        fs

-- Credit: https://stackoverflow.com/questions/19117922/parallel-folding-in-haskell/19119503
pfold :: (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
    len = length xs
    (ys', zs') = splitAt (len `div` 2) xs
    ys = pfold mappend ys'
    zs = pfold mappend zs'

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f, c] -> handleFileDir f c
        _   -> usage
    where
    handleFileDir f c = do
        isF <- doesFileExist f
        if isF then readWithPrep f
        else  handleDir f c
    handleDir f c = do
        isD <- doesDirectoryExist f
        if isD then do
            files <- traverseDir f excludeDot
            contents <- filesToStreamList files
            case c of
                "-s" -> do
                    print $ Map.lookup "xxx" $ handleStreams contents
                "-p" -> do
                    print $ Map.lookup "xxx" $ parHandleStreams contents
                    -- error "-p"
        else die $ ("File does not exists: " ++) $ show f
    doHandleStream :: (InputStream, FilePath) -> IdDB
    doHandleStream (s, f) = case parseC s $ initPos f of -- TODO: pass file nanme
        Right tu -> case tu of
            CTranslUnit l _ -> parseTranslUnit Map.empty l
        Left err -> Map.singleton "???" [dummyEntry] -- XXX: debugging
    handleStreams :: [(InputStream, FilePath)] -> IdDB
    handleStreams ss = foldl (Map.unionWith unionResult) Map.empty $
                            map doHandleStream ss
    parHandleStreams :: [(InputStream, FilePath)] -> IdDB
    parHandleStreams ss =
        pfold (Map.unionWith unionResult) $
            -- parMap rpar doHandleStream ss
            withStrategy (parBuffer 500 rpar) . map doHandleStream $ ss
    unionResult :: [IdEntry] -> [IdEntry] -> [IdEntry]
    unionResult new old = new ++ old
    excludeDot "." = True
    excludeDot ".." = True
    excludeDot _ = False
    -- XXX: Discard
    -- parHandleFiles :: [String] -> IO IdDB
    -- parHandleFiles fs = do
    --     let (as, bs) = splitAt (length fs `div` 2) fs in
    --         runEval $ do
    --             as' <- rpar (force map readWithPrep' as)
    --             bs' <- rpar (force map readWithPrep' bs)
    --             rseq as'
    --             rseq bs'
    --             return (combineIOList as' bs')
    --             -- let mres = runEval (parMap readWithPrep' fs) in
    --             --     do
    --             --         res <- sequence mres
    --             --         return $ concat res
    -- combineIOList :: [IO IdDB] -> [IO IdDB] -> IO IdDB
    -- combineIOList as bs = do
    --     as' <- sequence as
    --     bs' <- sequence bs
    --     return $ (concat as') ++ (concat bs')
    -- handleFiles :: [String] -> IO ()
    -- handleFiles fs = do
    --     rs <- sequence $ map readWithPrep' fs
    --     mapM_ print rs
    --     return ()

-- Testing facilities

-- testExpected1 :: IdDB
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
-- runTest :: String -> IdDB -> IO ()
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



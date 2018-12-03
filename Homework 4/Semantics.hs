-- ----------------------------------------
-- A lambda calculus interpreter.

{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Arrow (first, second)

import Control.Monad.Except
import Control.Monad

import Data.List (union, (\\))
import Data.Maybe

import Text.ParserCombinators.Parsec hiding ((<|>), many)

import qualified Control.Monad.State as MS
import qualified Data.Set as S
import qualified Data.Map as M

-- ----------------------------------------
-- Data structures
type Name = (String, Int)

data E = Lam Name T E
       | App E E
       | Var Name
       | MV String

       | ENum Integer
       | EBool Bool
       | EPair E E
       deriving (Eq)

data T = T :-> T
       | TVar Name
       | TBool
       | TNum
       | TUnit
       | TPair T T
       deriving (Eq)

infixr 1 :->

-- ----------------------------------------
-- Helper functions
name :: String -> Name
name s = (s, 0)

var :: String -> E
var = Var . name

tvar :: String -> T
tvar = TVar . name

showName :: Name -> ShowS
showName (n, 0) = (n++)
showName (n, i) = (n++) . shows i

-- ----------------------------------------
-- Show instances
instance Show E where
    showsPrec d (Lam x t b) = showParen (d >= 1)
        -- $ showString "\\" . showName x . showString ":" . shows t . showString ". "
        $ showString "\\" . showName x . showString ". "
        . showsPrec 0 b
    showsPrec d (App a@(App _ _) b) = showParen (d >= 1)
        $ showsPrec 0 a . showString " " . showsPrec 1 b
    showsPrec d (App a b) = showParen (d >= 1)
        $ showsPrec 1 a . showString " " . showsPrec 1 b
    showsPrec d (Var n)   = showName n
    showsPrec d (MV s)    = showString s

    showsPrec d (ENum n)    = shows n
    showsPrec d (EBool b)   = shows b
    showsPrec d (EPair a b) = showParen (d >= 2)
        $ showString "PAIR " . showsPrec 2 a . showString " " . showsPrec 2 b 

instance Show T where
    showsPrec d (a :-> b)   = showParen (d >= 1)
        $ showsPrec 1 a . showString " -> " . showsPrec 0 b
    showsPrec d (TVar n)    = showName n
    showsPrec d TBool       = showString "bool"
    showsPrec d TNum        = showString "num"
    showsPrec d TUnit       = showString "unit"
    showsPrec d (TPair a b) = showParen (d >= 2)
        $ showString "pair " . showsPrec 2 a . showString " " . showsPrec 2 b

-- ----------------------------------------
-- Parser
ident :: Parser Name
ident = do
    l <- many1 lower
    n <- many  digit
    case n of
        [] -> return (l, 0)
        _  -> return (l, read n)

whitespace :: Parser Char
whitespace = oneOf " \t\n"

parseMetaVarInline :: M.Map String E -> Parser E
parseMetaVarInline m = do
    n <- many1 upper
    case n `M.lookup` m of
        Just e -> return e
        _      -> unexpected $ concat ["unknown meta variable: ", n]

parseMetaVarExists :: M.Map String E -> Parser E
parseMetaVarExists m = do
    n <- many1 upper
    case n `M.lookup` m of
        Just _ -> return (MV n)
        _      -> unexpected $ concat ["unknown meta variable: ", n]

parseMetaVarAny :: M.Map String E -> Parser E
parseMetaVarAny _ = MV <$> (many1 upper <|> many1 digit)

parseMetaVar :: M.Map String E -> Parser E
parseMetaVar = parseMetaVarAny

parseE :: Parser E
parseE = many whitespace
    *> (parseLam <|> parseApp)

parseEApp :: Parser E
parseEApp = (parseLam <|> parseParen <|> parseVar <|> parseMetaVar metaVars)
          <* many whitespace

parseLam :: Parser E
parseLam = do
    char '\\' <|> char 'λ'
    x <- ident
    t <- char ':' *> parseType <|> pure (tvar "t")
    char '.'
    b <- parseE
    pure (Lam x t b)

parseType :: Parser T
parseType = do
    many whitespace
    t <- parseTypeParen <|> parseTypeBase
    many whitespace
    parseTypeArrow t <|> pure t

parseTypeParen :: Parser T
parseTypeParen = char '(' *> parseType <* many whitespace <* char ')'

parseTypeBase :: Parser T
parseTypeBase = do
    n <- ident
    many whitespace
    if n == ("pair", 0)
      then do
        a <- parseType
        b <- parseType
        return (TPair a b)
      else
        return (baseType n)
  where
    baseType ("num", 0)  = TNum
    baseType ("bool", 0) = TBool
    baseType ("unit", 0) = TUnit
    baseType n           = TVar n

parseTypeArrow :: T -> Parser T
parseTypeArrow a = (a :->) <$> (string "->" *> parseType)

parseParen :: Parser E
parseParen = char '(' *> parseE <* many whitespace <* char ')'

parseApp :: Parser E
parseApp = do
    f <- parseEApp
    foldl App f <$> many parseEApp

parseVar :: Parser E
parseVar = Var <$> ident

instance Read E where
    readsPrec _ s = case runParser parseE () "read" s of
                         Right e -> [(e, "")]
                         Left s -> error ("Failed to parse E in read: " ++ show s)

testParse :: String -> IO ()
testParse = print . runParser (parseE <* eof) () "test" 

testParseType :: String -> IO ()
testParseType = print . runParser (parseType <* eof) () "test" 

testString :: String -> Bool
testString s = map slash s == show e
  where
    e = read s :: E

slash :: Char -> Char
slash 'λ' = '\\'
slash c   = c 

test :: E -> Bool
test e = e == read (show e)

-- ----------------------------------------
-- Type checker
check :: M.Map Name T -> E -> Either String T
check g (Var n)     = maybe (Left $ "untyped variable " ++ showName n []) Right (n `M.lookup` g) 
check g (MV n)      = maybe (Left $ "untyped meta-variable " ++ n)        Right ((n,0) `M.lookup` g)
check g (Lam n t e) = (t :->) <$> check (M.insert n t g) e
check g (App f a)   = do
    tf <- check g f
    ta <- check g a
    case tf of
        (ta' :-> tr) -> if ta' == ta
                          then return tr
                          else Left $ concat
                                [ "Type mismatch, ", show tf
                                , " applied to argument with type ", show ta
                                , " in (", show (App f a), ")."
                                , " Gamma = ", show g
                                ]

        _            -> Left $ "Value applied to a non-function " ++ show tf

checkMV :: E -> Either String T
checkMV e = check g e
  where
    g = M.fromList . map (\(n, (_, t)) -> ((n,0),t)) $ exprs

-- ----------------------------------------
-- Type reconstruction or inference
data Constraint = T :=: T
    deriving (Show, Eq)

type StateR a = ExceptT String (MS.State ([Name],[Constraint])) a

addC :: Constraint -> StateR ()
addC c = do
    (vs,cs) <- MS.get
    MS.put (vs,c:cs)

fresh :: MS.MonadState ([Name], a) m => m T
fresh = do
    (v:vs,cs) <- MS.get
    MS.put (vs,cs)
    return (TVar v)

constrainAs g t tt tr = do
    tt' <- constrain g t
    addC (tt :=: tt')
    return tr

constrain :: M.Map Name T -> E -> StateR T
constrain g (MV n)  = case name n `M.lookup` g of
                        Just t  -> return t
                        Nothing -> throwError $ "Unbound meta variable " ++ n
constrain g (Var n) = case n `M.lookup` g of
                        Just t  -> return t
                        Nothing -> throwError $ "Unbound variable " ++ show (Var n)
constrain g (Lam x _ y) = do
    tx <- fresh
    ty <- constrain (M.insert x tx g) y
    return (tx :-> ty)
constrain g (App f a) = do
    tf <- constrain g f
    ta <- constrain g a
    s <- fresh
    addC (tf :=: (ta :-> s))
    return s

unify :: [Constraint] -> Either String (M.Map Name T)
unify []   = Right M.empty
unify ((s :=: t):cs)
  | s == t = unify cs
unify ((TVar n :=: t):cs)
  | n `notElem` freeVarsT t = M.insert n t <$> unify (map (rewrite n t) cs)
unify ((t :=: TVar n):cs)
  | n `notElem` freeVarsT t = M.insert n t <$> unify (map (rewrite n t) cs)
unify (((sa :-> sb) :=: (ta :-> tb)):cs) = unify ((sa :=: ta):(sb :=: tb):cs)
unify ((TPair sa sb :=: TPair ta tb):cs) = unify ((sa :=: ta):(sb :=: tb):cs)
unify (c:cs) = Left $ "Failed to unify " ++ show c

unifyRW :: [Constraint] -> Either String (M.Map Name T)
unifyRW cs = do
    m <- unify cs
    Right $ hammer f m
  where
    f m = go (M.toList m) m

    go [] m         = m
    go ((n,t):rs) m = go rs (rewriteT n t <$> m)

hammer :: Eq a => (a -> a) -> a -> a
hammer f x
  | x' == x = x'
  | otherwise = hammer f x'
  where x' = f x

rewrite :: Name -> T -> Constraint -> Constraint
rewrite n t (a :=: b) = rewriteT n t a :=: rewriteT n t b
  
rewriteT :: Name -> T -> T -> T
rewriteT n t v@(TVar x)
    | x == n    = t
    | otherwise = v
rewriteT n t (a :-> b)   = rewriteT n t a :-> rewriteT n t b
rewriteT n t (TPair a b) = TPair (rewriteT n t a) (rewriteT n t b)
rewriteT n t x           = x

vars :: [Name]
vars = [("t", n) | n <- [1..]]

-- infer :: E -> Either String T
infer e = second snd . flip MS.runState (vars', []) . runExceptT $ constrain g e
  where
    mvs = map (first name . second snd) $ exprs
    (g, (vars', _)) = flip MS.runState (vars, M.empty) . freshen $ mvs

inferTestIO e = do
    let (r, cs) = infer e
    case r of
        Left s -> do
            putStrLn s
            mapM_ print cs
        Right t -> do
            putStrLn "Type:"
            print t
            mapM_ print cs
            case unifyRW cs of
                Left s -> print s
                Right cs' -> do
                    putStrLn "Unified:"
                    mapM_ print (map (first TVar) $ M.toList cs')
                    putStrLn "Final type:"
                    print (applyTypes cs' t)

applyTypes :: (M.Map Name T) -> T -> T
applyTypes m (a :-> b) = applyTypes m a :-> applyTypes m b
applyTypes m v@(TVar n) = case n `M.lookup` m of
                            Just t -> t
                            _      -> v 
applyTypes m (TPair a b) = applyTypes m a `TPair` applyTypes m b
applyTypes m t = t

freshen [] = return M.empty
freshen ((n,t):as) = do
    t' <- freshenT t
    clear
    M.insert n t' <$> freshen as
  where
    clear = MS.modify (second (const M.empty))

    freshenT (a :-> b)   = (:->) <$> freshenT a <*> freshenT b
    freshenT (TPair a b) = TPair <$> freshenT a <*> freshenT b
    freshenT (TVar n) = do
        g <- getGamma
        case n `M.lookup` g of
            Just t  -> return t
            Nothing -> do
                t' <- fresh
                putGamma (M.insert n t' g)
                return t'
    freshenT t = return t

    getGamma = snd <$> MS.get
    putGamma g = MS.modify (second (const g))

testExprs = mapM_ print . map (second (f . fst)) $ exprs
  where
    f s
      | testString s = Nothing
      | otherwise    = Just (map slash s, read s :: E)

-- ----------------------------------------
-- Semantics
type Step = E -> Maybe E

-- Capture avoiding substitution helpers
freeVars :: E -> [Name]
freeVars (Var x) = [x]
freeVars (App a b) = freeVars a `union` freeVars b 
freeVars (Lam n _ x) = freeVars x \\ [n]
freeVars (MV s) = case s `M.lookup` metaVars of
                    Just e -> freeVars e
                    _      -> []

freeVarsT :: T -> [Name]
freeVarsT (TVar x)    = [x]
freeVarsT (a :-> b)   = freeVarsT a `union` freeVarsT b
freeVarsT (TPair a b) = freeVarsT a `union` freeVarsT b
freeVarsT _           = []

allVars :: E -> [Name]
allVars (Var x) = [x]
allVars (App a b) = allVars a `union` allVars b
allVars (Lam n _ x) = allVars x
allVars (MV s)    = case s `M.lookup` metaVars of
                        Just e -> freeVars e
                        _      -> []

-- Capture avoiding substitution.
subE :: Name -> E -> E -> E
subE x s b = sub vs0 b
  where
    sub _ e@(Var v)
        | v == x = s
        | otherwise = e
    sub vs e@(Lam v t e')
        | v == x = e
        | v `elem` fvs = Lam v' t (sub vs' e'')
        | otherwise = Lam v t (sub vs e')
        where
            (v',vs') = nextName v vs
            e'' = subE v (Var v') e'
    sub vs (App f a) = sub vs f `App` sub vs a
    sub vs (MV s)    = MV s
    fvs = freeVars s
    vs0 = fvs `union` allVars b

captureTest = read "(λx. ((λy. x y) z)) y" :: E
-- > sub captureTest 
-- Just ((\y. y y) z)   -- Wrong! We want to rename the bound y so it does not match.
-- Just ((\y1. y y1) z) -- Correct.

nextName :: Name -> [Name] -> (Name, [Name])
nextName n@(s, i) ns
   | n `elem` ns = nextName (s, i+1) ns
   | otherwise   = (n, n:ns)

-- ----------------------------------------
-- Semantic Rules
sub :: Step
sub (App (Lam x _ b) a) = Just $ subE x a b
sub _                 = Nothing 

arg :: Step -> Step
arg f (App a b) = App a <$> f b
arg _ _         = Nothing

func :: Step -> Step
func f (App a b) = App <$> f a <*> pure b
func _ _         = Nothing

body :: Step -> Step
body f (Lam x t b) = Lam x t <$> f b
body _ _         = Nothing

-- Meta variable expansion
mvExpand :: Step
mvExpand (App (MV s) a) =
    case s `M.lookup` metaVars of
      Just e -> Just (App e a)
      _      -> Nothing
mvExpand _      = Nothing

-- Meta variable high level interpretation
mvHighLevel :: Step
mvHighLevel (MV "Z") = Just $ ENum 0
mvHighLevel (MV "0") = Just $ ENum 0
mvHighLevel (MV "1") = Just $ ENum 1
mvHighLevel (MV "2") = Just $ ENum 2
mvHighLevel (MV "3") = Just $ ENum 3
mvHighLevel (MV "4") = Just $ ENum 4
mvHighLevel (MV "5") = Just $ ENum 5
mvHighLevel (App (MV "PRED") (ENum n))   = Just $ ENum (pred n)
mvHighLevel (App (MV "SUCC") (ENum n))   = Just $ ENum (succ n)
mvHighLevel (App (MV "ISZERO") (ENum n)) = Just $ if n == 0 then MV "T" else MV "F"
mvHighLevel (App (App (MV "PLUS") (ENum a)) (ENum b)) = Just $ ENum (a+b)
mvHighLevel (App (App (MV "SUB")  (ENum a)) (ENum b)) = Just $ ENum (max (a-b) 0)
mvHighLevel (App (App (MV "MULT") (ENum a)) (ENum b)) = Just $ ENum (a*b)
mvHighLevel (App (App (MV "POW")  (ENum a)) (ENum b)) = Just $ ENum (a^b)
mvHighLevel (App (App (MV "LEQ")  (ENum a)) (ENum b)) = Just $ if a <= b then MV "T" else MV "F"
mvHighLevel (App (MV s) a)
  | Just e <- getExpansion s = Just (App e a)
mvHighLevel _  = Nothing

-- Rule priority
stepHighLevel :: Step
stepHighLevel e = sub e <|> mvHighLevel e <|> func stepHighLevel e <|> arg stepHighLevel e -- <|> body step e

-- Rule priority
stepExpand :: Step
stepExpand e = sub e <|> mvExpand e <|> func stepExpand e <|> arg stepExpand e

metaVars :: M.Map String E
metaVars = M.fromList . map (second (read . fst)) $ exprs

-- These are the ones that have high-level semantics
mvBuiltIn :: S.Set String
mvBuiltIn = S.fromList
    [ "PRED", "SUCC", "ISZERO", "PLUS", "SUB", "MULT", "POW", "LEQ"
    , "Z", "0", "1", "2", "3", "4", "5"
    ]

getExpansion :: String -> Maybe E
getExpansion s
  | s `S.member` mvBuiltIn = Nothing
  | otherwise              = s `M.lookup` metaVars

-- ----------------------------------------
-- Evaluation helpers (Interpreter)
evalStepsWith :: Step -> E -> [E]
evalStepsWith s = catMaybes 
                . takeWhile isJust
                . iterate (>>= s)
                . pure

evalWith :: Step -> E -> E
evalWith s = last . evalStepsWith s

countStepsWith :: Step -> E -> (Int, E)
countStepsWith s e =
    let ss = catMaybes . takeWhile isJust . iterate (>>= s) . pure $ e
    in  (length ss, last ss)

evalStepsExpand :: E -> [E]
evalStepsExpand = evalStepsWith stepExpand

evalStepsExpandIO :: E -> IO ()
evalStepsExpandIO = mapM_ print . evalStepsExpand

countStepsExpand :: E -> (Int, E)
countStepsExpand = countStepsWith stepExpand

evalExpandIO :: E -> IO ()
evalExpandIO e = do
    let (l, e') = countStepsExpand e
    putStrLn $ show l ++ " steps"
    print e'

evalStepsHighLevel :: E -> [E]
evalStepsHighLevel = evalStepsWith stepHighLevel

evalStepsHighLevelIO :: E -> IO ()
evalStepsHighLevelIO = mapM_ print . evalStepsHighLevel

countStepsHighLevel :: E -> (Int, E)
countStepsHighLevel = countStepsWith stepHighLevel

evalHighLevelIO :: E -> IO ()
evalHighLevelIO e = do
    let (l, e') = countStepsHighLevel e
    putStrLn $ show l ++ " steps"
    print e'

-- ----------------------------------------
-- Meta variable definitions
exprs :: [(String, (String, T))]
exprs = [ ("F",      ("λx. λy. y",     TBool))
        , ("T",      ("λx. λy. x",     TBool))
        , ("AND",    ("λp. λq. p q p", TBool :-> TBool))
        , ("OR",     ("λp. λq. p p q", TBool :-> TBool))
        , ("NOT",    ("λp. p F T",     TBool :-> TBool))
        
        , ("Z",      ("λf. λx. x",                   TNum))
        , ("0",      ("λf. λx. x",                   TNum))
        , ("1",      ("λf. λx. f x",                 TNum))
        , ("2",      ("λf. λx. f (f x)",             TNum))
        , ("3",      ("λf. λx. f (f (f x))",         TNum))
        , ("4",      ("λf. λx. f (f (f (f x)))",     TNum))
        , ("5",      ("λf. λx. f (f (f (f (f x))))", TNum))

        , ("SUCC",   ("λn. λf. λx. f (n f x)",                           TNum :-> TNum))
        , ("PLUS",   ("λm. λn. λf. λx. m f (n f x)",                     TNum :-> TNum :-> TNum))
        , ("MULT",   ("λm. λn. λf. m (n f)",                             TNum :-> TNum :-> TNum))
        , ("POW",    ("λb. λe. e b",                                     TNum :-> TNum :-> TNum))
        , ("PRED",   ("λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)", TNum :-> TNum))
        , ("SUB",    ("λm. λn. n PRED m",                                TNum :-> TNum))
        , ("ISZERO", ("λn. n (λx. F) T",                                 TNum :-> TBool))
        , ("LEQ",    ("λm. λn. ISZERO (SUB m n)",                        TNum :-> TNum :-> TBool))

        , ("Y",      ("λg. (λx. g (x x)) (λx. g (x x))", (a :-> a) :-> a))
        , ("IF",     ("λp. λa. λb. p a b",               TBool :-> a :-> a :-> a))
        , ("I",      ("λx. x",                           a :-> a))
        , ("K",      ("λx. λy. x",                       a :-> b :-> a))
        , ("S",      ("λx. λy. λz. x z (y z)",           (a :-> b :-> c) :-> (a :-> b) :-> a :-> c))

        , ("PAIR",   ("λx. λy. λf. f x y", a :-> b :-> TPair a b))
        , ("FIRST",  ("λp. p T",           TPair a b :-> a))
        , ("SECOND", ("λp. p F",           TPair a b :-> b))

        , ("NIL",    ("λx. T",             TUnit))
        ]
    where
        a = tvar "a"
        b = tvar "b"
        c = tvar "c"

other = [ "λx. x y λz. z"
        , "λx. (λz. z) x"
        , "λx. (λz. z) x y"
        ]

-- Example programs
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibE :: Int -> E
fibE n = read . concat $
    [ "Y (λr. λn."
    , "  (IF (ISZERO n)"
    , "    (SUCC Z)"
    , "    (IF (ISZERO (PRED n))"
    , "      (SUCC Z)"
    , "      (PLUS (r (PRED n))"
    , "            (r (PRED (PRED n)))))))"
    , "            ", show n, " s z"
    ]

factE :: Int -> E
factE n = read . concat $
    [ "Y (λr. λn. IF (ISZERO n)"
    , "             (SUCC Z)"
    , "             (MULT n (r (PRED n))))"
    , "            ", show n, " s z"
    ]

factCompare = forM_ facts $ \(i,e) -> do
    let (n,  _) = countStepsWith stepHighLevel e
        (n', _) = countStepsWith stepExpand    e
    print (i, n, n')
  where
    facts = [(n, factE n) | n <- [1..5]]

{-# LANGUAGE TupleSections #-}
--  -----------------------------
--
--  LL parser in Haskell (based on the LL parser in Scheme).
--
--  Written by Michael L. Scott for CSC 254, September 2004. Translated to
--  Haskell by Ryan Yates for CSC 254, September 2012. Modified and extended,
--  September 2005, 2006, 2008, 2009, 2011, 2012, 2013, and 2014.
--
--  To test this code, load it into GHCi and type, e.g.
--
--  > let prog = "read a read b sum := a+b write sum write sum/2"
--  > parse (parseTable calcGrammar) prog
--
--  Note that the input program is a list of strings.
--
--  Note, also, that nothing in this file is input-language-specific, other than
--  the tokenizer, sample grammars, and inputs.  For the assignment you'll need
--  to add language-specific code to convert the parse tree into an abstract
--  syntax tree, to enforce semantic rules (if any), and to interpret program
--  input.
--
--  The output from `parse` may be difficult to read.  We include functions to
--  format the output as a tree:
--
--  > printResult $ parse (parseTable calcGrammar) prog
--
--  ------------------------------
module LLParse where

import Prelude hiding (last)
import Data.Char
import Data.List (stripPrefix)

--
-- This first section of the file contains utility routines that you may
-- find helpful.  I recommend you read all the routines carefully.
-- Understanding them (really, deeply understanding them) will help you
-- establish the mindset you need to write the rest of the code.
--

-- | Use the 'Ord' type class to quicksort a list.
sort :: Ord a => [a] -> [a]
sort []     = []
sort xs@[_] = xs
sort (x:xs) = sort as ++ (x : sort bs)
  where
    (as, bs) = partition x xs [] []
    partition _ [] as bs = (as, bs)
    partition e (c:cs) as bs
      | c < e     = partition e cs (c:as) bs
      | otherwise = partition e cs as (c:bs)

-- | Apply a function to the second entry in a pair.
second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b) -- Standard function found in Control.Arrow.

-- | Return list in which adjacent equal elements have been combined.
unique :: Eq a => [a] -> [a]
unique []     = []
unique xs@[_] = xs
unique (x:xs@(y:_)) 
  | x == y    = unique xs
  | otherwise = x : unique xs

-- | Sort (using the 'Ord' instance) and remove duplicates.
uniqueSort :: Ord a => [a] -> [a]
uniqueSort = unique . sort

-- | Return last element of list.
last :: [a] -> a
last [x] = x
last (_:xs) = last xs

-- | Return 'Just' the last element of list or 'Nothing'.
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

-- | Return 'Just' the tail of a list or 'Nothing'.
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- | A 'Grammar' is a list of pairs of non-terminals and 
-- all their right-hand sides.
type Grammar = [(String, [[String]])]

-- | Return left-to-right fringe of tree as list.
gflatten :: Grammar -> [String]
gflatten [] = []
gflatten ((s,ps):ss) = s : concat ps ++ gflatten ss

startSymbol :: Grammar -> String
startSymbol ((s, _):_) = s

-- endMarker :: Grammar -> String
-- endMarker ((_, ps):_) = last (last ps)

-- | Return list of all nonterminals in grammar.
nonterminals :: Grammar -> [String]
nonterminals rs = map fst rs

-- | Return list of all symbols in grammar (no duplicates).
gsymbols :: Grammar -> [String]
gsymbols g = unique . sort . gflatten $ g

-- | Return list of all terminals in grammar.
terminals :: Grammar -> [String]
terminals g = filter (not . (`isNonterminal` g)) . gsymbols $ g

-- |  Is a character a letter, underscore or digit?
isAlphanumeric c = isAlpha' c || isDigit c

-- |  Is a character a letter or underscore?
isAlpha' c = isAlpha c || c == '_'

-- | Does string contain only digits?
isGNumber :: String -> Bool
isGNumber s = case readsInt s of
    [(n, [])] -> True
    _         -> False
  where
    readsInt = reads :: ReadS Int


tokenize :: String -> [(String, String)]
tokenize program = map categorize (reverse toks)
  where
    (toks, _) = go [] program

    getId cs@(c:_) 
      | isAlpha' c = span isAlphanumeric cs
      | otherwise  = ([],cs)
    getInt = span isDigit
    getNum cs =
        case getInt cs of
          a@([],_)   -> a
          (t,'.':c:rs) | isDigit c
                     -> let (t',rs') = getInt rs
                        in  (concat [t,".",t'], rs')
          a          -> a
    getError = break valid
      where
        valid c = c `elem` tokenStart || isAlphanumeric c

    tokenStart = ":+-*/()[]<>=!,"
    tokens = map (,const id)    ["\n","\r","\t"," "]
          ++ map (,(:))         [":=","<=","==","!="]
          ++ map ((,(:)).(:[])) tokenStart

    tconcat [] = Nothing
    tconcat ((Just rs, f):_) = Just (rs,f)
    tconcat (_:ts) = tconcat ts

    go ts [] = ("$$" : ts, [])  -- End of file
    go ts cs = case tconcat . map (\(t,f) -> (stripPrefix t cs, f t)) $ tokens of
        Just (rs,f) -> go (f ts) rs
        Nothing     -> case cs of
           (c:_) | isDigit  c -> let (t,rs) = getNum   cs in go (t:ts) rs
                 | isAlpha' c -> let (t,rs) = getId    cs in go (t:ts) rs
                 | otherwise  -> let (t,rs) = getError cs in go (t:ts) rs


    categorize t
      | t `elem` [ "array", "begin",  "do",    "else",  "end",  "float"
                 , "for",   "if",     "int",   "proc",  "read", "real"
                 , "then",  "trunc",  "while", "write"
                 , ":=",  ":",   "+",  "-",  "*",  "/", "("
                 , ")",   "[",   "]",  "<",  "<=", ">", ">="
                 , "==",  "!=",  ",",  "$$"
                 ]
                   = (t,t)
       | otherwise = case t of
            (c:_) | isDigit  c -> ("num",   t)
                  | isAlpha' c -> ("id",    t)
                  | otherwise  -> ("error", t)


-- | Return list of all productions in grammar.
-- Each is represented as a (lhs rhs) pair, where rhs is a list.
productions :: Grammar -> [(String, [String])]
productions g = [(s,p) | (s,ps) <- g, p <- ps]

-- | Is s a nonterminal?
isNonterminal :: String -> Grammar -> Bool
isNonterminal s g = elem s (nonterminals g)

-- | Is s a symbol in grammar?
isGSymbol :: String -> Grammar -> Bool
isGSymbol s g = elem s (gsymbols g)

-- | Is s a terminal in grammar?
isTerminal :: String -> Grammar -> Bool
isTerminal s g = isGSymbol s g && not (isNonterminal s g)

-- | Join lists together keeping only the unique elements.
union :: Ord a => [[a]] -> [a]
union = unique . sort . concat


-- Two equivalent versions of 'rightContext' are given for your edification.

-- | Return a list of pairs.
-- Each pair consists of a symbol A and a list of symbols beta
-- such that for some alpha, A -> alpha B beta.
rightContext' :: String -> Grammar -> [(String, [String])]
rightContext' b g = [(s, ps) | Just (s, ps) <- map h . productions $ g]
  where
    h (s, []) = Nothing  -- 'b' was not found.
    h (s, p:ps)
      | p == b    = Just (s, ps) -- We found 'b' now return 'beta'.
      | otherwise = h (s, ps)    -- keep looking.

-- | Return a list of pairs.
-- Each pair consists of a symbol A and a list of symbols beta
-- such that for some alpha, A -> alpha B beta.
rightContext :: String -> Grammar -> [(String, [String])]
rightContext b g = [(s, ps) | (s, Just ps) <- map (second h) . productions $ g]
  where
    h = safeTail . dropWhile (/= b)

-- Note: In a list comprehension any values that do not match the pattern
-- binding 's' and 'ps' will be ignored (as opposed to errors).  For example
--
--   ghci> [x | (Just x) <- [Nothing]]
--   []
--   ghci> let f (Just x) = x in f Nothing
--   *** Exception: Non-exhaustive patterns in function f
--


-- --------------------------------------------------------------
--
--  Here is our good friend the calculator language,
--  in the form expected by the parser generator.
--  We've also provided the sum-and-average program
--
--  Note that all symbols in the grammar are 'Strings'.
-- 
calcGrammar =
    [ ("P",  [["SL"]])
    , ("SL", [["S", "SL"], []])
    , ("S",  [["id", ":=", "E"], ["read", "id"], ["write", "E"]])
    , ("E",  [["T", "TT"]])
    , ("T",  [["F", "FT"]])
    , ("TT", [["ao", "T", "TT"], []])
    , ("FT", [["mo", "F", "FT"], []])
    , ("ao", [["+"], ["-"]])
    , ("mo", [["*"], ["/"]])
    , ("F",  [["id"], ["num"], ["(", "E", ")"]])
    ]

-- A program as a list of strings.
sumAndAve = unlines
            [ "read a"
            , "read b"
            , "sum := a+b"
            , "write sum"
            , "write sum/2"
            ]

-- --------------------------------------------------------------
--
--  Here is the extended calculator grammar, with if and while statements.
--  To demonstrate that the language is no longer a complete toy, we've
--  provided a (rather slow) program to compute the first N prime numbers.
--
--  Feel free to experiment with other grammars and inputs.
--
extendedCalcGrammar =
    [ ("P",  [["SL"]])
    , ("SL", [["S", "SL"], []])
    , ("S",  [ ["id", ":=", "E"], ["read", "id"], ["write", "E"]
             , ["if", "C", "SL", "end"], ["while", "C", "SL", "end"]
             ])
    , ("C",  [["E", "rn", "E"]])
    , ("rn", [["=="], ["!="], ["<"], [">"], ["<="], [">="]])
    , ("E",  [["T", "TT"]])
    , ("T",  [["F", "FT"]])
    , ("TT", [["ao", "T", "TT"], []])
    , ("FT", [["mo", "F", "FT"], []])
    , ("ao", [["+"], ["-"]])
    , ("mo", [["*"], ["/"]])
    , ("F",  [["id"], ["num"], ["(", "E", ")"]])
    ]


primes = "read n                           \n\
         \cp := 2                          \n\
         \while n > 0                      \n\
         \    found := 0                   \n\
         \    cf1 := 2                     \n\
         \    cf1s := cf1 * cf1            \n\
         \    while cf1s <= cp             \n\
         \        cf2 := 2                 \n\
         \        pr := cf1 * cf2          \n\
         \        while pr <= cp           \n\
         \            if pr == cp          \n\
         \                found := 1       \n\
         \            end                  \n\
         \            cf2 := cf2 + 1       \n\
         \            pr := cf1 * cf2      \n\
         \        end                      \n\
         \        cf1 := cf1 + 1           \n\
         \        cf1s := cf1 * cf1        \n\
         \    end                          \n\
         \    if found == 0                \n\
         \        write cp                 \n\
         \        n := n - 1               \n\
         \    end                          \n\
         \    cp := cp + 1                 \n\
         \end"

-- --------------------------------------------------------------
--
--  Next comes the parser generator.
--  The main entry routine is 'parseTable', which takes a grammar as argument
--  (in the format shown above) and returns an LL(1) parse table as a result.
--  The table looks like the grammar, except that each RHS is replaced with a
--  (predict-Set, RHS) pair.  The computational heart of the algorithm is function
--  getKnowledge.
--
--  Much of the following employs a "Knowledge" data type.
--  It's a list of "KnowledgeEntry"s, one for each nonterminal,
--    in the same order those nonterminals appear in the grammar
--    (the order is important).

type ParseTable = [(String, [([String], [String])])]

data KnowledgeEntry = KnowledgeEntry
    { knowNonterminal :: String   -- ^ Nonterminal A [not needed computationally,
                                  -- but included for readability of output]
    , knowEpsilon     :: Bool     -- ^ Do we currently think A can -->* epsilon
    , knowFirst       :: [String] -- ^ (current guess at) FIRST(A)
    , knowFollow      :: [String] -- ^ (current guess at) Follow(A)
    }
  deriving (Show, Eq)

type Knowledge = [(String, KnowledgeEntry)]

-- | Return knowledge structure with empty FIRST and FOLLOW sets
-- and false gen-epsilon estimate for all symbols.
initialKnowledge :: Grammar -> Knowledge
initialKnowledge = map (\a -> (a, KnowledgeEntry a False [] [])) . nonterminals

-- | Return KnowledgeEntry for a given A.
symbolKnowledge :: String -> Knowledge -> Maybe KnowledgeEntry
symbolKnowledge = lookup

-- | Can w generate epsilon based on current estimates?
-- if w is a terminal, no
-- if w is a nonterminal, look it up
-- if w is an empty list, yes
-- if w is a non-empty list, "iterate" over elements
generatesEpsilon :: String -> Knowledge -> Grammar -> Bool
generatesEpsilon w k g 
    | w `isTerminal` g = False
    | otherwise        = maybe False knowEpsilon (symbolKnowledge w k)

-- | Return FIRST(w), based on current estimates.
-- if w is a terminal, return (w)
-- if w is a nonterminal, look it up
-- if w is an empty list, return []  (empty set)
-- if w is a non-empty list, "iterate" over elements
first :: [String] -> Knowledge -> Grammar -> [String]
first [] _ _ = []
first (w:ws) k g = helper w ++ if generatesEpsilon w k g 
                                 then first ws k g
                                 else []
  where
    helper w
      | w `isTerminal` g = [w]
      | otherwise        = maybe [] knowFirst (symbolKnowledge w k)

-- | Return FOLLOW(A), based on current estimates.
-- Simply look it up.
follow :: String -> Knowledge -> [String]
follow a k = maybe [] knowFollow (symbolKnowledge a k)

-- | Return knowledge structure for grammar.
-- Start with 'initialKnowledge grammar' and "iterate",
-- until the structure doesn't change.
-- Uses 'rightContext b grammar', for all nonterminals 'b',
-- to help compute follow sets.
getKnowledge :: Grammar -> Knowledge
getKnowledge g = helper (initialKnowledge g)
  where
    rcs = map (`rightContext` g) (nonterminals g) :: [[(String, [String])]]
    helper k | k == k'   = k
             | otherwise = helper k'
      where
        k' = zipWith3 update k g rcs
        update :: (String, KnowledgeEntry) -> (String, [[String]]) -> [(String, [String])] -> (String, KnowledgeEntry)
        update (_, KnowledgeEntry nt e kfirst kfollow) (_, ps) cs = 
            (nt, KnowledgeEntry
              nt
              (e || or (map gen ps))
              (union $ [kfirst, concatMap first' ps])
              (union $ [kfollow, concatMap first' (map snd cs), concatMap ff cs]))
          where
            first' s  = first s k g
            gen    ss = all (\s -> generatesEpsilon s k g) ss
            ff (s,w) 
              | gen w     = follow s k
              | otherwise = []


-- | Add a production to a grammar that includes an explicit end of file
-- and wraps the original starting production. *)
augmentWithStart :: Grammar -> Grammar
augmentWithStart [] = []
augmentWithStart g@((p,_):_) = ("$Start", [[p, "$$"]]) : g

-- | Return parse table for grammar.
-- Uses the 'getKnowledge' function above.
parseTable :: Grammar -> ParseTable
parseTable g = map h g'
  where
    g' = augmentWithStart g
    k = getKnowledge g'
    h (l, rs) = (l, map f rs)
      where
        f r = (union [first r k g', if all (\r -> generatesEpsilon r k g') r then follow l k else []], r)

-- | Double-index to find prediction (list of RHS symbols) for
-- nonterminal nt and terminal t.
-- Return 'Nothing' if not found.
parseLookup :: String -> String -> ParseTable -> Maybe [String]
parseLookup nt t table = maybe Nothing helper (lookup nt table)
  where
    helper []       = Nothing
    helper ((as,bs):ps)
      | t `elem` as = Just bs
      | otherwise   = helper ps

--
-- The main parse routine below returns a parse tree as a 'Right' value
-- (or a Left error message if the program is syntactically invalid).
-- To build that tree it employs a simplified version of the "attribute
-- stack" described in Section 4.5.2 (pages 54-57) of the PLP CD.
--
-- When it predicts A -> B C D, the parser pops A from the parse stack
-- and then, before pushing D, C, and B (in that order), it pushes a
-- number (in this case 3) indicating the length of the right hand side.
-- It also pushes A into the attribute stack.
--
-- When it matches a token, the parser pushes this into the attribute
-- stack as well.
--
-- Finally, when it encounters a number (say k) in the stack (as opposed
-- to a character string), the parser pops k+1 symbols from the
-- attribute stack, joins them together into a list, and pushes the list
-- back into the attribute stack.
--
-- These rules suffice to accumulate a complete parse tree into the
-- attribute stack at the end of the parse.
--
-- Note that everything is done functionally.  We don't really modify
-- the stacks; we pass new versions to a tail recursive routine.
--

-- | Extract grammar from a 'ParseTable', so we can invoke the various 
-- routines that expect a grammar as argument.
toGrammar :: ParseTable -> Grammar
toGrammar table = map (second (map snd)) table

-- A generic tree with n branches to hold the parse.
data Tree a = Node a (Forest a)
    deriving (Show, Eq)
type Forest a = [Tree a]
type ParseTree = Tree String
type AttributeStack = Forest String

-- A nice function for getting a nice display of a tree.
-- The definition of Tree above and this drawing method
-- are derived from the Data.Tree module.
drawTree :: ParseTree -> String
drawTree t = unlines (draw t)

drawForest :: Forest String -> String
drawForest f = unlines (map drawTree f)

draw :: ParseTree -> [String]
draw (Node a ts) = a : drawSubTrees ts

drawSubTrees :: Forest String -> [String]
drawSubTrees []     = []
drawSubTrees [t]    = shift "`- " "   " (draw t)
drawSubTrees (t:ts) = shift "+- " "|  " (draw t) ++ drawSubTrees ts

shift _ _ []     = []
shift f _ [s]    = [f ++ s]
shift f o (s:ss) = (f ++ s) : map (o++) ss

-- | Pop n + 1 symbols off the attribute stack,
-- assemble into a production, and push back onto the stack.
reduceOneProd :: AttributeStack -> Int -> AttributeStack
reduceOneProd as n = h (1+n) as []
  where
    h 0 as     (Node p []:ps) = Node p (ps) : as
    h n (a:as) ps     = h (n-1) as (a:ps)

-- Distinguish between numbers on the stack and symbols.
data ParseStackEntry = PNum Int | PSym String
    deriving (Show, Eq)

-- Unwraps the augmented ParseTree to remove the added start root
-- and explicit end of file $$
unwrap :: ParseTree -> Either String ParseTree
unwrap (Node "$Start" [p, Node "$$" []]) = Right p
unwrap _                                 = Left "expected parse wrapper"

-- | Main parsing function.
parse :: ParseTable -> String -> Either String ParseTree
parse table p = helper [PSym (startSymbol g)] (tokenize p) [] >>= unwrap
  where
    g = toGrammar table

    die s = Left ("syntax error: " ++ s) -- We die on the first error

    helper [] [] (a:_)  = Right a
    helper [] _  _      = die "extra input beyond end of program"
    helper (PNum n:ps) ts as = 
        -- We've reached the end of a production.  Pop lhs and rhs
        -- symbols off astack, join into list, and push back into astack.
        helper ps ts (reduceOneProd as n)
    helper _      [] as = die "unexpected end of program"
    helper (PSym t:ps) ts'@((term,token):ts) as
        | t `isTerminal` g = 
            if t == term
              then helper ps ts (Node token []:as) -- note push of token into astack
              else die (concat ["expected ", t, "; saw ", token])
        | otherwise        =
            case parseLookup t term table of
              Just rhs -> 
                helper
                  (map PSym rhs ++ PNum (length rhs) : ps)
                  ts'
                  (Node t []:as) -- note push of lhs into astack
              Nothing  -> die (concat ["no prediction for ", t, " when seeing ", token])

printResult :: Either String ParseTree -> IO ()
printResult (Left e)  = putStrLn e
printResult (Right t) = putStrLn (drawTree t)

-- --------------------------------------------------------------
--
-- Everything above this point in the file is complete and (I think)
-- usable as-is.  The rest of the file, from here down, is a skeleton
-- for the extra code you need to write.  It has been excised from my
-- working solution to the assignment.  You are welcome, of course, to
-- use a different organization if you prefer.  This is provided in the
-- hope you may find it useful.
--

-- First some types to represent the abstract syntax
type AST = [Statement]
type Ident = String
type Value = Integer

data Expr = Lit Value | Var Ident | Op String Expr Expr
  deriving (Show, Eq)

data Statement = Ident := Expr 
               | Read Ident 
               | Write Expr 
               | If Cond [Statement]
               | While Cond [Statement]
  deriving (Show, Eq)

data Cond = Cond String Expr Expr
  deriving (Show, Eq)

-- You will need to write code to map from the `ParseTree`
-- to the AST.  Where you see 'undefined' you will have
-- to fill in with an actual implementation.
toAstP :: ParseTree -> AST
toAstP (Node "P" [sl]) = toAstSL sl

-- Replace the 'something' with a pattern match which will bind the
-- correct 's' and 'sl' so the RHS can be:  toAstS s : toAstSL sl
toAstSL :: ParseTree -> AST
toAstSL (Node "SL" [s, sl]) = toAstS s : toAstSL sl
toAstSL _ = []

toAstS :: ParseTree -> Statement
-- Here you will want a pattern match on each LHS matching
-- each RHS of the Statement data type.
toAstS (Node "S" [Node i [], Node ":=" [], e]) = i := toAstE e
-- check below:
toAstS (Node "write" [e]) = Write (toAstE e)
toAstS (Node "read" [Node i []]) = Read i
toAstS (Node "if" [c, sl, eNd]) = If (toAstC c) (toAstSL sl)
toAstS (Node "while" [c, sl, eNd]) = While (toAstC c) (toAstSL sl)

toAstC :: ParseTree -> Cond
-- the code below needs to be edited:
toAstC (Node "C" [el, Node "rn" [Node cs []], er]) = Cond cs (toAstE el) (toAstE er)
toAstC _ = undefined

-- You can write 'toAstE' as a pair of functions.  The
-- first handling the E, T, or F cases:
toAstE :: ParseTree -> Expr
-- First the base cases from 'F'
--   toAstE (Node "F" ...) =
--   toAstE (Node "F" ...) =
-- then build terms or factor
--   toAstE (Node _ ...) = toAstETail ...
toAstE (Node "E" [t, tt]) = toAstETail (toAstE t) tt
toAstE (Node "T" [f, ft]) = toAstETail (toAstE f) ft
toAstE (Node "F" [Node xs@(x:_) []]) 
  | isAlpha' x = Var xs
  | otherwise  = Lit (read xs)
 -- toAstE (Node "")

-- The second function 'toAstETail' handles TT and FT
toAstETail :: Expr -> ParseTree -> Expr
-- toAstETail e (Node _ ...) = ... (toAstE ...) ...
-- toAstETail e ... = ...
toAstETail e (Node "TT" [Node "ao" [Node ao []], t, tt]) = toAstETail (Op ao e (toAstE t)) tt
toAstETail e (Node "FT" [Node "mo" [Node mo []], t, tt]) = toAstETail (Op mo e (toAstE t)) tt
toAstETail e (Node _ []) = e
-- -----------------------------------------------------

-- Model the state of the calculator
data Environment = Env 
    { envValues :: [(String, Value)]
    , envInput  :: [String]
    , envOutput :: [String]
    }
  deriving (Show, Eq)

-- Memory

-- Be sure to understand this type!  Use 'Left' values to
-- indicate errors.
lookupEnv :: String -> Environment -> Either String Value
lookupEnv _ _ = undefined

updateEnv :: String -> Environment -> Value -> Environment
updateEnv _ _ _ = undefined

-- IO

readEnv :: Environment -> Either String (Environment, Value)
readEnv _ = undefined

writeEnv :: Environment -> String -> Environment
writeEnv _ = undefined

-- -------------------------------------------------------

-- The next two functions are complete and illustrate using
-- 'do' notation to handle errors without our writing explicit
-- handling of error cases.
interpret :: ParseTable -> String -> [String] -> Either String [String]
interpret table program input = do
    t <- parse table program
    let ast = toAstP t
    interpretAst ast input

interpretAst :: AST -> [String] -> Either String [String]
interpretAst ast input = do
    (Env _ _ output) <- interpretSL ast (Env [] input [])
    Right $ reverse output

interpretSL :: [Statement] -> Environment -> Either String Environment
interpretSL _ _ = undefined

interpretS :: Statement -> Environment -> Either String Environment
interpretS _ _ = undefined

interpretCond :: Cond -> Environment -> Either String Bool
interpretCond _ _ = undefined

interpretE :: Expr -> Environment -> Either String Value
interpretE _ _ = undefined

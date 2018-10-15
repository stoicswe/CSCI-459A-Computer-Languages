import Data.Char
import System.Environment
import System.IO
import Control.Monad

data Token
    = Keyword String
    | Symbol String
    | Identifier String
    | OpenParen
    | ClasedParen
    | OpenBrace
    | ClosedBrace
    | Error String

scan :: String -> [Token]
scan [] = []
scan cs = go cs []
    where
        go [] as = [tokenize as]
        go (c:cs) as
            | isSpace c = tokenize as : skip cs
            | otherwise = go cs (as ++ [c])
        
        skip [] = []
        skip (c:cs)
            | isSpace c = skip cs
            | otherwise = go cs [c]
        
        tokenize [] = EOF
        tokenize s
            | s == "(" = OpenParan
            | s == ")" = ClosedParan
            | s == "{" = OpenBrace
            | s == "}" = ClosedBrace
            | isKeyword s = keyword s
            | all isSymbol s = symbol s
            | isIdentifier s = --what goes here?
        
isKeyword s = s `elem` ["class", "if", "for"]

isIdentifier (c:cs)
    isLetter c && all (cs)
    where
        isIdentifierChar c
            isLetter c = True
            c == '_' = True
            otherwise = False

main :: IO ()
main = do
    [f] <- getArgs
    cs <- readFile f
    --foreach(char c in cs){ }
    forM_ (scan cs) $ \c -> do
        print c
    --putStrLn cs
import System.Environment

main :: IO ()
main = do
    as <- getArgs
    case as of
      [] -> putStrLn "No arguments given."
      _  -> putStrLn . unlines $ "You gave the arguments" 
              : [show i ++ ": " ++ a | (i,a) <- zip [0..] as]
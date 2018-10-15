import Prelude hiding (flip)
import System.Environment
import System.Random
import Text.Show.Functions
import qualified Data.Set as S

{-
type Bracelet = [Int]

each bracelet is described by a list of colors, in which each color
simultaniously represents a bead, this is done woth each color is an
integer.
-}
type Bracelet = [Int]
{-
unionMap :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
inputs: Set a, Set b
returns: combined set of a+b

this function puts two sets together then returns
the result of it.
-}
unionMap :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
unionMap f = S.foldr S.union S.empty . S.map f
{-
rotate :: Bracelet -> Bracelet
input: Bracelet
returns: Bracelet

this function roatates one color from the front to
the back of the bracelet.
-}
rotate :: Bracelet -> Bracelet
rotate [] = []
rotate (b:bs) = bs ++ [b]
{-
flip :: Bracelet -> Bracelet
input: Bracelet
returns: Bracelet

this function will completely flip a Bracelet around
via reversing it.
-}
flip :: Bracelet -> Bracelet
flip = reverse
{-
canon :: Bracelet -> Bracelet
input: Bracelet
return: Bracelet

this function will generate all possible bracelets
from a basic bracelet, then return a unique one from
the top of the list
-}
canon :: Bracelet -> Bracelet
canon = minimum . equivalent
{-
equivalent :: Bracelet -> [Bracelet]
inputs: Bracelet
returns: list of type bracelet

equivalent performs a recursive call to itself 
until it generates all the bracelets, based on 
a basic bracelet, this is done through
-}
equivalent :: Bracelet -> [Bracelet]
equivalent b = let bs = rots b 
               in  bs ++ map flip bs
rots b = go (length b) b
    where
        go 0 _ = []
        go n b = b : go (n-1) (rotate b)
{-
bracelets :: Int -> Int -> S.Set(Bracelet)
inputs: colors beadCount
returns: Set of Bracelet type

This function will build a basic nracelet through
recursive call by counting down from n color to 0
this will also add colors via use of plusOne until
the length condition is met for that one bracelet

As the recursive call is made, each iteration adds a
bracelet to the Set via unionMap

# monkey operator == return? the hek?
-}
bracelets :: Int -> Int -> S.Set(Bracelet)
bracelets c b = go b
    where
        go 0 = S.singleton([])
        go 1 = S.fromList $ map (:[]) [1..c] -- #
        go b = let s = go (b-1)
               in  unionMap (plusOne c) (s)
{-
plusOne :: Int -> Bracelet -> S.Set(Bracelet)
inputs: colors, Bracelet
returns: Set of type Bracelet

adds one color to a basic bracelet until the
entire bracelet is generated.
-}
plusOne :: Int -> Bracelet -> S.Set(Bracelet)
plusOne colors b = S.fromList $ go colors
    where 
        go 0 = [] -- (c:) == ([c] ++)
        go c = map (canon . (c:)) (rots b) ++ go (c-1)
        -- go c = (map canon . map (c:) $ (equivalent b) )++ go (c-1)

{-
main :: IO ()

this is the main function that runs all the necessary
functions to generate the bracelets.
-}
main :: IO ()
main = do
    [a1, a2] <- map read <$> getArgs
    putStrLn "Generating bracelets..."
    let final_b = bracelets a1 a2
    putStrLn "The number of bracelets generated is:"
    print $ S.size final_b
    return ()
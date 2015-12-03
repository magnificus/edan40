import Data.Char
import Data.List
import Data.Ord

type AlignmentType = (String,String)
type AlTypeType = (Int, [AlignmentType])


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

al = (string1, string2)

alT = (4 :: Int, [al])

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _ = [("fuck", "off")]

correctForm f (a, b) = f a b

similarityScore :: Eq a => [a] -> [a] -> Int
similarityScore xs ys = simScore (length xs) (length ys)
  where
    simScore i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..]]
    mcsEntry :: Int -> Int -> Int
    mcsEntry x 0 = x * scoreSpace
    mcsEntry 0 y = y * scoreSpace
    mcsEntry i j
      | x == y  = scoreMatch + simScore (i-1) (j-1)
      | otherwise = maximum [(simScore i (j-1) + scoreSpace),
                        (simScore (i-1) j + scoreSpace), (simScore (i-1) (j-1) + scoreMismatch)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

		 
optAlignments :: String -> String -> AlTypeType
optAlignments xs ys = optAl (length xs) (length ys)
  where
    optAl i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..]]
    mcsEntry :: Int -> Int -> AlTypeType
    mcsEntry x 0 = (x * scoreSpace, [("", minuses x)])
    mcsEntry 0 y = (y * scoreSpace, [(minuses y, "")])
    mcsEntry i j = mergeAlternatives $ maximaBy fst $ [createAlternative (optAl (i-1) (j-1)) (score x y) x y, createAlternative (optAl i (j-1)) (score '-' y) '-' y,
                        createAlternative (optAl (i-1) j) (score x '-') x '-']
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
		 
createAlternative :: AlTypeType -> Int -> Char -> Char -> AlTypeType
createAlternative a x h1 h2 = (x + fst(a), attachTails h1 h2 (snd a))

mergeAlternatives :: [AlTypeType] -> AlTypeType
mergeAlternatives = foldl (\a b -> (fst b, snd(a) ++ snd(b))) (0,[])

score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y = scoreMatch
  | otherwise = scoreMismatch

minuses = flip replicate '-'

combinations :: String -> String -> [AlignmentType]
combinations x [] = [(x, minuses (length x))]
combinations [] y = [(minuses (length y) ,y )]
combinations (x:xs) (y:ys) = attachHeads x y (combinations xs ys) ++ attachHeads x '-' (combinations xs (y:ys)) ++ attachHeads '-' y (combinations (x:xs) ys)
 

addpossible :: Char -> Char -> [AlignmentType] -> [AlignmentType]
addpossible x y zs
  | zs == [] = [([x],[y]),([x], ['-']), (['-'], [y])]
  | otherwise = attachHeads x y zs ++ attachHeads x '-' zs ++ attachHeads '-' y zs

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails  :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails  h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaby f [] = []
maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]

--optAlignments :: String -> String -> [AlignmentType]
--optAlignments [] _ = [("","")]
--optAlignments _ [] = [("","")]
--optAlignments s1 s2 = [x | x <- combinations s1 s2, (correctForm similarityScore x) == similarityScore s1 s2]

outputOptAlignments string1 string2 = do
	putStrLn ("There are " ++ ( show (length (snd alig))) ++ " optimal solutions")
	putStrLn $ printAligs $ snd alig
	where
		alig = optAlignments string1 string2

printAligs :: [AlignmentType] -> String
printAligs [] = []
printAligs (a:as) = ("\n" ++ (fst a) ++ "\n" ++ (snd a)) ++ printAligs as
 
type AlignmentType = (String,String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _ = [("fuck", "off")]

similarityScore :: String -> String -> Int
similarityScore [] y = length y * scoreSpace
similarityScore x [] = length x * scoreSpace
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y, similarityScore (x:xs) ys + scoreSpace, similarityScore xs (y:ys) + scoreSpace]

score x y
  | x == y = scoreMatch
  | otherwise = scoreMismatch

combinations :: String -> String -> [AlignmentType]
combinations _ [] = []
combinations [] _ = []
combinations (x:xs) (y:ys) = addpossible x y (combinations xs ys)

addpossible :: Char -> Char -> [AlignmentType] -> [AlignmentType]
addpossible x y zs
  | zs == [] = [([x],[y]),([x], ['-']), (['-'], [y])]
  | otherwise = attachHeads x y zs ++ attachHeads x '-' zs ++ attachHeads '-' y zs

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaby f [] = []
maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] _ = [("","")]
optAlignments _ [] = [("","")]
optAlignments s1 s2 = [x | x <- combinations s1 s2, similarityScore (fst x) (snd x) == similarityScore s1 s2]

type AlignmentType = (String,String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"
al = (string1, string2)

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _ = [("fuck", "off")]

similarityScore :: String -> String -> Int
similarityScore [] y = length y * scoreSpace
similarityScore x [] = length x * scoreSpace
similarityScore (x:xs) (y:ys) = maximum [(similarityScore xs ys + score x y), (similarityScore (x:xs) ys + score '-' y), (similarityScore xs (y:ys) + score x '-')]

correctForm f (a, b) = f a b

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1))
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y = scoreMatch
  | otherwise = scoreMismatch

minuses n
  | n == 0 = []
  | otherwise = '-': minuses (n-1)

combinations :: String -> String -> [AlignmentType]
combinations x [] = [(x, minuses (length x))]
combinations [] y = [(y, minuses (length y))]
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
optAlignments s1 s2 = [x | x <- combinations s1 s2, (correctForm similarityScore x) == similarityScore s1 s2]

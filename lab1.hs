map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
    
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute a (x:xs) bs
	| x == a = bs ++ substitute a xs bs
	| otherwise = x : substitute a xs bs

match :: Eq a => a -> [a] -> [a] -> Maybe [a]


match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing	
match wc (p:ps) (s:ss)
  | p == wc = orElse (singleWildcardMatch (wc:ps) (s:ss)) (longerWildcardMatch (wc:ps) (s:ss))
  | p == s  = match wc ps ss
  | otherwise = Nothing
  
longerWildcardMatch (wc:ps) (s:ss) = mmap (s:) (match wc (wc:ps) ss)
 
singleWildcardMatch (wc:ps) (s:ss) = match wc ps ss >> return [s]
	
transformationApply :: (Eq a) => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]

transformationApply wc f s map = mmap (substitute wc (snd map)) (mmap f (match wc (fst map) s))

transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f [] map = Nothing
transformationsApply wc f p:ps map 
	| transformationApply wc p map != Nothing = transformationApply wc p map
	| otherwise = transformationsApply wc f ps map

frenchPresentation = ("My name is *", "Je m'appelle *")
frenchPresentations = [("My name is *", "Je m'appelle *"), ("FUCK *", "Tits*")]

rulesApply :: [PhrasePair] -> Phrase -> Phrase

reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs) = orElse lookup ++ reflect (xs)

rulesApply (a, b) = transformationApply

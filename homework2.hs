-- fn: 81786

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = insertBy f x (sortBy f xs)

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x (y:ys) | ((f x y) == EQ)   = x:y:ys
                    | ((f x y) == LT)   = x:y:ys
                    | otherwise = y:(insertBy f x ys)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] =  []
groupBy f (x:xs) =  let (y,z) = span (f x) xs 
                    in (x:y) : groupBy f z

--help functions
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> ( f (g x) (g y) )

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
f &&& g = \x -> (f x , g x)

applyOn' :: (a -> b) -> [a] -> [(b, a)]
applyOn' _  [] = []
applyOn' f (x:xs)= ((f &&& id) x) : applyOn' f (xs)
--

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f (x:xs) = map snd (sortBy ((compare) `on` fst) (applyOn' f (x:xs)))

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f (x:xs) = map (map snd) (groupBy ((==) `on` fst) (applyOn' f (x:xs)))

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f (x:xs) = groupOn f (sortOn f (x:xs))

--bonus
data NonEmpty a = a :| [a]
{-  
groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupByNonEmpty (==)

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] =  []
groupByNonEmpty f (x:xs) =  let (y,z) = span (f x) xs 
                            in (x :| (x:y)) : groupByNonEmpty f z

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f (x:xs) = map (map snd) (groupByNonEmpty ((==) `on` fst) (applyOn' f (x :|(x:xs))))

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f (x:xs) = groupOn f (sortOn f (x :|(x:xs)))
-}
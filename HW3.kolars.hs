module HW3 where
import Data.list

-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--

compress :: Eq a => [a] -> [(Int,a)]
compress = map entry . chunk
	where entry ys = (length ys, head ys)

group :: Eq a => [a] -> [[a]]
group 	= []
group (x,xs) = let (match,rest) = getAll x xs
				in match : group rest

getAll :: Eq a => a -> [a] -> ([a], [a])
getAll x [] 	= ([x], [])
getAll x (y:ys) = if x == y then let (match,rest) = getAll x ys
					 			 in (y:match, rest)
					 		else ([x], y:ys)

-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress = concatMap (uncurry replicate)

--runlengthDe :: [(Int,a)] -> [a]
--runlengthDe compressedRun = replicate (read $ init compressedRun) (last compressedRun)

-- curry :: ((a,b) -> c) -> a->b->c 	
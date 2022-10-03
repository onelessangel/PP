import Data.List

partitions :: [a] -> [[[a]]]
-- partitions xs = res
--     where
--         list = getCombinations xs
--         revList = reverse $ rev $ list
--         doubleResult = zipWith (\x y -> zipWith (\a b -> [a, b]) x y) list revList
--         importantPart = take sizeInt doubleResult
--         makeAlmostRes = foldl(\acc x -> acc ++ x) [] importantPart
--         almostAlmostRes = drop 1 makeAlmostRes ++ [[xs]]
--         res = [subsets 1 xs] ++ almostAlmostRes

--         len = length $ doubleResult
--         size = (fromIntegral len) / 2
--         sizeInt = round size

-- subsets 0 _ = [[]]
-- subsets _ [] = []
-- subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

-- getCombinations xs = reverse $ createRes 0 []
--     where
--         size = length xs
--         createRes k res
--             | k == size + 1 = res
--             | otherwise = createRes (k + 1) (subsets k xs : res)

-- rev list3 = map (\x -> reverse x) list3

-- partitions [] = [[]]
-- partitions (x : xs) = firstPart ++ secondPart
--     where
--         firstPart = [[x] : partition | partition <- partitions xs]
--         secondPart = [(x : y) : ys | (y : ys) <- partitions xs]
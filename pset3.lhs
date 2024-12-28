> permutations :: [a] -> [[a]]
> permutations [] = [[]]
> permutations (x:xs) = [ zs | ys <- permutations xs, zs <- include x ys ]

> include :: a -> [a] -> [[a]]
> include x [] = [[x]]
> include x (y:ys) = (x:y:ys) : map (y:) (include x ys)

> permutes :: [a] -> [[a]]
> permutes = foldr (\x ys -> [zs | y <- ys, zs <- include x y]) [[]]

> includer :: a -> [a] -> [[a]]
> includer x  = foldr (\y ys -> (x: head ys) : map (y:) ys) [[x]]

> cp :: [[a]] -> [[a]]
> cp = foldr (\xs xss -> [x:ys| x <- xs, ys <- xss]) [[]]

> cols :: [[a]] -> [[a]]
> cols = foldr (\xs xss -> zipWith (:) xs xss) (repeat [])

> scale a mat = [ [a * elem | elem <- row]|row <- mat]

> dot as bs =  sum (zipWith (*) as bs)

> add= zipWith (zipWith (+))

> mul matA matB = [[dot row col | col <- (cols matB)]|row <- matA]
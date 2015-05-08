module Main where
mapl op l = foldl f i l
where
f as a = as ++ [(op a)]
i :: [a]
i = []

mapr op l = foldr f i l
where
f a as = [(op a)] ++ as
i :: [a]
i = []

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' op (x:xs) = (op x) ++ (concatMap' op xs)

concatMapl op l = foldl f i l
where
f as a = as ++ (op a)
i :: [a]
i = []

concatMapr op l = foldr f i l
where
f a as = (op a) ++ as
i :: [a]
i = []

oper :: Int -> [Int]
oper x = [x+1, x+2, x+3]
main = print ( concatMapl oper [0,1,2] )
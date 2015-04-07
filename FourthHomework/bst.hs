data Tree = Empty | Node Tree Integer Tree deriving (Show, Read, Eq)

singleton :: Integer->Tree
singleton x = Node Empty x Empty

insertElem :: Integer->Tree->Tree
insertElem x Empty = singleton x

insertElem x (Node left a right)
	| x == a = Node left x right
	| x <  a = Node (insertElem x left) a right
	| x >  a = Node left a (treeInsert x right)

findElem :: Integer -> Tree -> Bool
findElem x Empty = False

findElem x (Node left a right)
	| x == a = True
	| x <  a = findElem x left
	| x >  a = findElem x right

treeHeight :: Tree -> Integer
treeHeight Empty = 0
treeHeight (Node l _ r) = (max(treeHeight l)(treeHeight r)) + 1

sumElem :: Tree -> Integer
sumElem Empty = 0;

sumElem (Node left e right) = e + sumElem left + sumElem right



import Data.List

{- Recursive functions on lists -}
isAscending :: [Int] -> Bool
isAscending _ = undefined

myTake :: Int -> [a] -> [a]
myTake _ _ = undefined

dropOdds :: [a] -> [a]
dropOdds _ = undefined

myIntersperse :: [a] -> a -> [a]
myIntersperse _ _ = undefined

myReverseRec :: [a] -> [a]
myReverseRec _ = undefined

myReverseFold :: [a] -> [a]
myReverseFold _ = undefined

{- Higher-order functions -}
myMap :: (a -> b) -> [a] -> [b]
myMap _ _ = undefined

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ _ _ = undefined

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ _ _ = undefined

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ _ = undefined


{- Data structures -}
data ArithExpr =
      Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    | Div ArithExpr ArithExpr
    | Value Int

evalExpr :: ArithExpr -> Int
evalExpr _ = undefined

showExpr :: ArithExpr -> String
showExpr _ = undefined

safeHead :: [a] -> Maybe a
safeHead _ = undefined

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ _ = undefined

addSafeDiv :: (Int, Int) -> (Int, Int) -> Maybe Int
addSafeDiv _ _ = undefined

data Tree a = Leaf | Node a [Tree a]

sumTree :: Tree Int -> Int
sumTree _ = undefined

reverseTree :: Tree a -> Tree a
reverseTree _ = undefined

data BinaryTree a = BLeaf | BNode a (BinaryTree a) (BinaryTree a)

toTree :: BinaryTree a -> Tree a
toTree _ = undefined

fromTree :: Tree a -> Maybe (BinaryTree a)
fromTree _ = undefined

{- Sorting -}
mergeSort :: (Ord a) => [a] -> [a]
mergeSort _ = undefined

{- Connected Words -}
printStats :: IO ()
printStats = undefined

connectedWords :: IO [String]
connectedWords = undefined

connectionMap :: [(Char, [Char])]
connectionMap =
    [
        ('a', "aqzsw"),
        ('b', "bvghn"),
        ('c', "cxdfv"),
        ('d', "dxserfc"),
        ('e', "ewsdr"),
        ('f', "fdrtgvc"),
        ('g', "gftyhbv"),
        ('h', "hgyujnb"),
        ('i', "iujklo"),
        ('j', "jhuikmn"),
        ('k', "kjiolm"),
        ('l', "lkop"),
        ('m', "mnjk"),
        ('n', "nbhjm"),
        ('o', "oiklp"),
        ('p', "pol"),
        ('q', "qwa"),
        ('r', "redft"),
        ('s', "sawedxz"),
        ('t', "trfgy"),
        ('u', "uyhji"),
        ('v', "vcfgb"),
        ('w', "wqase"),
        ('x', "xzsdc"),
        ('y', "ytghu"),
        ('z', "zasx")
    ]

isConnected :: String -> Bool
isConnected _ = undefined

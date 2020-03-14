module Lists
() where

-- A list is either empty or it is composed of a first element (head) and a tail, which is a list itself.


-- 1.01 (*) Find the last element of a list.
lst :: [a] -> Maybe a
lst []  = Nothing
lst [x] = Just x
lst (x:xs) = lst xs 

-- 1.02 (*) Find the last but one element of a list. 
secondLast :: [a] -> Maybe a
secondLast [] = Nothing
secondLast [x] = Nothing
secondLast [x, y] = Just x
secondLast (x:xs) = secondLast xs

-- 1.03 (*) Find the K'th element of a list.
kthElement :: (Ord k, Num k) => [a] -> k -> Maybe a
kthElement [] _ = Nothing
kthElement (x:xs) k 
    | k <= 0 = Nothing
    | k == 1 = Just x
    | otherwise = kthElement xs (k-1)

-- 1.04 (*) Find the number of elements of a list.
numOfElements :: (Num n) => [a] -> n
numOfElements [] = 0
numOfElements (_:xs) = 1 + numOfElements xs 

-- 1.05 (*) Reverse a list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- 1.06 (*) Find out whether a list is a palindrome. 
isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom (x:xs) = (x:xs) == reverseList (x:xs) 

-- 1.07 (**) Flatten a nested list structure. 
-- Transform a list, possibly holding lists as elements into a 'flat' list by replacing each list with its elements (recursively).
-- Example:
-- ?- flattenNestedList([a, [b, [c, d], e]], X).
-- X = [a, b, c, d, e]

-- Haskell does not have Nested List type. We need to create it first.
data NestedList a 
    = Element a 
    | List [NestedList a]
flattenNestedList :: NestedList a -> [a]
flattenNestedList (Element x) = [x]
flattenNestedList (List []) = []
flattenNestedList (List (x:xs)) = flattenNestedList x ++ flattenNestedList (List xs)

-- 1.07' (**) Flatten a list of list structure. 
flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList (x:xs) = x ++ flattenList xs


-- 1.08 (**) Eliminate consecutive duplicates of list elements.
--    If a list contains repeated elements they should be replaced with a single copy of the element. 
--    The order of the elements should not be changed.

--    Example:
--    ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
--    X = [a,b,c,a,d,e]
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) 
    | x == head xs = compress xs
    | otherwise = [x] ++ compress xs

-- 1.09 (**) Pack consecutive duplicates of list elements into sublists.
--    If a list contains repeated elements they should be placed in separate sublists.
--    Example:
--    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
--    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]    
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) 
    | x == head xs = (x : head (pack xs)) : tail (pack xs)
    | otherwise = [x] : pack xs

-- 1.10 (*) Run-length encoding of a list.
--    Use the result of problem 1.09 to implement the so-called run-length encoding data compression method. 
--    Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
--    Example:
--    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
--    X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]
lengthEncodedList :: (Eq a) => [a] -> [(Int, a)]
lengthEncodedList = lengthEncodedPack . pack

lengthEncodedPack :: [[a]] -> [(Int, a)]
lengthEncodedPack [] = []    
lengthEncodedPack [[]] = []
lengthEncodedPack (x:xs) = (numOfElements x, head x): lengthEncodedPack xs

-- 1.11 (*) Modified run-length encoding.
--   Modify the result of problem 1.10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.
--    Example:
--    ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
--    X = [[4,a],b,[2,c],[2,a],d,[4,e]]
-- As Haskell does not support lists with multiple types, I am going to skip this problem for now.

-- 1.12 (**) Decode a run-length encoded list.
--    Given a run-length code list generated as specified in problem 1.10. Construct its uncompressed version.
decodeLengthEncodedList :: [(Int, a)] -> [[a]]
decodeLengthEncodedList = map' replicate'

map' f [] = []
map' f (x:xs) = f x : map' f xs

replicate' :: (Int, a) -> [a]
replicate' (0, _) = []
replicate' (n, x) = x : replicate' (n-1, x) 

-- TODO
-- 1.13 (**) Run-length encoding of a list (direct solution).
--    Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 1.09, but only count them. As in problem 1.11, simplify the result list by replacing the singleton terms [1,X] by X.
--    Example:
--    ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
--    X = [[4,a],b,[2,c],[2,a],d,[4,e]]

-- 1.14 (*) Duplicate the elements of a list.
--    Example:
--    ?- dupli([a,b,c,c,d],X).
--    X = [a,a,b,b,c,c,c,c,d,d]
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

-- 1.15 (**) Duplicate the elements of a list a given number of times.
--    Example:
--    ?- dupli([a,b,c],3,X).
--    X = [a,a,a,b,b,b,c,c,c]
--    What are the results of the goal:
--    ?- dupli(X,3,Y).
replicateFor :: [a] -> Int -> [a]
replicateFor [] _ = []
replicateFor _ 0 = []
replicateFor xs 1 = xs
replicateFor (x:xs) n = x : replicateFor [x] (n-1) ++ replicateFor xs n

-- 1.16 (**) Drop every N'th element from a list.
--    Example:
--    ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
--    X = [a,b,d,e,g,h,k]
dropNth :: [a] -> Int -> [a]
dropNth xs n = dropNth' xs n
    where 
        dropNth' [] _ = []
        dropNth' (x:xs) 1 = dropNth' xs n
        dropNth' (x:xs) n = x : dropNth' xs (n-1) 

-- 1.17 (*) Split a list into two parts; the length of the first part is given.
--    Do not use any predefined predicates.
--    Example:
--    ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
--    L1 = [a,b,c]
--    L2 = [d,e,f,g,h,i,k]
splitList :: [a] -> Int -> ([a], [a])
splitList ys n = splitList' [] ys n
    where
        splitList' xs [] _ = (xs, [])
        splitList' xs ys 0 = (xs, ys)
        splitList' xs (y:ys) n = splitList' (xs ++ [y]) ys (n-1)
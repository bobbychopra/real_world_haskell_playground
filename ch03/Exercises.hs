
import Data.List

----
-- 1. Write the converse of fromList for the List type: a function that takes a 
--    List a and generates a [a]
data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList::List a -> [a]
fromList Nil = []
fromList (Cons x xs) = [x] ++ fromList(xs)
-- testing 
-- []      == fromList Nil
-- [1]     == fromList (Cons 1 Nil)
-- [1,2]   == fromList (Cons 1 (Cons 2 Nil))


----
-- 2. Define a tree type that has only one constructor, like our Java example. 
-- Instead of the Empty constructor, use the Maybe type to refer to a node's 
-- children. Below is previous defintion
-- data Tree a = Node a (Tree a) (Tree a)
--             | Empty
--             deriving (Show)
data MyTree a = Node a (Maybe (MyTree a)) (Maybe (MyTree a))
              deriving (Show)
-- testing
-- let l = Node 1 Nothing Nothing
-- let r = Node 5 Nothing Nothing
-- let n = Node 3 (Just l) (Just r)


----
-- 1. Write a function that computes the number of elements in a list. To test 
-- it, ensure that it gives the same answers as the standard length function.
-- 2. Add a type signature for your function to your source file. To test it, 
-- load the source file into ghci again.
-- length_of_list::Num a => [t] -> a --Alternate type signature
length_of_list::[t] -> Int
length_of_list[] = 0
length_of_list(x:xs) = 1 + length_of_list(xs)


----
-- 3. Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)
-- mean_of_list::(Fractional a, Foldable t ) => t a-> a
mean_of_list [] = 0
mean_of_list xs = sum_of_list / fromIntegral(list_length)
                where list_length = length xs
                      sum_of_list = sum xs
                      
----
-- 4. Turn a list into a palindrome, i.e. it should read the same both backwards
--  and forwards. For example, given the list [1,2,3], your function should 
-- return [1,2,3,3,2,1]
palindrome [] = []
palindrome xs = xs ++ reverse(xs)
-- in case, we can't use reverse
myreverse [] = []
myreverse (x:xs) = myreverse(xs) ++ [x]


----
-- 5.Write a function that determines whether its input list is a palindrome.
-- ispalindrome::[a] -> Bool 
ispalindrome [] = True
ispalindrome (x:[]) = True
ispalindrome (x:xs) = x == (head (reverse xs)) && ispalindrome rest
                    where rest = tail (reverse xs)
                    

----
-- 6. Create a function that sorts a list of lists based on the length of 
-- sublist. (You may want to look at the sortBy function from the Data.List 
-- module.)
sortlistoflists xxs = sortBy (\ x y -> compare (length x) (length y)) xxs


-- ----
-- -- 7. Define a function that joins a list of lists together using a separator value.
-- myintersperse::a -> [[a]] -> [a]
-- myintersperse a [] = []
-- myintersperse a ([]) = []
-- myintersperse a (xs:[]) = xs
-- myintersperse a (xs:ys:[]) = xs ++ a ++ ys 
-- myintersperse a (xs:ys:zs) = xs ++ a ++ ys ++ a ++ (myintersperse zs)
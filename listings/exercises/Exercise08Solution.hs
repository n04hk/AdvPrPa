-- Exercise 8 (Test Framework and Permutations)

-- general test framework
-- first parameter of function test should be removed
-- as soon as we have a set type as instance of class Eq
-- at our disposal

-- intended type:
--test :: (f -> (a -> b)) -> (b -> b -> Bool) -> f -> [(a, b)] -> Bool
-- most general type:
test :: (sut -> (a -> b)) -> (b -> c -> Bool) -> sut -> [(a, c)] -> Bool
-- Note:
--   the type of sut is completely unrestricted;
--   in particular, it need not be a function type
test cast eq sut tests =
  and [cast sut input `eq` expected | (input, expected) <- tests]

testV2 cast eq sut tests = all check tests
  where
    check (input, expected) =
      (cast sut) input `eq` expected

-- function setEq
-- setEq compares two sets represented by lists for equality
-- both lists are permitted to contain duplicates

setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = all (`elem` ys) xs && all (`elem` xs) ys

check1SetEq = setEq [1,2,3,2] [2,3,1,1]
check2SetEq = not $ setEq [1,2,3] [2,3,4]

test2 :: (b -> c -> Bool) -> (a1 -> a2 -> b) -> [((a1, a2), c)] -> Bool
test2 = test uncurry

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

test3 ::
  (b -> b -> Bool) -> (a1 -> a2 -> a3 -> b) -> [((a1, a2, a3), b)] -> Bool
test3 = testV2 uncurry3

-- function revApp
-- revApp rs xs puts the reverse of rs in front of xs

revApp :: [a] -> [a] -> [a]
revApp = flip (foldl (flip (:)))

checkRevApp = revApp [3,2,1] [4,5,6] == [1,2,3,4,5,6]

-- function insert
-- insert x ys n inserts x into list ys at position n
-- precondition: 0 <= n <= length ys

type Insert a = a -> [a] -> Int -> [a]

insertTests =
  [((99, [1,2,3], 0), [99,1,2,3]),
   ((99, [1,2,3], 1), [1,99,2,3]),
   ((99, [1,2,3], 2), [1,2,99,3]),
   ((99, [1,2,3], 3), [1,2,3,99])]

insertV1 :: Insert a
insertV1 x ys       0 = x : ys
insertV1 x (y : ys) n = y : insertV1 x ys (n-1)

insertV2 :: Insert a
insertV2 x ys n = ls ++ x : rs
  where
    (ls, rs) = splitAt n ys

insertV1Test = test3 (==) insertV1 insertTests
insertV2Test = test3 (==) insertV2 insertTests

insert :: Insert a
insert = insertV1

-- function inserts
-- inserts x ys inserts x at all possible positions in ys
-- the order of the insertions remains unspecified

type Inserts a = a -> [a] -> [[a]]

-- actually just one test
insertsTests =
  [((99, [1,2,3]), [[99,1,2,3],
                    [1,99,2,3],
                    [1,2,99,3],
                    [1,2,3,99]])]

insertsV1 :: Inserts a
insertsV1 x ys = map (insert x ys) [0 .. length ys]

insertsV2 :: Inserts a
insertsV2 x ys = h [] ys []
  where
    h rs yys@(y : ys) accu =
      h (y : rs) ys (revApp rs (x : yys) : accu)
    h rs [] accu = revApp rs [x] : accu

insertsV1Test = test uncurry setEq insertsV1 insertsTests
insertsV2Test = test uncurry setEq insertsV2 insertsTests

inserts :: Inserts a
inserts = insertsV1

-- function outsert
-- outsert xs n yields the element at position n and
-- list xs shortened by that element
-- precondition: 0 <= n < length xs

outsertTests =
  [(([1,2,3], 0), (1, [2,3])),
   (([1,2,3], 1), (2, [1,3])),
   (([1,2,3], 2), (3, [1,2]))]

outsert :: [a] -> Int -> (a, [a])
outsert (x : xs) 0 = (x, xs)
outsert (x : xs) n = (y, x : ys) where (y, ys) = outsert xs (n-1)

outsertTest = test uncurry (==) outsert outsertTests

-- function perms
-- perms xs yields the list of all permutations of xs
-- the order of the permutations remains unspecified
-- precondition: all elements in xs are pairwise distinct

permsTests =
  [([],      [[]]),
   ([1],     [[1]]),
   ([1,2],   [[1,2], [2,1]]),
   ([1,2,3], [[1,2,3], [1,3,2],
              [2,1,3], [2,3,1],
              [3,1,2], [3,2,1]])]

permsV1 :: [a] -> [[a]]
permsV1 [] = [[]]
permsV1 (x : xs) = concatMap (inserts x) (permsV1 xs)

permsV2 :: [a] -> [[a]]
permsV2 [] = [[]]
permsV2 (x : xs) = [insert x ps n | ps <- permsV2 xs,
                                    n <- [0 .. length xs]]

permsV2a :: [a] -> [[a]]
permsV2a [] = [[]]
permsV2a (x : xs) = [ls ++ x : rs | ps <- permsV2a xs,
                                    n <- [0 .. length xs],
                                    let (ls, rs) = splitAt n ps]

permsV3 :: [a] -> [[a]]
permsV3 [] = [[]]
permsV3 xs = concatMap h [0 .. length xs - 1]
  where
    h n = map (x:) (permsV3 (ls ++ rs))
      where
        (ls, x : rs) = splitAt n xs

permsV4 :: [a] -> [[a]]
permsV4 [] = [[]]
permsV4 xs = [x : ps | n <- [0 .. length xs - 1],
                       let (ls, x : rs) = splitAt n xs,
                       ps <- permsV4 (ls ++ rs)]

permsV5 :: [a] -> [[a]]
permsV5 [] = [[]]
permsV5 xs = [x : ps | n <- [0 .. length xs - 1],
                       let (x, ys) = outsert xs n,
                       ps <- permsV5 ys]

-- based on a solution by Daniel Krni
-- extremely compact, but requires an equality test
permsV6 :: Eq a => [a] -> [[a]]
permsV6 [] = [[]]
permsV6 xs = [x : ps | x <- xs,
                       ps <- permsV6 (filter (/=x) xs)]

-- original solution Daniel Krni
permsV7 :: Eq a => [a] -> [[a]]
permsV7 [] = [[]]
permsV7 xs = concat $ map (\x -> map (x:) (permsV7 $ filter (/=x) xs)) xs

permsTest =
  all (\perms -> test id setEq perms permsTests)
    [permsV1, permsV2, permsV2a, permsV3, permsV4, permsV5, permsV6, permsV7]

perms :: [a] -> [[a]]
perms = permsV1

-- an application: concurrent programming
-- determine the results of all possible schedulings
-- of some atomic computations

atomA1 x = x + 1
atomA2 x = 2 * x
atomA3 x = x * x

atomB1 x = 2*x + 1
atomB2 x = 3*x + 2
atomB3 x = 4*x + 3

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

allResultsA, allResultsB :: Int -> [Int]
allResultsA x = map (($x).compose) (perms [atomA1, atomA2, atomA3])
allResultsB x = map (($x).compose) (perms [atomB1, atomB2, atomB3])

singleResults :: Int -> [Int]
singleResults x = map ($x) [atomA1, atomA2, atomA3]

t01 :: [Int -> Int]
t01 = [atomA1, atomA2, atomA3]

t02 :: Int -> Int
t02 = compose [atomA1, atomA2, atomA3]

t03 :: [[Int -> Int]]
t03 = perms [atomA1, atomA2, atomA3]

t04 :: [Int -> Int]
t04 = map compose (perms [atomA1, atomA2, atomA3])


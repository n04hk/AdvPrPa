-- Exercise 7 (Higher-Order Functions)
import Prelude hiding ((.), ($))

-- function composition
testComposition = (head . tail) [1,2,3] == head (tail [1,2,3])

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

-- function composition reverse
testCompositionRev = (tail .> head) [1,2,3] == head (tail [1,2,3])

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

-- function application
testFunApp = (head $ [1,2,3]) == head [1,2,3]

($) :: (a -> b) -> (a -> b)
f $ x = f x

-- function application reverse
testFunAppRev = ([1,2,3] $> head) == head [1,2,3]

($>) :: a -> (a -> b) -> b
($>) = flip ($)

infixl 9 .>
infixl 0 $>

test01 = (5 $> (+2) .> (*3) .> (+4)) == ((+4) . (*3) . (+2) $ 5)


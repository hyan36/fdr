*/Q2 States and test

> import Data.List
> import Data.Ord

> data State = Pair Int Int | Triple Int Int Int
>     deriving (Eq, Show)

> data Test = TPair (Int,Int) (Int,Int) | TTrip (Int,Int,Int) (Int,Int,Int)
>     deriving (Eq,Show)

The valid function only returns true when matching following condition

* Pair can only be tested by TPair and Triple can only be tested by TTrip

* validTest: a valid test case requires the number of the coins on the right
side matches the coins the left. We learn nothing if we weigh 3 coins against 4.

* validSample: You can't take more coins than the pile can offer. For example,
assume we have a pile of 5 coins. We've taken 3 coins to the left hand side of
the scales. There are 2 coins remaining in the pile. We can't take more than 2
coins for the right side of the scale.

> valid:: State -> Test -> Bool
> valid (Pair x y) (TPair (a,b) (c,d)) = validTest && validSample
>     where
>         validTest   = (a + b) == (c + d)
>         validSample = (a + c) <= x && (b + d) <= y
> valid (Triple x y z) (TTrip (a,b,c) (d,e,f)) = validTest && validSample
>     where
>         validTest   = (a + b + c) == (d + e + f)
>         validSample = (a + d) <= x && (b + e) <= y && (c + f) <= z
> valid _ _ = False

In Triple, when the result is lighter, the fake coin will be 100% in the lighter
stack if the result is heavier it will 100% be heavier pile. Thus we can move
all coins of the opposite pile to pile G.

> outcomes::State -> Test -> [State]
> outcomes state test
>     | valid state test = [(lighter state test),(balanced state test),(heavier state test)]
>     | otherwise = []
>     where
>         balanced (Pair x y) (TPair (a,b) (c,d))         = Pair (x - a - c) (y + a + c)
>         balanced (Triple x y z) (TTrip (a,b,c) (d,e,f)) = Triple (x - a - d) (y - b - e) (z + a + d + b + e)
>         lighter (Pair x y) (TPair (a,b) (c,d))          = Triple a c (x - a - c)
>         lighter (Triple x y z) (TTrip (a,b,c) (d,e,f))  = Triple (a + d) 0 (x + y + z - a - d)
>         heavier (Pair x y) (TPair (a,b) (c,d))          = Triple c a (x - a - c)
>         heavier (Triple x y z) (TTrip (a,b,c) (d,e,f))  = Triple 0 (b + e) (x + y + z - b - e)

> sensible :: State -> Test -> Bool
> sensible state test = False

> weighings::State -> [Test]
> weighings state = []

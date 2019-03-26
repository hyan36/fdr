1. States and test

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

1.valid

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

2. outcomes

> outcomes::State -> Test -> [State]
> outcomes state test
>     | valid state test = [(lighter state test),(balanced state test),(heavier state test)]
>     | otherwise = []
>     where
>         balanced (Pair x y) (TPair (a,b) (c,d))         = Pair (x - a - c) (y + a + c)
>         balanced (Triple x y z) (TTrip (a,b,c) (d,e,f)) = Triple (x - a - d) (y - b - e) (z + a + d + b + e)
>         lighter (Pair x y) (TPair (a,b) (c,d))          = Triple a c (x - a - c)
>         lighter (Triple x y z) (TTrip (a,b,c) (d,e,f))  = Triple a e (x + y + z - a - e)
>         heavier (Pair x y) (TPair (a,b) (c,d))          = Triple c a (x - a - c)
>         heavier (Triple x y z) (TTrip (a,b,c) (d,e,f))  = Triple d b (x + y + z - b - d)

3. weighings - Pair

For TPair (a,b) (c,d), a sensible test must meet following requirement

* a + b == c + d : in our implementation, a + b == (a+b) + 0, thus there is no need to validate that
* a + b > 0 : we will use as it is
* b * d == 0 : in our  implementation,  d = 0 thus b * d will always equals 0, thus there is no need to validate that
* a + c <= u : since c = a + b thus we will translate this condition to 2 * a + b <= u
* b + d <= g : since d = 0 thus, b + d = b. In our implementation b <- [0..g], it will always be smaller than g, thus there is no need to validate that
* (a,b) <= (c,d) : since c = a + b, d = 0. We translate the equation to (a,b) <= (a+b,0)

> weighings::State -> [Test]
> weighings (Pair u g) = [TPair (a,b) (a + b, 0) | a <- [0..u], b <- [0..g],
>                                                  a + b > 0,
>                                                  2 * a + b <= u,
>                                                  (a,b) <= (a + b, 0)]

5. weigh - Triple

> weighings (Triple l h g) = [ TTrip (a,b,c) (d,e,f) | k <- [1..((l + h + g) `div` 2)],
>                                                      (a,b,c) <- choices k (l,h,g),
>                                                      (d,e,f) <- choices k (l,h,g),
>                                                      a + b + c == d + e + f,
>                                                      a + b + c > 0,
>                                                      c * f == 0,
>                                                      a + d <= l,
>                                                      b + e <= h,
>                                                      c + f <= g,
>                                                      (a,b,c) <= (d,e,f)]

4. choice

> choices::Int -> (Int,Int,Int)->[(Int,Int,Int)]
> choices k (l,h,g) = [(i,j,k-i-j) | i <- [0..l], j <-[0..h], k-i-j >= 0, k-i-j <= g]

6. define ord state

> instance Ord State where
>     compare (Triple _ _ _) (Pair _ _)        = compare 0 1
>     compare (Pair _ _) (Triple _ _ _)        = compare 1 0
>     compare (Pair u g) (Pair u' g')          = compare g' g
>     compare (Triple l h g) (Triple l' h' g') = compare g' g

7.

> productive::State -> Test -> Bool
> productive s t
>    | length results > 0 = foldr (\x y -> x && y) True [ s' < s | s'<-results]
>    | otherwise = False
>    where
>        results = outcomes s t


8.

> tests::State -> [Test]
> tests s = filter (\t -> (valid s t) && (productive s t)) (weighings s)

9.

> data Tree = Stop State | Node Test [Tree]
>    deriving Show

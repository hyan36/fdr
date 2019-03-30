1 Introduction

> import Data.List
> import Data.Ord

The puzzle require the application to find the coin with different weight. In
this simulation, we are going to use a list to represent a pile of coins, the
element inside of the List is the weight of the coins. For example, given a list
of coin [1,1,2,1,1], the coin weighs 2 is the fake coin.

The following method will weigh 3 coins and find the fake coin.

> weigh:: Ord a => [a] -> [a]
> weigh (a:b:xs)
>    | a == b && a == c = []
>    | a == b && a /= c = [c]
>    | a /= b && a == c = [b]
>    | otherwise = [a]
>    where
>        c = head xs
> weigh xs = []

This method will round the original pile of coins if it can not mod by 3. The
method will add n element to the bottom of the list. Given we only have one fake
coin in the pile, regardless the coin is at top or bottom of the pile, the
modification will not affect the final result. It just add one more round of
test for remaining coins.

> prefill::Int -> [a] -> [a]
> prefill n xs = xs ++ (take (if size == n then 0 else size) xs)
>     where
>         size = n - ((length xs) `mod` n)

Now that we have a list of test cases. We just need to weigh every 3 coins until
find out the fake coin.

The complexity of the solution is O(n/3)

> findFake:: Ord a => [a] -> [a]
> findFake [] = []
> findFake xs
>     | length rounded >= 3 = test ++ next
>     where
>         rounded   = prefill 3 xs
>         test      = weigh (take 3 rounded)
>         next      = if length test == 0 then findFake (drop 3 rounded) else []

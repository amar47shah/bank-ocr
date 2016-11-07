module Split (atSplits, splitEvery) where

atSplit :: (Functor f, Monoid (f [a])) => (a -> f a) -> ([a], [a]) -> f [a]
atSplit f (a, x:b) = (a ++) . (:b) <$> f x
atSplit _  _       = mempty

atSplits :: (Functor f, Monoid (f [a])) => (a -> f a) -> [a] -> [f [a]]
atSplits f = map (atSplit f) . splits

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs

splits :: [a] -> [([a], [a])]
splits = take . succ . length <*> zipWith splitAt [0..] . repeat

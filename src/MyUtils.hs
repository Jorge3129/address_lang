module MyUtils where

data Infinitable = NegInf | Reg Int | PosInf deriving (Show, Eq, Ord)

scanUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
scanUntil p f initial = takeWhile (not . p) $ iterate f initial

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x
  | p x = return x
  | otherwise = f x >>= untilM p f
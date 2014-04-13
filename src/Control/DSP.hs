module Control.DSP where

-- | Generally, an impulse response should take an a, and its neighbours, and
-- | return an a.
-- |
-- | Parameters:
-- | previous - stuff before. A container of Maybe a.
-- | current  - the sample right now, of type a.
-- | further  - stuff after.  A container of Maybe a.
-- |
-- | Both "previous" and "further" use the same container.
-- | The first element of each container should be the one nearest to the
-- | current item (therefore, @previous@ is reversed in time).
type IR t a = t (Maybe a) -> a -> t (Maybe a) -> a

-- | For selecting the neighbours of a sample.
type Taking t a = [a] -> t (Maybe a)

convolve  :: IR t a -> Taking t a        -> [a] -> [a]
convolve  ir take = convolve' ir take []

convolve' :: IR t a -> Taking t a -> [a] -> [a] -> [a]
convolve' _  _    _        []                = []
convolve' ir take previous (current:further) = currentProcessed:furtherProcessed
  where
    currentProcessed = ir previousRelevant current furtherRelevant
    furtherProcessed = convolve' ir take (current:previous) further
    previousRelevant = take previous
    furtherRelevant  = take further

distribute :: (a -> Bool) -> [a] -> ([a], [a])
distribute _    []     = ([], [])
distribute pred (x:xs) =
  case pred x of
    True  -> (x:good,   bad)
    False -> (  good, x:bad)
  where
    (good, bad) = distribute pred xs

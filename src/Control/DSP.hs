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

-- | Generalized convolution.
-- | Also a generalized map.
-- | And a special-cased scan which forgets its history after n steps.
-- | (generalizes both scanl and scanr at the same time)
-- |
-- | You could generalize the type to any Traversable, not just lists.
-- | You could probably make special case versions for trees, graphs,
-- | or other data structures.
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

-- | Distributes a list into two lists, depending on some binary property of its
-- | elements. No elements are lost, each element goes into one list exactly.
-- |
-- | You could also generalize distribute for any Eq or Ord or Enum, making it
-- | distribute to a Map rather than a tuple.
distribute :: (a -> Bool) -> [a] -> ([a], [a])
distribute _    []     = ([], [])
distribute pred (x:xs) =
  case pred x of
    True  -> (x:good,   bad)
    False -> (  good, x:bad)
  where
    (good, bad) = distribute pred xs


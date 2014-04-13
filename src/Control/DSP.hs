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

-- todo: add a high pass filter, which takes a sequence and outputs Nothing
-- when nothing has changed, or Just x if something has changed. To check
-- if something has changed, you can have a distance function, which will
-- take the previous sample and the current sample and compare. If the delta
-- is too high, then it will trigger. If it is not, then it will not trigger
-- and will output Nothing. When it triggers it outputs Just with the value
-- of the current sample. We could also use a binary function which takes
-- two samples and outputs Bool. This will likely be the easiest to use and
-- to understand.
--
-- todo: add another high pass filter, which is more continuous, but only
-- works on stuff that's like Float. Add a typeclass which defines that
-- operations similar to the ones on Float can be performed on the type.
--
-- todo: add a low pass filter, similar to the above, using the float-like
-- typeclass. It will hold state (as the hpf should as well) and will
-- move the state halfway towards the signal at every step (or move another
-- fraction of the way, depending on what cutoff you set!).
--
-- todo: using the Float-like typeclass, add an oversampling filter which will
-- let you perform operations on signals at a higher resolution than the signal
-- itself.
--
-- todo: add a comparator and/or a non-linearity. You can use the comparator
-- and "continuous" (proper) high pass filter to implement something similar
-- to the crude change-detecting high-pass filter from before.
--
-- todo: think about whether heterodyning can be useful for anything at all?
--
-- what about pitch shifting? Can this be useful?
--
-- What about the Z transform?
--
-- What about iir filters?

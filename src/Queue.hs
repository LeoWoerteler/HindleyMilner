
module Queue (
  Queue,
  empty, 
  enqueue,
  enqueueAll,
  dequeue,
  fromList,
  toList
) where

data Queue a = Queue [a] [a]

instance Show a => Show (Queue a) where
  show = showString "Queue" . show . toList

empty :: Queue a
empty = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue [] _)  = Queue [x] []
enqueue x (Queue hs ts) = Queue hs  (x:ts)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue []     _ ) = Nothing
dequeue (Queue (x:xs) ys) = Just (x, queue)
  where
    queue = case xs of
      [] -> Queue (reverse ys) []
      _  -> Queue xs           ys

fromList :: [a] -> Queue a
fromList xs = Queue xs []

toList :: Queue a -> [a]
toList (Queue xs ys) = xs ++ reverse ys

enqueueAll :: [a] -> Queue a -> Queue a
enqueueAll xs (Queue [] _)  = Queue xs []
enqueueAll xs (Queue hs ts) = Queue hs $ foldl (flip (:)) ts xs


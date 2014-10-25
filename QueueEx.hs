module QueueEx where

import           Control.Applicative
import           Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue           as Q
import           Data.Maybe


-- |Shift a queue to the left, such as:
-- [1, 2, 3] -> [2, 3, 1]
shiftQueueLeft :: BankersDequeue a -> BankersDequeue a
shiftQueueLeft  = (\(b, nq) -> Q.pushBack nq (fromJust b)) <$> Q.popFront


-- |Shift a queue to the right, such as:
-- [1, 2, 3] -> [3, 1, 2]
shiftQueueRight :: BankersDequeue a -> BankersDequeue a
shiftQueueRight = (\(b, nq) -> Q.pushFront nq (fromJust b)) <$> Q.popBack


-- |Convert a Queue back to a list.
queueToList :: BankersDequeue a -> [a]
queueToList q = Q.takeFront (Q.length q) q


-- |Unsafe version of Q.first.
uQfirst :: BankersDequeue a -> a
uQfirst = fromJust . Q.first
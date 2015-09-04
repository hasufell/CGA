module QueueEx where

import           Control.Applicative
import           Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue           as Q
import           Data.Maybe


-- |Shift a queue to the left, such as:
-- [1, 2, 3] -> [2, 3, 1]
shiftQueueLeft :: BankersDequeue a -> BankersDequeue a
shiftQueueLeft  = (\(Just (b, nq)) -> Q.pushBack nq b) <$> Q.popFront


-- |Shift a queue to the right, such as:
-- [1, 2, 3] -> [3, 1, 2]
shiftQueueRight :: BankersDequeue a -> BankersDequeue a
shiftQueueRight = (\(Just (b, nq)) -> Q.pushFront nq b) <$> Q.popBack


-- |Convert a Queue back to a list.
queueToList :: BankersDequeue a -> [a]
queueToList q = Q.takeFront (Q.length q) q

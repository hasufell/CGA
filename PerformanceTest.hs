import Algorithms.ConvexHull.GrahamScan
import Parser.Meshparser
import System.Environment
import System.FileSystem.FileExt
import MyPrelude


main = do
  a <- getArgs
  mesh <- readFile (head a)
  print . grahamCH . meshToArr $ mesh


{- real	0m0.031s -}
{- user	0m0.029s -}
{- sys	0m0.001s -}


{- real	0m0.027s -}
{- user	0m0.026s -}
{- sys	0m0.000s -}

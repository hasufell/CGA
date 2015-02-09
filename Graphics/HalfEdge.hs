{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This module provides methods to build a cyclic half-edge data structure
-- from an already parsed obj mesh file. As such, it depends on details
-- of the parsed data.
--
-- In particular, 'indirectHeFaces', 'indirectHeVerts' and 'indirectToDirect'
-- assume specific structure of some input lists. Check their respective
-- documentation.
--
-- As the data structure has a lot of cross-references and the knots are
-- not really known at compile-time, we have to use helper data structures
-- such as arrays, lists or vectors under the hood and tie the knots through
-- index lookups.
--
-- For an explanation of the abstract concept of the half-edge data structure,
-- check <http://www.flipcode.com/archives/The_Half-Edge_Data_Structure.shtml>
module Graphics.HalfEdge (
    HeVert(..)
  , HeFace(..)
  , HeEdge(..)
  , buildHeEdge
  , buildHeEdgeFromStr
) where

import Algebra.Vector
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Data.Functor
import Parser.Meshparser
import Safe


-- |The vertex data structure for the half-edge.
data HeVert a = HeVert {
    vcoord  :: a        -- the coordinates of the vertex
  , emedge  :: HeEdge a -- one of the half-edges emanating from the vertex

}

-- |The face data structure for the half-edge.
data HeFace a = HeFace {
  bordedge :: HeEdge a -- one of the half-edges bordering the face
}

-- |The actual half-edge data structure.
data HeEdge a = HeEdge {
    startvert :: HeVert a         -- start-vertex of the half-edge
  , oppedge   :: Maybe (HeEdge a) -- oppositely oriented adjacent half-edge
  , edgeface  :: HeFace a         -- face the half-edge borders
  , nextedge  :: HeEdge a         -- next half-edge around the face
}

-- This is a helper data structure of half-edge edges
-- for tying the knots in 'indirectToDirect'.
data IndirectHeEdge = IndirectHeEdge {
    edgeindex  :: Int  -- edge index
  , svindex    :: Int  -- index of start-vertice
  , nvindex    :: Int  -- index of next-vertice
  , indexf     :: Int  -- index of face
  , offsetedge :: Int  -- offset to get the next edge
}

-- This is a helper data structure of half-edge vertices
-- for tying the knots in 'indirectToDirect'.
data IndirectHeVert = IndirectHeVert {
    emedgeindex  :: Int    -- emanating edge index (starts at 1)
  , edgelist     :: [Int]  -- index of edge that points to this vertice
}

-- This is a helper data structure of half-edge faces
-- for tying the knots in 'indirectToDirect'.
data IndirectHeFace =
  IndirectHeFace (Int, [Int]) -- (faceIndex, [verticeindex])


-- |Construct the indirect data structure for half-edge faces.
-- This function assumes that the input faces are parsed exactly like so:
--
-- @
-- f 1 3 4 5
-- f 4 6 1 3
-- @
--
-- becomes
--
-- > [[1,3,4,5],[4,6,1,3]]
indirectHeFaces :: [[Int]]          -- ^ list of faces with their respective
                                    --   list of vertice-indices
                -> [IndirectHeFace]
indirectHeFaces = fmap IndirectHeFace . zip [0..]


-- |Construct the indirect data structure for half-edge edges.
indirectHeEdges :: [IndirectHeFace] -> [IndirectHeEdge]
indirectHeEdges = concat . fmap indirectHeEdge
  where
    indirectHeEdge :: IndirectHeFace -> [IndirectHeEdge]
    indirectHeEdge   (IndirectHeFace (_, []))  = []
    indirectHeEdge p@(IndirectHeFace (_, pv@(v:_))) = go p 0
      where
        go (IndirectHeFace (_, [])) _
          = []
        -- connect last to first element
        go (IndirectHeFace (fi, [vlast])) ei
          = [IndirectHeEdge ei vlast v fi (negate $ length pv - 1)]
        -- regular non-last element
        go (IndirectHeFace (fi, vfirst:vnext:vrest)) ei
          = (:) (IndirectHeEdge ei vfirst vnext fi 1)
                (go (IndirectHeFace (fi, vnext:vrest)) (ei + 1))


-- |Construct the indirect data structure for half-edge vertices.
-- It is assumed that the list of points is indexed in order of their
-- appearance in the obj mesh file.
indirectHeVerts :: [a]                      -- ^ list of points
                -> [IndirectHeEdge]         -- ^ list of indirect edges
                -> Array Int IndirectHeVert -- ^ output list, starts at index 1
indirectHeVerts pts hes'
  = runSTArray $ do
    arr <- newArray (1, length pts) (IndirectHeVert 0 [])
      :: ST s (STArray s Int IndirectHeVert)
    -- build the array
    let go [] _ = return ()
        go (IndirectHeEdge _ _ nv _ offset:hes) i
          = do
            (IndirectHeVert _ xs) <- readArray arr nv
            writeArray arr nv (IndirectHeVert (i + offset) (i:xs))
            go hes (i + 1)
    go hes' 0
    return arr


-- |Tie the knots!
-- It is assumed that the list of points is indexed in order of their
-- appearance in the obj mesh file.
-- TODO: make this function safe.
indirectToDirect :: [a]                      -- ^ list of points
                 -> [IndirectHeEdge]
                 -> [IndirectHeFace]
                 -> Array Int IndirectHeVert -- ^ assumed to start at index 1
                 -> HeEdge a
indirectToDirect pts pe@(e:_) fs vertarr
  = thisEdge e
  where
    thisEdge (IndirectHeEdge ei sv _ fi off)
      = HeEdge (thisVert (vertarr ! sv) sv)
               (getOppEdge sv fi)
               (thisFace (fs !! fi))
               (thisEdge . (!!) pe $ (ei + off))
    thisFace (IndirectHeFace (_, vi:_))
      = HeFace (thisEdge (pe !! vi))
    thisVert (IndirectHeVert eedg _) coordi
      = HeVert (pts !! (coordi - 1))
               (thisEdge (pe !! (eedg - 1)))
    getOppEdge sv fi
      = (\x -> thisEdge (pe !! x))
        <$>
        (headMay
          . filter (\x -> (/=) fi . indexf $ (pe !! x))
          . edgelist
          $ (vertarr ! sv))


-- |Build the half-edge data structure from a list of points
-- and from a list of faces.
-- The points are assumed to have been parsed in order of their appearance
-- in the .obj mesh file, so that the indices match.
-- The faces are assumed to have been parsed in order of their appearance
-- in the .obj mesh file as follows:
--
-- @
-- f 1 3 4 5
-- f 4 6 1 3
-- @
--
-- becomes
--
-- > [[1,3,4,5],[4,6,1,3]]
buildHeEdge :: [a] -> [[Int]] -> Maybe (HeEdge a)
buildHeEdge [] _   = Nothing
buildHeEdge _ []   = Nothing
buildHeEdge pts fs
  = let faces' = indirectHeFaces fs
        edges' = indirectHeEdges faces'
        verts' = indirectHeVerts pts edges'
    in Just $ indirectToDirect pts edges' faces' verts'


-- |Build the HeEdge data structure from the .obj mesh file contents.
buildHeEdgeFromStr :: B.ByteString -- ^ contents of an .obj mesh file
                   -> HeEdge PT
buildHeEdgeFromStr bmesh =
  let pts    = meshVertices bmesh
      faces' = indirectHeFaces . meshFaces $ bmesh
      edges  = indirectHeEdges faces'
      verts  = indirectHeVerts pts edges
  in  indirectToDirect pts edges faces' verts


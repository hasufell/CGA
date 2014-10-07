{-# OPTIONS_HADDOCK ignore-exports #-}

module Class.Defaults where


-- |Used to create a common interface for default settings of data types.
class Def a where
    def :: a

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain (
  arbitraryGame
) where

import qualified Interface as I
import qualified GHC.Generics as Gen

data Moves = FirstMove {
    coord :: (I.Column, I.Row)
  , next :: Maybe Moves
  } | NextMove {
    coord :: (I.Column, I.Row)
  , result :: I.MoveResult
  , next :: Maybe Moves
  } deriving Gen.Generic

toNestedMoves :: Moves -> I.NestedMoves
toNestedMoves = toNestedMoves'
  where
    toNestedMoves' :: Moves -> I.NestedMoves -> I.NestedMoves
    toNestedMoves' (FirstMove c Nothing) acc -> 

arbitraryGame :: I.GameVariation -> IO I.NestedMoves
arbitraryGame _ = return $ toNestedMoves $ NextMove I.A I.R2 I.Miss $ FirstMove I.A I.R1
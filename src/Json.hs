{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Data.Aeson.Types

import Interface

instance ToJSON MoveResult where
  toJSON Miss = String "MISS"
  toJSON Hit = String "HIT"

instance ToJSON Moves
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Bencoding where

import Data.BEncode

import Interface

instance BEncode MoveResult where
  toBEncode Miss = BString "MISS"
  toBEncode Hit = BString "HIT"
  fromBEncode _ = Left "Not Implemented"

instance BEncode Moves where
  toBEncode Moves {..} = toDict $
      "coord" .=! map toBEncode coord .:
      "prev" .=? prev .:
      "result" .=? result .:
      endDict
  fromBEncode _ = Left "Not Implemented"
  
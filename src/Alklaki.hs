{-# LANGUAGE DeriveGeneric #-}

module Alklaki
(
    alklakiKontrolon
) where

import              Data.Aeson                      (Value, ToJSON, FromJSON, decode, encode)
import              GHC.Generics                    (Generic)
import qualified    Data.ByteString.Lazy    as BL

alklakiKontrolon :: (String, String, String) -> IO ()
alklakiKontrolon (servo, salutnomo, pasvorto) = 
    return ()
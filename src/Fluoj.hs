{-# LANGUAGE DeriveGeneric #-}

module Fluoj
(
    Fluoj
    , Fluo
    , kreiFluon
    , kreiFluojn
    , malkodiFluojn
    , kodiFluojn
    , aldoni
    , novajnRetpagxojn
) where

import              Data.Aeson                      (Value, ToJSON, FromJSON, decode, encode)
import              GHC.Generics                    (Generic)
import qualified    Data.ByteString.Lazy    as BL
import              Data.List                       ((\\))
import              Fluo

data Fluo = Fluo {
    _ligilo :: String
    , _retpagxojn :: [String]
} deriving (Generic, Eq)

instance ToJSON Fluo
instance FromJSON Fluo

data Fluoj = Fluoj {
    _fluoj :: [Fluo]
} deriving (Generic, Eq)

instance ToJSON Fluoj
instance FromJSON Fluoj

kreiFluon :: String -> [String] -> Fluo
kreiFluon = Fluo

kreiFluojn :: [Fluo] -> Fluoj
kreiFluojn = Fluoj

malkodiFluojn :: BL.ByteString -> Fluoj
malkodiFluojn bs = 
    case decode bs of
        Just f -> f
        Nothing -> kreiFluojn []

kodiFluojn :: Fluoj -> BL.ByteString
kodiFluojn = encode

aldoni :: Fluoj -> Fluoj -> (Fluoj, Int)
aldoni malnovajFluoj novajFluoj = 
    let
        mnf = map _ligilo (_fluoj malnovajFluoj)
        nf = map (\n -> kreiFluon (_ligilo n) []) $ 
                filter (\n ->   length (_retpagxojn n) > 0 
                                && (not $ elem (_ligilo n) mnf)) 
                        (_fluoj novajFluoj)
    in
        (kreiFluojn $ (_fluoj malnovajFluoj) ++ nf, length nf)

relegiFluojn :: Fluoj -> IO (Fluoj)
relegiFluojn f = do 
    fluoj <- mapM (\n -> do 
                    retpagxojn <- legiFluon n False
                    return $ kreiFluon n retpagxojn) (map _ligilo $ _fluoj f)
    return $ kreiFluojn fluoj

diferencoFluo :: Fluo -> Fluo -> [String]
diferencoFluo f1 f2 = 
    if _ligilo f1 == _ligilo f2 then
        _retpagxojn f2 \\ _retpagxojn f1
    else
        []

diferencoFluoj :: Fluoj -> Fluoj -> [String]
diferencoFluoj f1 f2 =  
    concat $ map (\n -> diferencoFluo (fst n) (snd n)) $ zip (_fluoj f1) (_fluoj f2)


novajnRetpagxojn :: Fluoj -> IO ([String], Fluoj)
novajnRetpagxojn f = do
    f2 <- relegiFluojn f
    return $ (diferencoFluoj f f2, f2)

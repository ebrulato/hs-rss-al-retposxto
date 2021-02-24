{-# LANGUAGE OverloadedStrings #-}

module Fluo 
(
    legiFluon
) where

import qualified Data.Text.Lazy                as TL    (Text)
import qualified Data.Text                     as T     (unpack)
import           Network.Wreq
import           Control.Lens
import           Data.Text.Lazy.Encoding                (decodeUtf8)
import           Text.HTML.Parser


import           Helpi


ekstrakti :: TL.Text -> [String]
ekstrakti teksto = 
    filtri False False $ parseTokensLazy teksto


filtri :: Bool -> Bool -> [Token] -> [String]
filtri item link [] = []
filtri item link (x:xs) = 
    case (x, item, link) of 
        (TagOpen "item" atributoj, _, _) -> filtri True link xs
        (TagOpen "link" atributoj, True, _) -> filtri True True xs
        (ContentText teksto, True, True) -> (T.unpack teksto) : (filtri False False xs) 
        _ -> filtri item link xs 

legiFluon :: String -> Bool -> IO [String]
legiFluon fluo babilu = do
    case ekstraktiServon fluo of
        Just servo -> do
            -- TODO try 
            r <- get fluo
            if r ^. responseStatus . statusCode == 200 then do
                retpagxoj <- return $ ekstrakti $ decodeUtf8 $  r ^. responseBody
                seSkribu babilu $ "La fluo \""++ fluo ++"\" havas "++ (show $ length retpagxoj) ++ " retpago(j)n."
                return retpagxoj
            else do
                seSkribu babilu $ "La fluo ne legeblas: responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)
                return []
        Nothing -> do
            seSkribu babilu $ "La fluoligilo ne havas ĝustan servon : " ++ fluo
            return []

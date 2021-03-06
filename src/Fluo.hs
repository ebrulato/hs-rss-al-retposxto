{-# LANGUAGE OverloadedStrings #-}

module Fluo 
(
    legiFluon
) where

import qualified Data.Text.Lazy                as TL    (Text)
import qualified Data.Text                     as T     (unpack)
import qualified Data.List                     as L     (isPrefixOf)
import           Network.Wreq
import           Control.Lens
import           Data.Text.Lazy.Encoding                (decodeUtf8)
import           Text.HTML.Parser


import           Helpi


ekstrakti :: String -> TL.Text -> [String]
ekstrakti fluo teksto = 
    let  
        rezulto = filtriRSS2 False False $ parseTokensLazy teksto
    in
        if null rezulto then
            filtriMedium fluo $ parseTokensLazy teksto
        else
            rezulto

filtriRSS2 :: Bool -> Bool -> [Token] -> [String]
filtriRSS2 item link [] = []
filtriRSS2 item link (x:xs) = 
    case (x, item, link) of 
        (TagOpen "item" atributoj, _, _) -> filtriRSS2 True link xs
        (TagOpen "link" atributoj, True, _) -> filtriRSS2 True True xs
        (ContentText teksto, True, True) -> (T.unpack teksto) : (filtriRSS2 False False xs) 
        _ -> filtriRSS2 item link xs 


filtriMedium :: String -> [Token] -> [String]
filtriMedium fluo [] = []
filtriMedium fluo (x:xs) = 
    case x of 
        (TagOpen "a" atributoj) ->
            let
                href = sercxiAtributon "href" atributoj
            in 
            if (href == "") || (href == fluo) || (not $ L.isPrefixOf fluo href) then
                filtriMedium fluo xs
            else 
                href : (filtriMedium fluo xs)
        _ -> filtriMedium fluo xs 
    where
        sercxiAtributon nomo as = concat $ map (\(Attr atributaNomo valo) -> if T.unpack atributaNomo == nomo then (T.unpack valo) else "") as


legiFluon :: String -> Bool -> IO [String]
legiFluon fluo babilu = do
    case ekstraktiServon fluo of
        Just servo -> do
            -- TODO try 
            r <- get fluo
            if r ^. responseStatus . statusCode == 200 then do
                retpagxoj <- return $ ekstrakti fluo $ decodeUtf8 $  r ^. responseBody
                seSkribu babilu $ "La fluo \""++ fluo ++"\" havas "++ (show $ length retpagxoj) ++ " retpago(j)n."
                return retpagxoj
            else do
                seSkribu babilu $ "La fluo ne legeblas: responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)
                return []
        Nothing -> do
            seSkribu babilu $ "La fluoligilo ne havas ĝustan servon : " ++ fluo
            return []

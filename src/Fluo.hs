{-# LANGUAGE OverloadedStrings #-}

module Fluo
(
    legiFluon
) where

import           Control.Lens
import qualified Data.List               as L (isPrefixOf)
import qualified Data.Text               as T (unpack)
import qualified Data.Text.Lazy          as TL (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.Wreq
import           Text.HTML.Parser


import           Helpi


ekstrakti :: String -> String -> TL.Text -> [String]
ekstrakti servo fluo teksto =
    let
        tokens = parseTokensLazy teksto
        rezultoRSS2 = filtriRSS2 False False tokens
    in
        if null rezultoRSS2 then
            let
              rezultoMedium = filtriMedium servo False tokens
            in
              if null rezultoMedium then
                filtriBlogon servo fluo tokens
              else
                rezultoMedium
        else
            rezultoRSS2

filtriRSS2 :: Bool -> Bool -> [Token] -> [String]
filtriRSS2 item link [] = []
filtriRSS2 item link (x:xs) =
    case (x, item, link) of
        (TagOpen "item" atributoj, _, _) -> filtriRSS2 True link xs
        (TagOpen "link" atributoj, True, _) -> filtriRSS2 True True xs
        (ContentText teksto, True, True) -> (T.unpack teksto) : (filtriRSS2 False False xs)
        _ -> filtriRSS2 item link xs

filtriMedium :: String -> Bool -> [Token] -> [String]
filtriMedium _ h1 [] = []
filtriMedium servo h1 (x:xs) =
    case (x, h1) of
        (TagOpen "h1" _, False) -> filtriMedium servo True xs
        (TagClose "h1", _) -> filtriMedium servo False xs
        (TagOpen "a" atributoj, True) -> (servo ++ (sercxiAtributon "href" atributoj)) : filtriMedium servo False xs
        _ -> filtriMedium servo h1 xs

filtriBlogon :: String -> String -> [Token] -> [String]
filtriBlogon servo fluo [] = []
filtriBlogon servo fluo (x:xs) =
    case x of
        (TagOpen "a" atributoj) ->
            let
                href = sercxiAtributon "href" atributoj
            in
            if (href == "") || (href == fluo) || (href == "/") || (href == servo) then
               filtriBlogon servo fluo xs
            else if L.isPrefixOf servo href then
               href : (filtriBlogon servo fluo xs)
            else
              let
                sep = if L.isPrefixOf "/" href then "" else "/"
              in
               (servo ++ sep ++ href) : (filtriBlogon servo fluo xs)
        _ -> filtriBlogon servo fluo xs

sercxiAtributon nomo as = concat $ map (\(Attr atributaNomo valo) -> if T.unpack atributaNomo == nomo then (T.unpack valo) else "") as

legiFluon :: String -> Bool -> IO [String]
legiFluon fluo babilu = do
    case ekstraktiServon fluo of
        Just servo -> do
            putStrLn servo
            r <- get fluo
            if r ^. responseStatus . statusCode == 200 then do
                retpagxoj <- return $ ekstrakti servo fluo $ decodeUtf8 $  r ^. responseBody
                seSkribu babilu $ "La fluo \""++ fluo ++"\" havas "++ (show $ length retpagxoj) ++ " retpago(j)n."
                return retpagxoj
            else do
                seSkribu babilu $ "La fluo ne legeblas: responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)
                return []
        Nothing -> do
            seSkribu babilu $ "La fluoligilo ne havas ĝustan servon : " ++ fluo
            return []

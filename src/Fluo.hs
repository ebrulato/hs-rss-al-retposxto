{-# LANGUAGE OverloadedStrings #-}

module Fluo
(
    legiFluon
) where

import           Control.Lens
import qualified Data.List               as L (isInfixOf, isPrefixOf, nub)
import qualified Data.Text               as T (unpack)
import qualified Data.Text.Lazy          as TL (Text, replace)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.Wreq
import           Text.HTML.Parser


import           Helpi


ekstrakti :: String -> String -> TL.Text -> [String]
ekstrakti servo fluo teksto =
    let
        tokens = parseTokensLazy (TL.replace "<![CDATA[" "" (TL.replace "]]>" "" teksto))
        rezultoRSS2 = filtriRSS2 False False tokens
        havasArtikolon = any (\n -> case n of
                TagOpen "article" _ -> True
                _                   -> False
            ) tokens
    in
    case (null rezultoRSS2, "medium.com" `L.isInfixOf` fluo) of
      (True, True)  -> filtriMedium servo False tokens
      (True, False) -> filtriBlogon servo fluo False havasArtikolon tokens
      (False, _)    -> rezultoRSS2


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
        (TagOpen "a" atributoj, True) ->
          let
            href = sercxiAtributon "href" atributoj
          in
            if L.isPrefixOf servo href then
              href : filtriMedium servo False xs
            else
              (servo ++ href) : filtriMedium servo False xs
        _ -> filtriMedium servo h1 xs

filtriBlogon :: String -> String -> Bool -> Bool ->  [Token] -> [String]
filtriBlogon servo fluo ignoru seArtiklo [] = []
filtriBlogon servo fluo ignoru seArtiklo (x:xs) =
    case (x, ignoru, seArtiklo) of
        (TagOpen "head" _, _, _) -> filtriBlogon servo fluo True seArtiklo xs
        (TagClose "head", _, False) -> filtriBlogon servo fluo False seArtiklo xs
        (TagClose "head", _, True) -> filtriBlogon servo fluo True seArtiklo xs
        (TagOpen "footer" _, _, _) -> filtriBlogon servo fluo True seArtiklo xs
        (TagClose "footer", _, False) -> filtriBlogon servo fluo False seArtiklo xs
        (TagClose "footer", _, True) -> filtriBlogon servo fluo True seArtiklo xs
        (TagOpen "article" _, _, _) -> filtriBlogon servo fluo False seArtiklo xs
        (TagClose "article", _, True) -> filtriBlogon servo fluo True seArtiklo xs
        (TagOpen "a" atributoj, False, _) -> filtri atributoj
        _ -> filtriBlogon servo fluo ignoru seArtiklo xs
    where
      filtri atributoj =
            let
                href = sercxiAtributon "href" atributoj
            in
            if L.isPrefixOf servo href then
               href : (filtriBlogon servo fluo False seArtiklo xs)
            else
              if (href == "") || (href == "#") || (href == fluo)
                 || (href == "/") || (href == servo)
                 || (L.isPrefixOf "//" href)
                 || (L.isPrefixOf "http" href)
              then
                filtriBlogon servo fluo False seArtiklo xs
              else
                let
                  sep = if L.isPrefixOf "/" href then "" else "/"
                in
                  (servo ++ sep ++ href) : (filtriBlogon servo fluo False seArtiklo xs)


sercxiAtributon nomo as = concat $ map (\(Attr atributaNomo valo) -> if T.unpack atributaNomo == nomo then (T.unpack valo) else "") as

legiFluon :: String -> Bool -> IO [String]
legiFluon fluo babilu = do
    case ekstraktiServon fluo of
        Just servo -> do
            putStrLn servo
            r <- get fluo
            if r ^. responseStatus . statusCode == 200 then do
                retpagxoj <- return $ L.nub $ ekstrakti servo fluo $ decodeUtf8 $  r ^. responseBody
                seSkribu babilu $ "La fluo \""++ fluo ++"\" havas "++ (show $ length retpagxoj) ++ " retpago(j)n."
                return retpagxoj
            else do
                seSkribu babilu $ "La fluo ne legeblas: responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)
                return []
        Nothing -> do
            seSkribu babilu $ "La fluoligilo ne havas ĝustan servon : " ++ fluo
            return []

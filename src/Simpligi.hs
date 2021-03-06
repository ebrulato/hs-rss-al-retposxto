{-# LANGUAGE OverloadedStrings #-}

module Simpligi
    ( simpligiRetpagxon
    ) where

import           Control.Exception
import           Network.Wreq
import           Control.Lens
import           Text.HTML.Parser
import           Data.Text                            (Text, pack, unpack, append, singleton, strip)
import qualified Data.ByteString               as B   (concat)
import qualified Data.ByteString.Lazy          as BL  (toChunks)
import           Data.Text.Lazy                       (toStrict) 
import           Network.HTTP.Client                  (HttpException)
import           Data.Text.Encoding                   (decodeUtf8, encodeUtf8)
import           Codec.Picture                        (decodeImage, dynamicMap, imageWidth, encodePng)
import           Data.ByteString.Lazy.Base64          (encodeBase64)
import           MalgrandigiBildon
import           GrizigiBildon
import           Helpi

ekstrakti :: Bool -> Int -> String -> Text -> Text -> IO (Text, Text)
ekstrakti porLegilo largxo servo bazaTeksto teksto = 
    let 
        elementoj = parseTokens teksto
        havasArtikolon = any (\n -> case n of 
                TagOpen "article" _ -> True
                _ -> False
            ) elementoj
        titolo = strip $ sercxiTitolon False elementoj
    in do
        teksto <- aliformi porLegilo largxo servo bazaTeksto [Nothing] $ filtri havasArtikolon False [Nothing] elementoj
        return (titolo, teksto)


sercxiTitolon :: Bool -> [Token] -> Text
sercxiTitolon _ [] = "sen titolo"
sercxiTitolon enTitolo (x:xs) = 
    case x of 
        TagOpen nomo atributoj -> 
            case nomo of 
                "title" -> sercxiTitolon True xs 
                _ -> sercxiTitolon enTitolo xs
        TagClose nomo ->
            case nomo of 
                "title" -> ""
                _ -> sercxiTitolon False xs
        ContentText teksto ->
            if enTitolo then
                append teksto $ sercxiTitolon enTitolo xs
            else 
                sercxiTitolon enTitolo xs
        _ -> sercxiTitolon enTitolo xs
   

aliformi :: Bool -> Int -> String -> Text -> [Maybe Token] -> [Token] -> IO Text
aliformi porLegilo largxo servo bazaTeksto _ [] = return bazaTeksto
aliformi porLegilo largxo servo bazaTeksto fermoj (x:xs) = 
    case x of 
        TagOpen nomo atributoj -> 
            case nomo of 
            "title" -> aliformi porLegilo largxo servo (append bazaTeksto "<h1>") ((Just $ TagClose "title"):fermoj) xs
            "time" -> aliformi porLegilo largxo servo (append bazaTeksto "<i>") ((Just $ TagClose "time"):fermoj) xs
            "article" -> aliformi porLegilo largxo servo bazaTeksto ((Just $ TagClose "article"):fermoj) xs
            "p" -> aliformi porLegilo largxo servo (append bazaTeksto "<p>") ((Just $ TagClose "p"):fermoj) xs
            "h1" -> aliformi porLegilo largxo servo (append bazaTeksto "<h1>") ((Just $ TagClose "h1"):fermoj) xs
            "h2" -> aliformi porLegilo largxo servo (append bazaTeksto "<h2>") ((Just $ TagClose "h2"):fermoj) xs
            "h3" -> aliformi porLegilo largxo servo (append bazaTeksto "<h3>") ((Just $ TagClose "h3"):fermoj) xs
            "li" -> aliformi porLegilo largxo servo (append bazaTeksto "<li>") ((Just $ TagClose "li"):fermoj) xs
            "ol" -> aliformi porLegilo largxo servo (append bazaTeksto "<ol>") ((Just $ TagClose "ol"):fermoj) xs
            "ul" -> aliformi porLegilo largxo servo (append bazaTeksto "<ul>") ((Just $ TagClose "ul"):fermoj) xs
            "strong" -> aliformi porLegilo largxo servo (append bazaTeksto "<strong>") ((Just $ TagClose "strong"):fermoj) xs
            "figcaption" -> aliformi porLegilo largxo servo (append bazaTeksto "<center><i>") ((Just $ TagClose "figcaption"):fermoj) xs
            "address" -> aliformi porLegilo largxo servo bazaTeksto ((Just $ TagClose "address"):fermoj) xs
            "a" -> aliformi porLegilo largxo servo (append bazaTeksto $ pack $ "<a href="++ (korektiServon servo $ sercxiAtributon "href" atributoj) ++" >") ((Just $ TagClose "a"):fermoj) xs
            "img" -> aliformiBildonKunEraroj atributoj
            "br" -> aliformi porLegilo largxo servo (append bazaTeksto "<br>") fermoj xs
            _ -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
        TagSelfClose nomo atributoj ->
            case nomo of 
                "img" -> aliformiBildonKunEraroj atributoj
                _ -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
        TagClose nomo -> 
            case head fermoj of 
                Nothing -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
                Just token ->
                    case (token, nomo) of
                        (TagClose "title", "title") -> aliformi porLegilo largxo servo (append bazaTeksto "</h1>") (tail fermoj) xs 
                        (TagClose "time", "time") -> aliformi porLegilo largxo servo (append bazaTeksto "</i>") (tail fermoj) xs 
                        (TagClose "article", "article") -> aliformi porLegilo largxo servo bazaTeksto (tail fermoj) xs
                        (TagClose "p", "p") -> aliformi porLegilo largxo servo (append bazaTeksto "</p>") (tail fermoj) xs
                        (TagClose "h1", "h1") -> aliformi porLegilo largxo servo (append bazaTeksto "</h1>") (tail fermoj) xs
                        (TagClose "h2", "h2") -> aliformi porLegilo largxo servo (append bazaTeksto "</h2>") (tail fermoj) xs
                        (TagClose "h3", "h3") -> aliformi porLegilo largxo servo (append bazaTeksto "</h3>") (tail fermoj) xs
                        (TagClose "li", "li") -> aliformi porLegilo largxo servo (append bazaTeksto "</li>") (tail fermoj) xs
                        (TagClose "ol", "ol") -> aliformi porLegilo largxo servo (append bazaTeksto "</ol>") (tail fermoj) xs
                        (TagClose "ul", "ul") -> aliformi porLegilo largxo servo (append bazaTeksto "</ul>") (tail fermoj) xs
                        (TagClose "strong", "strong") -> aliformi porLegilo largxo servo (append bazaTeksto "</strong>") (tail fermoj) xs
                        (TagClose "figcaption", "figcaption") -> aliformi porLegilo largxo servo (append bazaTeksto "</i></center>") (tail fermoj) xs
                        (TagClose "address", "address") -> aliformi porLegilo largxo servo bazaTeksto (tail fermoj) xs
                        (TagClose "a", "a") -> aliformi porLegilo largxo servo (append bazaTeksto "</a>") (tail fermoj) xs
                        (_, _) -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
        ContentText teksto ->
            case head fermoj of 
                Nothing -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
                Just token -> aliformi porLegilo largxo servo (append bazaTeksto teksto) fermoj xs
        ContentChar litero ->
            case head fermoj of 
                Nothing -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
                Just token -> aliformi porLegilo largxo servo (append bazaTeksto $ singleton litero) fermoj xs
        Comment _ -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
        Doctype _ -> aliformi porLegilo largxo servo bazaTeksto fermoj xs
    where
        sercxiAtributon nomo as = concat $ map (\(Attr atributaNomo valo) -> if unpack atributaNomo == nomo then (unpack valo) else "") as
        aliformiBildonKunEraroj atributoj = do 
            rezulto <- try (aliformiBildon atributoj) 
            case (rezulto :: (Either HttpException Text))  of 
                Left eraro -> aliformi porLegilo largxo servo (append bazaTeksto $ pack $ "<center>"++(show eraro)++"</center><center><i>"++ (sercxiAtributon "alt" atributoj) ++ "</i></center> ") fermoj xs
                Right bonajxo -> return bonajxo
        aliformiBildon atributoj = 
            let 
                src = korektiServon servo $ sercxiAtributon "src" atributoj
                alt = sercxiAtributon "alt" atributoj
            in
                if porLegilo then do
                    eBildaTeksto <- elsxutiBildon src largxo
                    case eBildaTeksto of
                        Left mesagxo -> aliformi porLegilo largxo servo (append bazaTeksto $ pack $ "<center>La bildo "++ src ++" ne videblas</center><center><i>"++ alt ++ "</i></center> ") fermoj xs
                        Right bildaTeksto -> aliformi porLegilo largxo servo (append (append (append bazaTeksto (pack "<center><img src=\"data:image/.png;base64,")) bildaTeksto) (pack $ "\"/></center><center><i>"++ alt ++ "</i></center> ")) fermoj xs
                else 
                    aliformi porLegilo largxo servo (append bazaTeksto $ pack $ "<center><img src=" ++ src ++ " width=400 /></center><center><i>"++ alt ++ "</i></center> ") fermoj xs

              
  


filtri :: Bool -> Bool -> [Maybe Token] -> [Token] -> [Token]
filtri havasArtikolon artikole fermoj [] = []
filtri havasArtikolon artikole fermoj (x:xs) = 
    case x of 
        TagOpen nomo atributoj -> 
            if neScriptAuxStyle fermoj then
                case (nomo, havasArtikolon && artikole || not havasArtikolon) of 
                ("title", _) -> x : filtri havasArtikolon artikole ((Just $ TagClose "title"):fermoj) xs
                ("article", _) -> x : filtri havasArtikolon True ((Just $ TagClose "article"):fermoj) xs
                ("time", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "time"):fermoj) xs
                ("p", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "p"):fermoj) xs
                ("h1", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "h1"):fermoj) xs
                ("h2", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "h2"):fermoj) xs
                ("h3", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "h3"):fermoj) xs
                ("address", _) -> x : filtri havasArtikolon artikole ((Just $ TagClose "address"):fermoj) xs
                ("a", True) -> x : filtri havasArtikolon artikole ((Just $ TagClose "a"):fermoj) xs
                ("script", True) -> filtri havasArtikolon artikole ((Just $ TagClose "script"):fermoj) xs
                ("style", True) -> filtri havasArtikolon artikole ((Just $ TagClose "style"):fermoj) xs
                ("figure", True) -> x: filtri havasArtikolon artikole ((Just $ TagClose "figure"):fermoj) xs
                ("figcaption", True) -> x: filtri havasArtikolon artikole ((Just $ TagClose "figcaption"):fermoj) xs
                ("img", True) -> x : filtri havasArtikolon artikole fermoj xs
                ("br", True) -> x : filtri havasArtikolon artikole fermoj xs
                _ -> case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token -> x : filtri havasArtikolon artikole fermoj xs
            else filtri havasArtikolon artikole fermoj xs
        TagSelfClose _ _ -> 
            if neScriptAuxStyle fermoj then
                case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token -> x : filtri havasArtikolon artikole fermoj xs
            else filtri havasArtikolon artikole fermoj xs
        TagClose nomo -> 
            if neScriptAuxStyle fermoj then
                case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token ->
                        case (token, nomo) of
                            (TagClose "title", "title") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "article", "article") -> x : filtri havasArtikolon False (tail fermoj) xs 
                            (TagClose "time", "time") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "p", "p") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "h1", "h1") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "h2", "h2") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "h3", "h3") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "a", "a") -> x : filtri havasArtikolon artikole (tail fermoj) xs 
                            (TagClose "address", "address") -> x : filtri havasArtikolon artikole (tail fermoj) xs
                            (TagClose "figure", "figure") -> x : filtri havasArtikolon artikole (tail fermoj) xs
                            (TagClose "figcaption", "figcaption") -> x : filtri havasArtikolon artikole (tail fermoj) xs
                            _ -> x : filtri havasArtikolon artikole fermoj xs
            else 
                case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token ->
                        case token of
                            TagClose "script" -> filtri havasArtikolon artikole (tail fermoj) xs 
                            TagClose "style" -> filtri havasArtikolon artikole (tail fermoj) xs 
                            _ -> filtri havasArtikolon artikole fermoj xs
        ContentText teksto ->
            if neScriptAuxStyle fermoj then
                case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token -> x : filtri havasArtikolon artikole fermoj xs
            else filtri havasArtikolon artikole fermoj xs
        ContentChar litero ->
            if neScriptAuxStyle fermoj then
                case head fermoj of 
                    Nothing -> filtri havasArtikolon artikole fermoj xs
                    Just token -> x : filtri havasArtikolon artikole fermoj xs
            else filtri havasArtikolon artikole fermoj xs
        Comment _ -> filtri havasArtikolon artikole fermoj xs
        Doctype _ -> filtri havasArtikolon artikole fermoj xs
    where 
        neScriptAuxStyle fermoj = 
            case head fermoj of 
                Just (TagClose "script") -> False
                Just (TagClose "style") -> False
                _ -> True


simpligiRetpagxon :: String -> Bool -> Int -> Bool -> IO (Either String (Text, Text))
simpligiRetpagxon retpagxo porLegilo largxo babilu = do
    case (ekstraktiServon retpagxo, ekstraktiVojon retpagxo) of
        (Just servo, Just vojo) -> do
            r <- get $ servo ++ "/" ++ vojo
            if r ^. responseStatus . statusCode == 200 then do
                (titolo, teksto) <- ekstrakti porLegilo largxo servo (pack $ "<a href="++retpagxo++">source</a>") $ decodeUtf8 $ B.concat . BL.toChunks $ r ^. responseBody
                if porLegilo then do
                    if babilu then putStrLn $ "ekstraktis por legilo la ligilo: " ++ retpagxo else return ()
                    return $ Right (titolo,  append (append "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"></head><body>" teksto) (pack $ "</body></html>"))
                else do
                    if babilu then putStrLn $ "ekstraktis la ligilo: " ++ retpagxo else return ()
                    return $ Right (titolo, teksto)
            else 
                return $ Left $ "La retpagxo ne legeblas : responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)
        (Nothing, _) -> return $ Left $ "La retpagxo ne havas gxustan servon : " ++ retpagxo
    
elsxutiBildon :: String -> Int -> IO (Either String Text)
elsxutiBildon retBildon largxo = do
    r <- get retBildon 
    if r ^. responseStatus . statusCode == 200 then
        case decodeImage . B.concat . BL.toChunks $ r ^. responseBody of 
            Right bildon -> return $ Right $ toStrict $ encodeBase64 . encodePng . grizigi $ malgrandi bildon largxo
            Left mesagxo -> return $ Left $ "La retbildo ne legeblas : " ++ mesagxo
    else 
        return $ Left $ "La retbildo ne legeblas : responsa kodo = " ++ (show $ r ^. responseStatus . statusCode)



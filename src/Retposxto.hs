{-# LANGUAGE OverloadedStrings #-}

module Retposxto
    ( sendiMajlon
    , sendiMajlojn
    , korekti
    ) where

import              Control.Exception
import qualified    Data.Text                       as T    (Text, unpack, pack, append, replace, breakOn, null, any, strip)
import              Data.Text.IO                    as TIO  (putStrLn)
import qualified    Data.Text.Lazy                  as TL   (pack, fromStrict, toStrict)
import              Data.Text.Lazy.Encoding                 (encodeUtf8, decodeUtf8)
import qualified    Data.ByteString.Lazy            as L
import              Data.String                             (fromString)
import              Data.Char                               (ord)
import              Network.Mail.SMTP
import qualified    Network.Mail.Mime               as M    (plainPart, htmlPart, Mail, Disposition(..), Encoding(..), Part(..), PartContent(..))

sendiMajlon :: (String, String, String) -> String -> T.Text -> T.Text -> Bool -> Bool -> IO (Either String ())
sendiMajlon (servo, salutnomo, pasvorto) retadreso titolo teksto porLegilo babilu = 
    let
        (mail, dosiero) = fariMajlon salutnomo retadreso titolo teksto porLegilo
    in do
        rezulto <- try $ sendMailWithLoginSTARTTLS servo salutnomo pasvorto mail
        case (rezulto :: (Either SomeException ())) of
            Right () -> do
                if babilu then TIO.putStrLn $ T.pack $ "sendis al " ++ retadreso ++ " la dosieron \"" ++ dosiero ++ "\"" else return ()
                return $ Right ()
            Left eraro -> return $ Left (show eraro)


sendiMajlojn :: (String, String, String) -> String -> [(T.Text, T.Text)] -> Bool -> Bool -> IO ()
sendiMajlojn (servo, salutnomo, pasvorto) retadreso dokumentoj porLegilo babilu = 
    let
        majloj = map (\(titolo, teksto) -> fariMajlon salutnomo retadreso titolo teksto porLegilo) dokumentoj
    in do
        con <- connectSMTPSTARTTLS servo
        _ <- sendCommand con (AUTH LOGIN salutnomo pasvorto)
        mapM (\(majlo, dosiero) -> do 
            renderAndSend con majlo
            if babilu then TIO.putStrLn $ T.pack $ "sendis al " ++ retadreso ++ " la dosieron \"" ++ dosiero ++ "\"" else return ()
            ) majloj 
        closeSMTP con

fariMajlon :: String -> String -> T.Text -> T.Text -> Bool -> (M.Mail, String)
fariMajlon salutnomo retadreso titolo teksto porLegilo =
    let 
        from       = Address Nothing (T.pack salutnomo)
        to         = [Address Nothing (T.pack retadreso)]
        cc         = []
        bcc        = []
        subject    = titolo
        body       = M.plainPart $ TL.fromStrict titolo
        dosiero    = (korekti titolo) ++ ".html"
        mail       = if porLegilo then 
                        simpleMail from to cc bcc subject [body, 
                            M.Part
                                "application/octet-stream"
                                M.Base64 
                                (M.AttachmentDisposition $ T.pack dosiero)
                                [("Content-Type", T.append "text/html; charset=UTF-8; name=" titolo)]
                                (M.PartContent (encodeUtf8 . TL.fromStrict $ teksto))
                        ]
                    else 
                        simpleMail from to cc bcc subject [body, (M.htmlPart (TL.fromStrict teksto))]
    in
        (mail, dosiero)


korekti :: T.Text -> String
korekti t =
    farigxiUsonen . 
        demetiSpecialajnStirsignojn . 
        T.unpack . 
        demetiHTMLEntity . 
        korektiFrance . 
        korektiXSistemo . 
        T.strip $ t

korektiXSistemo :: T.Text -> T.Text
korektiXSistemo t =
    T.replace "ĉ" "cx"$ 
    T.replace "Ĉ" "Cx" $
    T.replace "â" "ax" $
    T.replace "Â" "Ax" $
    T.replace "ĝ" "gx" $
    T.replace "Ĝ" "Gx" $
    T.replace "ĥ" "hx" $
    T.replace "Ĥ" "Hx" $
    T.replace "ĵ" "jx" $
    T.replace "Ĵ" "Jx" $
    T.replace "ŝ" "sx" $
    T.replace "Ŝ" "Sx" $
    T.replace "ǚ" "ux" $
    T.replace "Ǔ" "Ux" t

korektiFrance :: T.Text -> T.Text
korektiFrance t =
    T.replace "à" "a"$ 
    T.replace "â" "a" $
    T.replace "À" "A" $
    T.replace "Â" "A" $
    T.replace "ç" "c" $
    T.replace "Ç" "C" $
    T.replace "è" "e" $
    T.replace "È" "E" $
    T.replace "é" "e" $
    T.replace "É" "E" $
    T.replace "ê" "e" $
    T.replace "Ê" "E" $
    T.replace "î" "i" $
    T.replace "Î" "I" $
    T.replace "ï" "i" $
    T.replace "Ï" "I" $
    T.replace "ô" "o" $
    T.replace "œ" "oe" $
    T.replace "ö" "o" $
    T.replace "Ô" "O" $
    T.replace "Œ" "OE" $
    T.replace "Ö" "O" $    
    T.replace "ù" "u" $    
    T.replace "ü" "u" $    
    T.replace "Ù" "U" $    
    T.replace "Ü" "U" $    
    T.replace "Û" "U" $    
    T.replace "û" "u" t

demetiHTMLEntity :: T.Text -> T.Text
demetiHTMLEntity t =
    let 
        (unua, dua) = (T.breakOn "&" t)
    in
        if T.null dua then unua
        else T.append unua $ demetiHTMLEntity2 dua

demetiHTMLEntity2 :: T.Text -> T.Text
demetiHTMLEntity2 t = 
    let 
        (unua, dua) = (T.breakOn ";" t)
    in
        if T.null dua then unua
        else if T.any (\k -> k == ' ') unua then T.append unua $ demetiHTMLEntity dua
        else demetiHTMLEntity dua


-- v2 rfc2045 kay aliaj
demetiSpecialajnStirsignojn :: String -> String
demetiSpecialajnStirsignojn xs = [x | x <- xs, not (elem x ("()<>@,.\\/?![]=:;\"\'" :: [Char])) ]

farigxiUsonen :: String -> String
farigxiUsonen s =
    filter (\n -> ord n > 47 && ord n < 58
                  || ord n == 32
                  || ord n > 64 && ord n < 91
                  || ord n > 96 && ord n < 123) s
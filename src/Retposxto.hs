{-# LANGUAGE OverloadedStrings #-}

module Retposxto
    ( sendiMajlon
    ) where

import              Control.Exception
import qualified    Data.Text                       as T    (Text, unpack, pack, append, replace, breakOn, null, any, strip)
import              Data.Text.IO                    as TIO  (putStrLn)
import qualified    Data.Text.Lazy                  as TL   (pack, fromStrict, toStrict)
import              Data.Text.Lazy.Encoding                 (encodeUtf8, decodeUtf8)
import qualified    Data.ByteString.Lazy            as L
import              Data.String                             (fromString)
import              Network.Mail.SMTP
import qualified    Network.Mail.Mime               as M    (plainPart, htmlPart, Disposition(..), Encoding(..), Part(..), PartContent(..))

sendiMajlon :: (String, String, String) -> String -> T.Text -> T.Text -> Bool -> IO (Either String ())
sendiMajlon (servo, salutnomo, pasvorto) retadreso titolo teksto porLegilo = 
    let 
        from       = Address Nothing (T.pack salutnomo)
        to         = [Address Nothing (T.pack retadreso)]
        cc         = []
        bcc        = []
        subject    = titolo
        body       = M.plainPart $ TL.fromStrict titolo
        dosiero    = korekti titolo 
        mail       = if porLegilo then 
                        simpleMail from to cc bcc subject [body, 
                            M.Part
                                "application/octet-stream"
                                M.Base64 
                                (M.AttachmentDisposition (T.pack $ dosiero ++ ".html"))
                                [("Content-Type", T.append "text/html; charset=UTF-8; name=" titolo)]
                                (M.PartContent (encodeUtf8 . TL.fromStrict $ teksto))
                        ]
                    else 
                        simpleMail from to cc bcc subject [body, (M.htmlPart (TL.fromStrict teksto))]
    in do
        rezulto <- try $ sendMailWithLoginSTARTTLS servo salutnomo pasvorto mail
        case (rezulto :: (Either SomeException ())) of
            Right () -> return $ Right ()
            Left eraro -> return $ Left (show eraro)


korekti :: T.Text -> String
korekti t =
    demetiSpecialajnStirsignojn . T.unpack . demetiHTMLEntity . korektiXSistemo . T.strip $ t

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


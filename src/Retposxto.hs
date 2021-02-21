{-# LANGUAGE OverloadedStrings #-}

module Retposxto
    ( sendiMajlon
    ) where

import              Control.Exception
import qualified    Data.Text                       as T    (Text, pack, append)
import qualified    Data.Text.Lazy                  as TL   (pack, fromStrict, toStrict)
import              Data.Text.Lazy.Encoding                 (encodeUtf8, decodeUtf8)
import              Blaze.ByteString.Builder
import qualified    Data.ByteString.Lazy            as L
import              Data.String                             (fromString)
import              Network.Mail.SMTP
import qualified    Network.Mail.Mime               as M    (plainPart, htmlPart, addAttachmentBS, quotedPrintable)

sendiMajlon :: (String, String, String) -> String -> String -> T.Text -> Bool -> IO (Either String ())
sendiMajlon (servo, salutnomo, pasvorto) retadreso titolo teksto porLegilo = 
    let 
        from       = Address Nothing (T.pack salutnomo)
        to         = [Address Nothing (T.pack retadreso)]
        cc         = []
        bcc        = []
        subject    = T.pack titolo
        body       = M.plainPart (TL.pack titolo)
        mail       = if porLegilo then 
                        M.addAttachmentBS
                            "application/octet-stream"
                            (T.append (TL.toStrict . decodeUtf8 . L.fromStrict . toByteString $ M.quotedPrintable True $ fromString $ titolo) ".html")
                            (encodeUtf8 . TL.fromStrict $ teksto)
                            (simpleMail from to cc bcc subject [body])
                    else 
                        simpleMail from to cc bcc subject [body, (M.htmlPart (TL.fromStrict teksto))]
    in do
        rezulto <- try $ sendMailWithLoginSTARTTLS servo salutnomo pasvorto mail
        case (rezulto :: (Either SomeException ())) of
            Right () -> return $ Right ()
            Left eraro -> return $ Left (show eraro)

       


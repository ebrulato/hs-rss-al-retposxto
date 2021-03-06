module Helpi (
    ekstraktiServon
    , ekstraktiVojon
    , korektiServon
    , seSkribu
) where

import           Network.URL                          (importURL, URLType(..), exportHost, url_type, url_path)


ekstraktiServon :: String -> Maybe String 
ekstraktiServon retpagxo = 
    case importURL retpagxo of 
        Nothing -> Nothing
        Just url ->
            case url_type url of 
                Absolute servo -> 
                    Just $ (exportHost servo)
                _ -> Nothing 

ekstraktiVojon :: String -> Maybe String
ekstraktiVojon retpagxo =
    case importURL retpagxo of
        Nothing -> Nothing
        Just url -> Just $ url_path url


korektiServon :: String -> String -> String
korektiServon servo retpagxo =
    case importURL retpagxo of 
        Nothing -> servo ++ retpagxo -- ?
        Just url ->
            case url_type url of 
                Absolute servo -> retpagxo                    
                HostRelative -> servo ++ retpagxo
                PathRelative -> servo ++ "/" ++ retpagxo

seSkribu :: Bool -> String -> IO () 
seSkribu babilu mesagxo =
    if babilu then putStrLn mesagxo else return ()
                
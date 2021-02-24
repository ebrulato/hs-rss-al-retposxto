module Helpi (
    ekstraktiServon
    , korektiServon
    , seSkribu
) where

import           Network.URL                          (importURL, URLType(..), exportHost, url_type)


ekstraktiServon :: String -> Maybe String 
ekstraktiServon retpagxo = 
    case importURL retpagxo of 
        Nothing -> Nothing
        Just url ->
            case url_type url of 
                Absolute servo -> 
                    Just $ (exportHost servo)
                _ -> Nothing 

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
                
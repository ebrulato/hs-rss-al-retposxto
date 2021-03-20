{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO                  as T     (putStrLn, writeFile) -- ?
import           Data.Text                              (Text, append, unpack, pack, dropEnd)
import qualified Simpligi                      as S     (simpligiRetpagxon)
import           Data.List
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B
import           System.Console.GetOpt
import           System.Directory 
import           System.Environment
import           System.Exit
import           System.IO
import           Control.Exception
import           Control.Monad
import           Text.Read
import           Versio
import           Retposxto
import qualified Fluo                          as F     (legiFluon)
import qualified Fluoj                         as FJ    (kreiFluon, kreiFluojn, Fluo, Fluoj, malkodiFluojn, kodiFluojn, aldoni, novajnRetpagxojn)
import           Helpi
import           Alklaki

retposxtaServo = "SMTP_SERVER"
retposxtaSalutnomo = "SMTP_LOGIN"
retposxtaPasvorto = "SMTP_PASSWORD"

data Flag
        = Legilo                -- -l --legilo
        | Dosiero               -- -d --dosiero
        | Largxo String         --    --largxo
        | Retadreso String      --    --retadreso
        | Help                  -- -h --helpo
        | Versio                -- -v --vervio
        | Fluo                  -- -f --fluo
        | Babili                -- -b --babili
        | Kindle                -- -k --kindle
        | Dosierejo String      -- -j --dosierejo
        deriving (Show, Eq)


flagoj =
       [Option ['l']    ["legilo"]      (NoArg Legilo)                          "La simpligita retpaĝo kunhavas ĝia(j)n bildon(j)n tiel\nĝi legeblas legile (Kindle, ...)"
       ,Option ['d']    ["dosiero"]     (NoArg Dosiero)                         "Skribas la simpligita retpaĝo en dosiero nomigita kun\nla titolo de la artikolo"
       ,Option []       ["dosierejo"]   (ReqArg Dosierejo "10")                 "Kun la -retadreso parametro, ĉi tiu ilo serĉas la\ndosierojn en la nuna dosierejo. La valoro estas la\nkvanto de dosieroj elsendotaj."
       ,Option []       ["largxo"]      (ReqArg Largxo "400")                   "Se vi uzas -l/--legilo parametron, la largxo definas\nla dimensiojn de la bildojn. La defaǔltvaloro estas\n400 rastrumeroj."
       ,Option []       ["retadreso"]   (ReqArg Retadreso "nomo@servo.eo")      "Se vi uzas -l/--legilo parametron, la artikolo\nelsendigitos emajle al la retadreso."
       ,Option ['f']    ["fluo"]        (NoArg Fluo)                            "Se vi uzas --fluo parametron, vi enlistigas la\nligilo(j)n kiel fluo(j) registren sur via diskingo\nAǓ vi uzas la dosieron «fluoj.json» por\ntrovi retpaĝojn."
       ,Option ['k']    ["kindle"]      (NoArg Kindle)                          "Estonteca uzado…"
       ,Option ['h']    ["helpo"]       (NoArg Help)                            "Afiŝas tiun ĉi mesaĝon por helpi vin."
       ,Option ['v']    ["versio"]      (NoArg Versio)                          "Afiŝas la version de tiu ĉi ilo."
       ,Option ['b']    ["babili"]      (NoArg Babili)                          "Afiŝas mesaĝojn dum la ago de la programo."
       ]

parse prgNomo argv = case getOpt Permute flagoj argv of

        (args,w,[]) -> do
            let words = if null w then [] else w
            if Help `elem` args
            then do hPutStrLn stderr (usageInfo (header prgNomo) flagoj)
                    exitWith ExitSuccess
            else if Versio `elem` args 
            then do hPutStrLn stdout (prgNomo ++ " " ++ Versio.versio)
                    exitWith ExitSuccess
            else return (nub args, words)

        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo (header prgNomo) flagoj)
            exitWith (ExitFailure 1)

        where header prgName = "Uzado: "++ prgNomo ++ " [-ldfkhvb] <retpaĝojn aǔ fluoligilon> \n\n"

sercxiFlago :: (Flag -> Maybe a) -> [Flag] -> Maybe a
sercxiFlago f [] = Nothing
sercxiFlago f (flago:flagoj) = 
    case f flago of 
        Just s -> Just s 
        Nothing -> sercxiFlago f flagoj

sercxiDosierejo :: Int -> [Flag] -> Int 
sercxiDosierejo defauxlto flagoj = 
        case dosierejoDemandon flagoj of 
            Just v -> if v < 1 then defauxlto else v
            Nothing -> defauxlto


sercxiLargxo :: Int -> [Flag] -> Int 
sercxiLargxo defauxlto flagoj = 
    let 
        valoro = sercxiFlago (\n -> case n of 
                        Largxo v -> readMaybe v   
                        _ -> Nothing ) flagoj
    in
        case valoro of 
            Just v -> if v < 10 then defauxlto else v
            Nothing -> defauxlto

sercxiRetadreso :: [Flag] -> Maybe String
sercxiRetadreso = sercxiFlago (\n -> case n of 
                        Retadreso v -> Just v
                        _ -> Nothing ) 

dosierejoDemandon :: [Flag] -> Maybe Int
dosierejoDemandon = sercxiFlago (\n -> case n of 
                        Dosierejo v -> readMaybe v
                        _ -> Nothing )

seDosierejoDemandon :: [Flag] -> Bool
seDosierejoDemandon flagoj = dosierejoDemandon flagoj /= Nothing

devasBabili :: [Flag] -> Bool
devasBabili = elem Babili 

fluaDemandon :: [Flag] -> Bool
fluaDemandon = elem Fluo 

kindleDemandon :: [Flag] -> Bool
kindleDemandon = elem Kindle 


fariDosiero :: Maybe ([Flag], Text, Text) -> IO (Maybe ([Flag], Text, Text))
fariDosiero Nothing = return Nothing
fariDosiero (Just (args, titolo, teksto)) =
    if (Dosiero `elem` args) then do
        nomo <- return $ (korekti titolo) ++ ".html"
        T.writeFile (nomo) teksto
        putStrLn $ "skribus diskingen " ++ nomo 
        return $ Just (args, titolo, teksto)
    else 
        case sercxiRetadreso args of 
            Just _ -> return $ Just (args, titolo, teksto)
            Nothing -> do 
                T.putStrLn teksto 
                return Nothing

sercxiRetposxto :: IO (Maybe (String, String, String))
sercxiRetposxto = do
    mbRetposxtaServo <- lookupEnv retposxtaServo
    mbRetposxtaSalutnomo <- lookupEnv retposxtaSalutnomo
    mbRetposxtaPasvorto <- lookupEnv retposxtaPasvorto
    case (mbRetposxtaServo, mbRetposxtaSalutnomo, mbRetposxtaPasvorto) of 
        (Just servo, Just salutnomo, Just pasvorto) -> return $ Just $ (servo, salutnomo, pasvorto)
        _ -> return $ Nothing

elsendiMajlojn :: [Flag] -> [String] -> IO ()
elsendiMajlojn args retpagxojn = do
    mbDokumentoj <- mapM (simpligiRetpagxon args) retpagxojn          
    mbRetposxto <- sercxiRetposxto
    case (sercxiRetadreso args, mbRetposxto) of
        (Nothing, _) -> return ()
        (Just retadreso, Just retposxto) -> do
            dokumentoj <- return $ map (\mb -> case mb of
                Nothing -> ("", "") -- neniam
                Just v -> v
                ) $ filter (\mb -> case mb of
                        Nothing -> False
                        Just _ -> True
                    ) mbDokumentoj
            sendiMajlojn retposxto retadreso dokumentoj (Legilo `elem` args) (devasBabili args) 

sercxiDosierojnHTML :: [FilePath] -> [FilePath]
sercxiDosierojnHTML dosieroj =
    filter (\n -> isSuffixOf ".html" n) dosieroj

sercxiDosieron :: FilePath -> IO (Text, Text)
sercxiDosieron fp = do
    datumoj <- readFile fp
    return $ (dropEnd 5 $ pack fp, pack datumoj)

sercxiDosierojn :: Int -> IO ([(Text, Text)])
sercxiDosierojn kvantoDeDosiero = do
    tie <- getCurrentDirectory
    cxioj <- listDirectory tie
    cxiojHtml <- pure $ sercxiDosierojnHTML cxioj
    mapM sercxiDosieron $ take kvantoDeDosiero cxiojHtml

elsendiMajlojnElDosierejo :: [Flag] -> IO ()
elsendiMajlojnElDosierejo args  = do
    dokumentoj <- sercxiDosierojn $ sercxiDosierejo 10 args          
    mbRetposxto <- sercxiRetposxto
    case (sercxiRetadreso args, mbRetposxto) of
        (Just retadreso, Just retposxto) -> do
            sendiMajlojn retposxto retadreso dokumentoj (Legilo `elem` args) (devasBabili args)
            seSkribu (devasBabili args) "Forviŝas dosierojn"
            mapM (\n -> do 
                dokumento <- pure $ unpack . fst $ n
                removeFile $ dokumento ++ ".html"
                seSkribu (devasBabili args) $ " « "++dokumento++" » forviŝita." ) dokumentoj 
            return ()
        _ -> return ()

simpligiRetpagxon :: [Flag] -> String -> IO (Maybe (Text, Text))
simpligiRetpagxon args retpagxo = do 
    retpagxoSimpligita <- S.simpligiRetpagxon retpagxo (Legilo `elem` args) (sercxiLargxo 400 args) (devasBabili args)
    case retpagxoSimpligita of 
        Left mesagxo -> do 
            putStrLn mesagxo
            return $ Nothing
        Right (titolo, teksto) -> do 
            fariDosiero (Just (args, titolo, teksto)) 
            return (Just (titolo, teksto))

legiFluon :: [Flag] -> String -> IO FJ.Fluo
legiFluon args fluo = do
    babilu <- return $ devasBabili args
    retpagxojn <- F.legiFluon fluo babilu
    if babilu then do 
        mapM putStrLn retpagxojn
        return ()
    else return ()
    return $ FJ.kreiFluon fluo retpagxojn

legiMalnovajnFluojn :: IO FJ.Fluoj
legiMalnovajnFluojn = do
    eBytes <- try $ B.readFile "fluoj.json"
    case (eBytes :: (Either SomeException B.ByteString)) of
        Right fluoj -> return $ FJ.malkodiFluojn $ BL.fromStrict fluoj
        Left eraro -> return $ FJ.kreiFluojn []

main :: IO ()
main = do
    prgNomo <- getProgName
    (args, ligiloj) <- getArgs >>= parse prgNomo
    if null ligiloj then do
        if seDosierejoDemandon args then do 
            elsendiMajlojnElDosierejo args
        else if fluaDemandon args then do
            malnovajFluoj <- legiMalnovajnFluojn
            (retpagxojn, novajFluoj) <- FJ.novajnRetpagxojn malnovajFluoj
            if (devasBabili args) then do 
                seSkribu True $ "Nova(j) retpagxo(j) "++ (show $ length retpagxojn)
                mapM putStrLn retpagxojn
                return ()
            else
                return ()
            rezulto <- try $ elsendiMajlojn args retpagxojn
            case (rezulto :: (Either SomeException ())) of
                Right () -> do
                    seSkribu (devasBabili args) "savi la stato de la fluojn"
                    BL.writeFile "fluoj.json" $ FJ.kodiFluojn novajFluoj
                Left eraro -> do 
                    putStrLn $ show eraro          
        else do
            putStrLn "Vi donu retpaĝojn aǔ fluoligilojn."
    else
        if fluaDemandon args then do
            fluoj <- mapM (legiFluon args) ligiloj            
            novajFluoj <- return $ FJ.kreiFluojn fluoj
            malnovajFluoj <- legiMalnovajnFluojn
            (kf, l) <- return $ FJ.aldoni malnovajFluoj novajFluoj
            BL.writeFile "fluoj.json" $ FJ.kodiFluojn kf
            seSkribu (devasBabili args) $ "Aldoni "++ (show l) ++ " fluo(j)n."
        else do
            elsendiMajlojn args ligiloj       
    if kindleDemandon args then
        alklakiKindle
    else 
        return ()
            

alklakiKindle :: IO ()
alklakiKindle = do
    mbRetposxto <- sercxiRetposxto
    case mbRetposxto of
        Nothing -> return ()
        Just retposxto -> alklakiKontrolon retposxto


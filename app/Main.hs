{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.List
import           Data.Text             (Text, append, dropEnd, pack, unpack)
import qualified Data.Text.IO          as T (putStrLn, writeFile)
import qualified Fluo                  as F (legiFluon)
import qualified Fluoj                 as FJ (Fluo, Fluoj, aldoni, kodiFluojn,
                                              kreiFluojn, kreiFluon,
                                              malkodiFluojn, novajnRetpagxojn)
import           Helpi
import           Retposxto
import qualified Simpligi              as S (simpligiRetpagxon)
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read
import           Versio

retposxtaServo = "SMTP_SERVO"
retposxtaSalutnomo = "SMTP_SALUTNOMO"
retposxtaPasvorto = "SMTP_PASVORTO"

data Flag
        = Legilo                -- -l --legilo
        | Dosiero               -- -d --dosiero
        | Largxo String         --    --largxo
        | Retadreso String      --    --retadreso
        | Help                  -- -h --helpo
        | Versio                -- -v --vervio
        | Fluo                  -- -f --fluo
        | Babili                -- -b --babili
        | Dosierejo String      -- -j --dosierejo
        deriving (Show, Eq)


flagoj =
       [Option ['l']    ["legilo"]      (NoArg Legilo)                          "La simpligita retpaĝo kunhavas ĝia(j)n bildon(j)n tiel\nĝi legeblas legile (Kindle, ...)"
       ,Option ['d']    ["dosiero"]     (NoArg Dosiero)                         "Skribas la simpligita retpaĝo en dosiero nomigita kun\nla titolo de la artikolo"
       ,Option []       ["dosierejo"]   (ReqArg Dosierejo "10")                 "Kun la -retadreso parametro, ĉi tiu ilo serĉas la\ndosierojn en la nuna dosierejo. La valoro estas la\nkvanto de dosieroj elsendotaj."
       ,Option []       ["largxo"]      (ReqArg Largxo "400")                   "Se vi uzas -l/--legilo parametron, la largxo definas\nla dimensiojn de la bildojn. La defaǔltvaloro estas\n400 rastrumeroj."
       ,Option []       ["retadreso"]   (ReqArg Retadreso "nomo@servo.eo")      "Se vi uzas -l/--legilo parametron, la artikolo\nelsendigitos emajle al la retadreso."
       ,Option ['f']    ["fluo"]        (NoArg Fluo)                            "Se vi uzas --fluo parametron, vi enlistigas la\nligilo(j)n kiel fluo(j) registren sur via diskingo\nAǓ vi uzas la dosieron «fluoj.json» por\ntrovi retpaĝojn."
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


fariDosiero :: Bool -> Maybe String -> Text -> Text -> IO (Maybe (Text, Text))
fariDosiero faruDosieron mbRetadreso titolo teksto =
    if faruDosieron then do
        nomo <- return $ (korekti titolo) ++ ".html"
        T.writeFile (nomo) teksto
        putStrLn $ "skribus diskingen " ++ nomo
        return $ Just (titolo, teksto)
    else
        case mbRetadreso of
            Just _ -> return $ Just (titolo, teksto)
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
        _ -> do
          putStrLn $ "Vi devu krei mediajn parametrojn "++ retposxtaServo ++", "++ retposxtaSalutnomo  ++" kaj "++ retposxtaPasvorto  ++"."
          return $ Nothing

elsendiMajlojn :: Bool -> Bool -> Int -> Bool -> Maybe String -> [String] -> IO ()
elsendiMajlojn babilu porLegilo bildLargxo faruDosieron mbRetadreso retpagxojn = do
    mbDokumentoj <- mapM (simpligiRetpagxon porLegilo babilu bildLargxo faruDosieron mbRetadreso) retpagxojn
    mbRetposxto <- sercxiRetposxto
    case (mbRetadreso, mbRetposxto) of
        (Just retadreso, Just retposxto) -> do
            dokumentoj <- return $ map (\mb -> case mb of
                Nothing -> ("", "") -- neniam
                Just v  -> v
                ) $ filter (\mb -> case mb of
                        Nothing -> False
                        Just _  -> True
                    ) mbDokumentoj
            sendiMajlojn retposxto retadreso dokumentoj porLegilo babilu
        _ -> return ()

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

elsendiMajlojnElDosierejo :: Bool -> Bool -> Int -> Maybe String -> IO ()
elsendiMajlojnElDosierejo babilu porLegilo kvantoDeDosiero mbRetadreso = do
    dokumentoj <- sercxiDosierojn kvantoDeDosiero
    mbRetposxto <- sercxiRetposxto
    case (mbRetadreso, mbRetposxto) of
        (Just retadreso, Just retposxto) -> do
            sendiMajlojn retposxto retadreso dokumentoj porLegilo babilu
            mapM (\n -> do
                dokumento <- pure $ unpack . fst $ n
                removeFile $ dokumento ++ ".html"
                seSkribu babilu $ " « " ++ dokumento ++ " » forviŝita." ) dokumentoj
            return ()
        _ -> return ()

simpligiRetpagxon :: Bool -> Bool -> Int -> Bool -> Maybe String -> String -> IO (Maybe (Text, Text))
simpligiRetpagxon porLegilo babilu bildLargxo faruDosiero mbRetadreso retpagxo = do
    retpagxoSimpligita <- S.simpligiRetpagxon retpagxo porLegilo bildLargxo babilu
    case retpagxoSimpligita of
        Left mesagxo -> do
            putStrLn mesagxo
            return $ Nothing
        Right (titolo, teksto) -> do
            fariDosiero faruDosiero mbRetadreso titolo teksto

legiFluon :: Bool -> String -> IO FJ.Fluo
legiFluon babilu fluo = do
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
        Left eraro  -> return $ FJ.kreiFluojn []

main :: IO ()
main = do
    prgNomo <- getProgName
    (args, ligiloj) <- getArgs >>= parse prgNomo

    babilu <- return $ Babili `elem` args
    porLegilo <- return $ Legilo `elem` args
    bildLargxo <- return $ sercxiLargxo 400 args
    kvantoDeDosiero <- return $ sercxiDosierejo 10 args
    mbRetadreso <- return $ sercxiRetadreso args
    faruDosiero <- return $ Dosiero `elem` args
    fluaDemando <- return $ Fluo `elem` args
    dosierejoDemando <- return $ dosierejoDemandon args /= Nothing

    if null ligiloj then do
        if dosierejoDemando then do
            elsendiMajlojnElDosierejo babilu porLegilo kvantoDeDosiero mbRetadreso
        else if fluaDemando then do
            malnovajFluoj <- legiMalnovajnFluojn
            (retpagxojn, novajFluoj) <- FJ.novajnRetpagxojn malnovajFluoj
            if babilu then do
                seSkribu True $ "Nova(j) retpagxo(j) "++ (show $ length retpagxojn)
                mapM putStrLn retpagxojn
                return ()
            else
                return ()
            rezulto <- try $ elsendiMajlojn babilu porLegilo bildLargxo faruDosiero mbRetadreso retpagxojn
            case (rezulto :: (Either SomeException ())) of
                Right () -> do
                    seSkribu babilu "savi la stato de la fluojn"
                    BL.writeFile "fluoj.json" $ FJ.kodiFluojn novajFluoj
                Left eraro -> do
                    putStrLn $ show eraro
        else do
            putStrLn "Vi donu retpaĝojn aǔ fluoligilojn."
    else
        if fluaDemando then do
            fluoj <- mapM (legiFluon babilu) ligiloj
            novajFluoj <- return $ FJ.kreiFluojn fluoj
            malnovajFluoj <- legiMalnovajnFluojn
            (kf, l) <- return $ FJ.aldoni malnovajFluoj novajFluoj
            BL.writeFile "fluoj.json" $ FJ.kodiFluojn kf
            seSkribu babilu $ "Aldoni "++ (show l) ++ " fluo(j)n."
        else do
            elsendiMajlojn babilu porLegilo bildLargxo faruDosiero mbRetadreso ligiloj
    return ()


sercxiFlago :: (Flag -> Maybe a) -> [Flag] -> Maybe a
sercxiFlago f [] = Nothing
sercxiFlago f (flago:flagoj) =
    case f flago of
        Just s  -> Just s
        Nothing -> sercxiFlago f flagoj

sercxiDosierejo :: Int -> [Flag] -> Int
sercxiDosierejo defauxlto flagoj =
        case dosierejoDemandon flagoj of
            Just v  -> if v < 1 then defauxlto else v
            Nothing -> defauxlto


sercxiLargxo :: Int -> [Flag] -> Int
sercxiLargxo defauxlto flagoj =
    let
        valoro = sercxiFlago (\n -> case n of
                        Largxo v -> readMaybe v
                        _        -> Nothing ) flagoj
    in
        case valoro of
            Just v  -> if v < 10 then defauxlto else v
            Nothing -> defauxlto

sercxiRetadreso :: [Flag] -> Maybe String
sercxiRetadreso = sercxiFlago (\n -> case n of
                        Retadreso v -> Just v
                        _           -> Nothing )

dosierejoDemandon :: [Flag] -> Maybe Int
dosierejoDemandon = sercxiFlago (\n -> case n of
                        Dosierejo v -> readMaybe v
                        _           -> Nothing )









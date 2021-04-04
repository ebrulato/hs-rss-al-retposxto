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

        where header prgName = "Uzado: "++ prgNomo ++ " [-bdfhlv] <retpaĝojn aǔ fluoligilon> \n\n"


type Retadreso = String
type Ligilo = String
type Babili = Bool
type PorLegilo = Bool
type BildLargxo = Int
type Titolo = Text
type Teksto = Text
type Fluojn = FJ.Fluoj
type Farita = Bool
type FariDosiero = Bool
type FluaDemando = Bool
type DosierejoDemando = Bool

data RSSalRetposxtoProgramo a
  = HavigiParametrojn [Flag] [Ligilo] (Babili -> Maybe Retadreso -> PorLegilo -> BildLargxo -> Int -> FariDosiero -> FluaDemando -> DosierejoDemando -> [Ligilo] -> RSSalRetposxtoProgramo a)
  | AldoniFluojn [Ligilo] Babili (RSSalRetposxtoProgramo a)
  | LegiFluojn Babili PorLegilo BildLargxo ([(Titolo, Teksto)] -> Fluojn -> RSSalRetposxtoProgramo a)
  | LegiFluojnElLigilojn [Ligilo] Babili PorLegilo BildLargxo ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo a)
  | LegiFluojnElDosierejo Babili Int ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo a)
  | ElsendiMajlon Retadreso Babili PorLegilo [(Titolo, Teksto)] (Farita -> RSSalRetposxtoProgramo a)
  | AfisxiDosierojn Babili [(Titolo, Teksto)] (RSSalRetposxtoProgramo a)
  | SkribiDosierojn Babili [(Titolo, Teksto)] (RSSalRetposxtoProgramo a)
  | SkribiFluojn Fluojn Babili (RSSalRetposxtoProgramo a)
  | MalAperigiDosierojn [Titolo] Babili (RSSalRetposxtoProgramo a)
  | NeParametron a
  | Fino a

interpreti :: RSSalRetposxtoProgramo a -> IO a

interpreti (Fino a) = pure a

interpreti (NeParametron a) = do
  putStrLn "Vi donu retpaĝojn aǔ fluoligilojn."
  interpreti (Fino a)

interpreti (HavigiParametrojn args ligiloj sekvo) = do
  babilu <- return $ Babili `elem` args
  porLegilo <- return $ Legilo `elem` args
  bildLargxo <- return $ sercxiLargxo 400 args
  kvantoDeDosiero <- return $ sercxiDosierejo 10 args
  mbRetadreso <- return $ sercxiRetadreso args
  faruDosiero <- return $ Dosiero `elem` args
  fluaDemando <- return $ Fluo `elem` args
  dosierejoDemando <- return $ dosierejoDemandon args /= Nothing
  interpreti (sekvo babilu mbRetadreso porLegilo bildLargxo kvantoDeDosiero faruDosiero fluaDemando dosierejoDemando ligiloj)

interpreti (AldoniFluojn ligiloj babilu sekvo) = do
  fluoj <- mapM (legiFluon babilu) ligiloj
  novajFluoj <- return $ FJ.kreiFluojn fluoj
  malnovajFluoj <- legiMalnovajnFluojn
  (kf, l) <- return $ FJ.aldoni malnovajFluoj novajFluoj
  BL.writeFile "fluoj.json" $ FJ.kodiFluojn kf
  seSkribu babilu $ "Aldoni "++ (show l) ++ " fluo(j)n."
  interpreti sekvo

interpreti (LegiFluojn babilu porLegilo bildLargxo sekvo) = do
  malnovajFluoj <- legiMalnovajnFluojn
  (retpagxojn, novajFluoj) <- FJ.novajnRetpagxojn malnovajFluoj
  seSkribu babilu ("Nova(j) retpagxo(j) "++ (show $ length retpagxojn))
  mapM (seSkribu babilu) retpagxojn
  dokumentoj <- simpligiRetpagxojn babilu porLegilo bildLargxo retpagxojn
  interpreti (sekvo dokumentoj novajFluoj)

interpreti (LegiFluojnElLigilojn ligilojn babilu porLegilo bildLargxo sekvo) = do
  dokumentoj <- simpligiRetpagxojn babilu porLegilo bildLargxo ligilojn
  interpreti (sekvo dokumentoj)

interpreti (LegiFluojnElDosierejo babilu kvantoDeDosiero sekvo) = do
  dokumentoj <- sercxiDosierojn babilu kvantoDeDosiero
  interpreti (sekvo dokumentoj)

interpreti (ElsendiMajlon retadreso babilu porLegilo dokumentoj sekvo) = do
  mbRetposxto <- sercxiRetposxto
  case mbRetposxto of
    Just retposxto -> do
      rezulto <- try $ sendiMajlojn retposxto retadreso dokumentoj porLegilo babilu
      case (rezulto :: (Either SomeException ())) of
        Right () ->
          interpreti (sekvo True)
        Left eraro -> do
          putStrLn $ show eraro
          interpreti (sekvo False)
    Nothing ->
      interpreti (sekvo False)

interpreti (AfisxiDosierojn babilu dokumentoj sekvo) = do
  mapM (\(titolo, teksto) -> do
           seSkribu babilu $ unpack teksto
          ) dokumentoj
  interpreti sekvo

interpreti (SkribiDosierojn babilu dokumentoj sekvo) = do
  mapM (\(titolo, teksto) -> do
            nomo <- return $ (korekti titolo) ++ ".html"
            T.writeFile (nomo) teksto
            seSkribu babilu $ "skribus diskingen " ++ nomo
          ) dokumentoj
  interpreti sekvo

interpreti (SkribiFluojn fluojn babilu sekvo) = do
  seSkribu babilu "savi la stato de la fluojn"
  BL.writeFile "fluoj.json" $ FJ.kodiFluojn fluojn
  interpreti sekvo

interpreti (MalAperigiDosierojn titoloj babilu sekvo) = do
  mapM (\titolo -> do
           dokumento <- pure $ unpack titolo
           removeFile $ dokumento ++ ".html"
           seSkribu babilu $ " « " ++ dokumento ++ " » forviŝita." ) titoloj
  interpreti sekvo


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


simpligiRetpagxojn :: Babili -> PorLegilo -> BildLargxo -> [Ligilo] -> IO ([(Titolo, Teksto)])
simpligiRetpagxojn babilu porLegilo bildLargxo retpagxojn = do
    mbDokumentoj <- mapM (simpligiRetpagxon porLegilo babilu bildLargxo) retpagxojn
    dokumentoj <- return $ map (\mb -> case mb of
                Nothing -> ("", "") -- neniam
                Just v  -> v
                ) $ filter (\mb -> case mb of
                        Nothing -> False
                        Just _  -> True
                    ) mbDokumentoj
    return $ dokumentoj

simpligiRetpagxon :: PorLegilo -> Babili -> BildLargxo -> Ligilo -> IO (Maybe (Titolo, Teksto))
simpligiRetpagxon porLegilo babilu bildLargxo retpagxo = do
    retpagxoSimpligita <- S.simpligiRetpagxon retpagxo porLegilo bildLargxo babilu
    case retpagxoSimpligita of
        Left mesagxo -> do
            putStrLn mesagxo
            return $ Nothing
        Right (titolo, teksto) -> do
            return $ Just (titolo, teksto)

sercxiDosieron :: FilePath -> IO (Titolo, Teksto)
sercxiDosieron fp = do
    datumoj <- readFile fp
    return $ (dropEnd 5 $ pack fp, pack datumoj)

sercxiDosierojn :: Babili -> Int -> IO ([(Titolo, Teksto)])
sercxiDosierojn babilu kvantoDeDosiero = do
    tie <- getCurrentDirectory
    seSkribu babilu $ tie
    cxioj <- listDirectory tie
    cxiojHtml <- pure $ filter (\n -> isSuffixOf ".html" n) cxioj
    mapM sercxiDosieron $ take kvantoDeDosiero cxiojHtml


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


havigiParametrojnProgramo :: Babili -> Maybe Retadreso -> PorLegilo -> BildLargxo -> Int -> FariDosiero -> FluaDemando -> DosierejoDemando -> [Ligilo] -> RSSalRetposxtoProgramo ()
havigiParametrojnProgramo babilu mbRetadreso porLegilo bildLargxo kvantoDeDosiero faruDosiero fluaDemando dosierejoDemando ligiloj =
  case (null ligiloj, fluaDemando, dosierejoDemando) of
    (True, True, False) ->
      if faruDosiero then
        LegiFluojn babilu porLegilo bildLargxo (devuSkribiDosierojnElFluojnProgramo babilu (majluDokumentojnElFluojnProgramo mbRetadreso babilu porLegilo True (skribuFluojnProgramo babilu)))
      else LegiFluojn babilu porLegilo bildLargxo (majluDokumentojnElFluojnProgramo mbRetadreso babilu porLegilo False (skribuFluojnProgramo babilu))
    (True, False, True) -> LegiFluojnElDosierejo babilu kvantoDeDosiero (majluDokumentojnProgramo mbRetadreso babilu porLegilo (malAperiguDosierojnProgramo babilu))
    (True, False, False) -> NeParametron ()
    (False, True, _) -> AldoniFluojn ligiloj babilu (Fino ())
    (False, False, _) ->
      if faruDosiero then
        LegiFluojnElLigilojn ligiloj babilu porLegilo bildLargxo (devuSkribiDosierojnProgramo babilu (majluDokumentojnProgramo mbRetadreso babilu porLegilo (finiguProgramo babilu)))
      else LegiFluojnElLigilojn ligiloj babilu porLegilo bildLargxo (devuAfisxiDosierojnProgramo babilu (majluDokumentojnProgramo mbRetadreso babilu porLegilo (finiguProgramo babilu)))
    _ -> Fino ()


devuSkribiDosierojnProgramo :: Babili -> ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()) -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
devuSkribiDosierojnProgramo babilu sekvo dokumentoj =
  SkribiDosierojn babilu dokumentoj (sekvo dokumentoj)

devuAfisxiDosierojnProgramo :: Babili -> ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()) -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
devuAfisxiDosierojnProgramo babilu sekvo dokumentoj =
  AfisxiDosierojn babilu dokumentoj (sekvo dokumentoj)

majluDokumentojnProgramo :: Maybe Retadreso -> Babili -> PorLegilo -> ([(Titolo, Teksto)] -> Farita -> RSSalRetposxtoProgramo ()) -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
majluDokumentojnProgramo mbRetadreso babilu porLegilo sekvo dokumentoj =
  case mbRetadreso of
    Just retadreso -> ElsendiMajlon retadreso babilu porLegilo dokumentoj (sekvo dokumentoj)
    Nothing        -> sekvo dokumentoj False

finiguProgramo :: Babili -> [(Titolo, Teksto)] -> Farita -> RSSalRetposxtoProgramo ()
finiguProgramo _ _ _ = Fino ()

malAperiguDosierojnProgramo :: Babili -> [(Titolo, Teksto)] -> Farita -> RSSalRetposxtoProgramo ()
malAperiguDosierojnProgramo babilu dokumentoj farita =
  if farita then MalAperigiDosierojn (map fst dokumentoj) babilu (Fino ())
  else Fino ()


devuSkribiDosierojnElFluojnProgramo :: Babili -> ([(Titolo, Teksto)] -> Fluojn -> RSSalRetposxtoProgramo ()) -> [(Titolo, Teksto)] -> Fluojn -> RSSalRetposxtoProgramo ()
devuSkribiDosierojnElFluojnProgramo babilu sekvo dokumentoj fluojn =
  SkribiDosierojn babilu dokumentoj (sekvo dokumentoj fluojn)

majluDokumentojnElFluojnProgramo :: Maybe Retadreso -> Babili -> PorLegilo -> Bool -> (Fluojn -> Farita -> RSSalRetposxtoProgramo ()) -> [(Titolo, Teksto)] -> Fluojn -> RSSalRetposxtoProgramo ()
majluDokumentojnElFluojnProgramo mbRetadreso babilu porLegilo deviguSkribiFluojn sekvo dokumentoj fluojn =
  case mbRetadreso of
    Just retadreso -> ElsendiMajlon retadreso babilu porLegilo dokumentoj (sekvo fluojn)
    Nothing        -> sekvo fluojn deviguSkribiFluojn

skribuFluojnProgramo :: Babili -> Fluojn -> Farita -> RSSalRetposxtoProgramo ()
skribuFluojnProgramo babilu fluojn farita =
  if farita then SkribiFluojn fluojn babilu (Fino ())
  else Fino ()


main :: IO ()
main = do
    prgNomo <- getProgName
    (args, ligiloj) <- getArgs >>= parse prgNomo

    interpreti (HavigiParametrojn args ligiloj havigiParametrojnProgramo)














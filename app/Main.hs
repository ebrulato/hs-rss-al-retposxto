{-# LANGUAGE DeriveFunctor     #-}
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
  | LegiFluojnElLigiloj [Ligilo] Babili PorLegilo BildLargxo ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo a)
  | LegiFluojnElDosierejo Babili Int ([(Titolo, Teksto)] -> RSSalRetposxtoProgramo a)
  | ElsendiMajlon Retadreso Babili PorLegilo [(Titolo, Teksto)] (Farita -> RSSalRetposxtoProgramo a)
  | AfisxiDosierojn Babili [(Titolo, Teksto)] (RSSalRetposxtoProgramo a)
  | SkribiDosierojn Babili [(Titolo, Teksto)] (RSSalRetposxtoProgramo a)
  | SkribiFluojn Fluojn Babili (RSSalRetposxtoProgramo a)
  | MalAperigiDosierojn [Titolo] Babili (RSSalRetposxtoProgramo a)
  | NeParametron a
  | Fino a
  deriving Functor

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

interpreti (LegiFluojnElLigiloj ligilojn babilu porLegilo bildLargxo sekvo) = do
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
    rezulto <- try $ S.simpligiRetpagxon retpagxo porLegilo bildLargxo babilu
    case (rezulto :: (Either SomeException (Either String (Text, Text)))) of
        Right retpagxoSimpligita ->
          case retpagxoSimpligita of
            Left mesagxo -> do
              putStrLn mesagxo
              return $ Nothing
            Right (titolo, teksto) -> do
              return $ Just (titolo, teksto)
        Left eraro  -> do
            putStrLn $ show eraro
            return $ Nothing

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



kajTiam :: RSSalRetposxtoProgramo a -> (a -> RSSalRetposxtoProgramo b) -> RSSalRetposxtoProgramo b

kajTiam (HavigiParametrojn flagoj ligiloj sekvo) fariProgramo = HavigiParametrojn flagoj ligiloj (\babilu mbRetadreso porLegilo bildLargxo kvantoDeDosiero faruDosiero fluaDemando dosierejoDemando ligiloj -> kajTiam (sekvo babilu mbRetadreso porLegilo bildLargxo kvantoDeDosiero faruDosiero fluaDemando dosierejoDemando ligiloj) fariProgramo)
kajTiam (AldoniFluojn ligiloj babilu sekvo) fariProgramo = AldoniFluojn ligiloj babilu (kajTiam sekvo fariProgramo)
kajTiam (LegiFluojn babilu porLegilo bildLargxo sekvo) fariProgramo = LegiFluojn babilu porLegilo bildLargxo (\dokumentoj fluojn -> kajTiam (sekvo dokumentoj fluojn) fariProgramo)
kajTiam (LegiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo sekvo) fariProgramo = LegiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo (\dokumentoj -> kajTiam (sekvo dokumentoj) fariProgramo)
kajTiam (LegiFluojnElDosierejo babilu kvantoDeDosiero sekvo) fariProgramo = LegiFluojnElDosierejo babilu kvantoDeDosiero (\dokumentoj -> kajTiam (sekvo dokumentoj) fariProgramo)
kajTiam (ElsendiMajlon retadreso babilu porLegilo dokumentoj sekvo) fariProgramo = ElsendiMajlon retadreso babilu porLegilo dokumentoj (\farita -> kajTiam (sekvo farita) fariProgramo)
kajTiam (AfisxiDosierojn babilu dokumentoj sekvo) fariProgramo = AfisxiDosierojn babilu dokumentoj (kajTiam sekvo fariProgramo)
kajTiam (SkribiDosierojn babilu dokumentoj sekvo) fariProgramo = SkribiDosierojn babilu dokumentoj (kajTiam sekvo fariProgramo)
kajTiam (SkribiFluojn fluojn babilu sekvo) fariProgramo = SkribiFluojn fluojn babilu (kajTiam sekvo fariProgramo)
kajTiam (MalAperigiDosierojn titoloj babilu sekvo) fariProgramo = MalAperigiDosierojn titoloj babilu (kajTiam sekvo fariProgramo)
kajTiam (NeParametron respondo) fariProgramo = fariProgramo respondo
kajTiam (Fino respondo) fariProgramo = fariProgramo respondo

instance Applicative RSSalRetposxtoProgramo where
  pure = Fino
  (<*>) = (\(Fino f) (Fino a) -> Fino (f a))

instance Monad RSSalRetposxtoProgramo where
  return = Fino
  (>>=) = kajTiam


havigiParametrojn :: Babili -> Maybe Retadreso -> PorLegilo -> BildLargxo -> Int -> FariDosiero -> FluaDemando -> DosierejoDemando -> [Ligilo] -> RSSalRetposxtoProgramo ()
havigiParametrojn babilu mbRetadreso porLegilo bildLargxo kvantoDeDosiero faruDosiero fluaDemando dosierejoDemando ligiloj =
  case (null ligiloj, fluaDemando, dosierejoDemando) of
    (True, True, False) ->
      if faruDosiero then do
        (dokumentoj, fluojn) <- legiFluojn babilu porLegilo bildLargxo
        skribiDosierojn babilu dokumentoj
        majliDokumentojn mbRetadreso babilu porLegilo dokumentoj
        skribiFluojn babilu fluojn
      else do
        (dokumentoj, fluojn) <- legiFluojn babilu porLegilo bildLargxo
        farita <- majliDokumentojn mbRetadreso babilu porLegilo dokumentoj
        if farita then skribiFluojn babilu fluojn
        else finigiProgramon
    (True, False, True) -> do
      dokumentoj <- legiFluojnElDosierejo babilu kvantoDeDosiero
      farita <- majliDokumentojn mbRetadreso babilu porLegilo dokumentoj
      if farita then malAperigiDosierojn babilu dokumentoj
      else finigiProgramon
    (True, False, False) -> NeParametron ()
    (False, True, _) -> AldoniFluojn ligiloj babilu (Fino ())
    (False, False, _) -> do
      if faruDosiero then do
        dokumentoj <- legiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo
        skribiDosierojn babilu dokumentoj
        majliDokumentojn mbRetadreso babilu porLegilo dokumentoj
      else do
        dokumentoj <- legiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo
        afisxiDosierojn babilu dokumentoj
        majliDokumentojn mbRetadreso babilu porLegilo dokumentoj
      finigiProgramon
    _ -> Fino ()


legiFluojn :: Babili -> PorLegilo -> BildLargxo -> RSSalRetposxtoProgramo ([(Titolo, Teksto)], Fluojn)
legiFluojn babilu porLegilo bildLargxo =
  LegiFluojn babilu porLegilo bildLargxo (\dokumentoj fluojn -> Fino (dokumentoj, fluojn))

legiFluojnElDosierejo :: Babili -> Int -> RSSalRetposxtoProgramo [(Titolo, Teksto)]
legiFluojnElDosierejo babilu kvantoDeDosiero =
  LegiFluojnElDosierejo babilu kvantoDeDosiero (\dokumentoj -> Fino dokumentoj)

legiFluojnElLigiloj :: [Ligilo] -> Babili -> PorLegilo -> BildLargxo -> RSSalRetposxtoProgramo [(Titolo, Teksto)]
legiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo =
  LegiFluojnElLigiloj ligiloj babilu porLegilo bildLargxo (\dokumentoj -> Fino dokumentoj)

skribiDosierojn :: Babili -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
skribiDosierojn babilu dokumentoj =
  SkribiDosierojn babilu dokumentoj (Fino ())

majliDokumentojn :: Maybe Retadreso -> Babili -> PorLegilo -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo Farita
majliDokumentojn mbRetadreso babilu porLegilo dokumentoj =
  case mbRetadreso of
    Just retadreso -> ElsendiMajlon retadreso babilu porLegilo dokumentoj (\farita -> Fino farita)
    Nothing        -> Fino False

skribiFluojn :: Babili -> Fluojn -> RSSalRetposxtoProgramo ()
skribiFluojn babilu fluojn =
  SkribiFluojn fluojn babilu (Fino ())

afisxiDosierojn :: Babili -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
afisxiDosierojn babilu dokumentoj =
  AfisxiDosierojn babilu dokumentoj (Fino ())

finigiProgramon :: RSSalRetposxtoProgramo ()
finigiProgramon = Fino ()

malAperigiDosierojn :: Babili -> [(Titolo, Teksto)] -> RSSalRetposxtoProgramo ()
malAperigiDosierojn babilu dokumentoj =
  MalAperigiDosierojn (map fst dokumentoj) babilu (Fino ())

main :: IO ()
main = do
    prgNomo <- getProgName
    (args, ligiloj) <- getArgs >>= parse prgNomo
    interpreti (HavigiParametrojn args ligiloj havigiParametrojn)














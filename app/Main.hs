module Main where

import qualified Data.Text.IO                  as T     (putStrLn, writeFile) -- ?
import           Data.Text                              (Text)
import           Simpligi
import           Data.List
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read
import           Versio
import           Retposxto

retposxtaServo = "SMTP_SERVER"
retposxtaSalutnomo = "SMTP_LOGIN"
retposxtaPasvorto = "SMTP_PASSWORD"

data Flag
        = Legilo                -- -l --legilo
        | Dosiero               -- -d --dosiero
        | Largxo String         --    --largxo
        | Retadreso String      --    --retadreso
        | Help                  -- -h --helpo
        | Versio                -- -v -vervio
        deriving (Show, Eq)


flagoj =
       [Option ['l']    ["legilo"]      (NoArg Legilo)                          "La simpligita retpaĝo kunhavas ĝia(j)n bildon(j)n tiel ĝi legeblas legile (Kindleq, ...)"
       ,Option ['d']    ["dosiero"]     (NoArg Dosiero)                         "Skribas la simpligita retpaĝo en dosiero nomigita kun la titolo de la artikolo"
       ,Option []       ["largxo"]      (ReqArg Largxo "ek. 400")               "Se vi uzas -l/--legilo parametro, la largxo definas la dimensiojn de la bildojn. La defauxltvalo estas 400 rastrumeroj."
       ,Option []       ["retadreso"]   (ReqArg Retadreso "ek. nomo@servo.eo")  "Se vi uzas -l/--legilo parametro, la artikolo inkluzivigitos je la emajlo kiel dosiero."
       ,Option ['h']    ["helpo"]       (NoArg Help)                            "Afisxas tiun cxi mesagxon por helpi vin"
       ,Option ['v']    ["versio"]      (NoArg Versio)                          "Afisxas la version de tiu cxi ilo"
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

        where header prgName = "Uzado: "++ prgNomo ++ " [-ldhv] <La retpagxoj kiu estos simpligata> \n\n"

sercxiFlago :: (Flag -> Maybe a) -> [Flag] -> Maybe a
sercxiFlago f [] = Nothing
sercxiFlago f (flago:flagoj) = 
    case f flago of 
        Just s -> Just s 
        Nothing -> sercxiFlago f flagoj

sercxiLargxo :: Int -> [Flag] -> Int 
sercxiLargxo defauxlto flagoj = 
    let 
        valo = sercxiFlago (\n -> case n of 
                        Largxo v -> readMaybe v   
                        _ -> Nothing ) flagoj
    in
        case valo of 
            Just v -> if v < 10 then defauxlto else v
            Nothing -> defauxlto

sercxiRetadreso :: [Flag] -> Maybe String
sercxiRetadreso = sercxiFlago (\n -> case n of 
                        Retadreso v -> Just v
                        _ -> Nothing ) 

fariDosiero :: Maybe ([Flag], String, Text) -> IO (Maybe ([Flag], String, Text))
fariDosiero Nothing = return Nothing
fariDosiero (Just (args, titolo, teksto)) =
    if (Dosiero `elem` args) then do
        T.writeFile (titolo ++ ".html") teksto
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


fariRetadreso :: Maybe ([Flag], String, Text) -> IO (Maybe ([Flag], String, Text))
fariRetadreso Nothing = return Nothing
fariRetadreso (Just (args, titolo, teksto)) = do
        mbRetposxto <- sercxiRetposxto
        case (sercxiRetadreso args, mbRetposxto) of
            (Nothing, _) -> return $ Just (args, titolo, teksto)
            (Just retadreso, Just retposxto) -> do
                rezulto <- sendiMajlon retposxto retadreso titolo teksto (Legilo `elem` args)
                case rezulto of 
                    Left mesagxo -> do 
                        putStrLn $ "eraro : " ++ mesagxo
                        return $ Just (args, titolo, teksto)
                    Right _ -> return $ Just (args, titolo, teksto)
            (Just retadreso, Nothing) -> do
                putStrLn $ "eraro : vi deklaru SMTP-on"
                return $ Just (args, titolo, teksto)
                
main :: IO ()
main = do
    prgNomo <- getProgName
    (args, retpagxoj) <- getArgs >>= parse prgNomo
    retpagxoSimpligita <- simpligiRetpagxon (head retpagxoj) (Legilo `elem` args) (sercxiLargxo 400 args)
    case retpagxoSimpligita of 
        Left mesagxo -> putStrLn mesagxo
        Right (titolo, teksto) -> do 
            fariDosiero (Just (args, titolo, teksto)) 
                >>= fariRetadreso  
            return ()

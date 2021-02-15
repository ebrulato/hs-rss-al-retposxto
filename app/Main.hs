module Main where

import qualified Data.Text.IO                  as T   (putStrLn, writeFile) -- ?
import           Simpligi
import           Data.List
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read
import           Versio

data Flag
        = Legilo                -- -l --legilo
        | Dosiero               -- -d --dosiero
        | Largxo String         --    --largxo
        | Help                  -- -h --helpo
        | Versio                -- -v -vervio
        deriving (Show, Eq)


flags =
       [Option ['l']    ["legilo"]   (NoArg Legilo)         "La simpligita retpagxo kunhavas gxia(j)n bildon(j)n tiel gxi legeblas legile (Kindle)"
       ,Option ['d']    ["dosiero"]  (NoArg Dosiero)        "Skribas la simpligita retpagxo en dosiero nomigita kun la titolo de la artikolo"
       ,Option []       ["largxo"]   (ReqArg Largxo "400")  "Se vi uzas -l/--legilo parametro, la largxo definas la dimensiojn de la bildojn. La defauxltvalo estas 400 rastrumeroj."
       ,Option ['h']    ["helpo"]    (NoArg Help)           "Afisxas tiun cxi mesagxon por helpi vin"
       ,Option ['v']    ["versio"]   (NoArg Versio)         "Afisxas la version de tiu cxi ilo"
       ]

parse prgNomo argv = case getOpt Permute flags argv of

        (args,w,[]) -> do
            let words = if null w then [] else w
            if Help `elem` args
            then do hPutStrLn stderr (usageInfo (header prgNomo) flags)
                    exitWith ExitSuccess
            else if Versio `elem` args 
            then do hPutStrLn stdout (prgNomo ++ " " ++ Versio.versio)
                    exitWith ExitSuccess
            else return (nub args, words)

        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo (header prgNomo) flags)
            exitWith (ExitFailure 1)

        where header prgName = "Uzado: "++ prgNomo ++ " [-ldhv] <La retpagxoj kiu estos simpligata> \n\n"

sercxiFlago :: (Flag -> Maybe Int) -> [Flag] -> Maybe Int
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
            Just v -> v
            Nothing -> defauxlto


main :: IO ()
main = do
    prgNomo <- getProgName
    (args, retpagxoj) <- getArgs >>= parse prgNomo
    retpagxoSimpligita <- simpligiRetpagxon (head retpagxoj) (Legilo `elem` args) (sercxiLargxo 400 args)
    case retpagxoSimpligita of 
        Left mesagxo -> putStrLn mesagxo
        Right (titolo, teksto) -> 
            if (Dosiero `elem` args) then
                T.writeFile (titolo ++ ".html") teksto
            else 
                T.putStrLn teksto 

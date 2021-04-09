# hs-rss-al-retposxto

# Projekto RSS al retpoŝto

La celo de tiu projekto estas la kreado de ilo kiu povas ekstrakti retpaĝojn kaj simpligi ilin.
Ĉi tiu ilo eblas elsendi ilin retpoŝten, tiel ke vi povas legi viajn ŝatitajn blogojn facile en via majllegilo. 
Se vi volas vi povas inkluzivi la bildojn en la artikoloj kaj elsendi la majlojn al via ŝatita legilo tiel Kindle.

## Konstrui kaj Installi la programo

> $> stack build

> $> stack exec hs-rss-al-retposxto-exe

### Ubuntu / Raspberry Pi 4

> $> cabal build

Verŝajne vi povas konstrui la programon, sed vi devas aldoni pli da 8Gb de labormemoro.
Mi uzas Raspberry Pi 4 kun 8Gb labormemoro kaj 2Gb da virtuala labormemoro.

> $> cabal install --overwrite-policy=always

## kion vi povas faris 

- simpligi retpaĝojn
- surdiskigi simpligitajn retpaĝojn
- elsendi simpligitjan retpaĝojn majlen
- elsendi la surdiskigitajn retpaĝojn majlen.
- uzi TLS servo kiel gmx.com por elsendi la majlojn
- la nomo de la dosiero en la majloj korektitas, sed ĉi tiu majlsistemo uzas nur usonajn signojn. (farigi x-sistemon por la esperantaj titoloj) 
- legi RSS2
- legi Mediun
- legi simplan blogon 

## kion mi devas korekti

- mi devas uzi la maldiligentajn versiojn de la tipoj se eblas. 
- se la ligiloj estas en CDATA, la ilo ne povas uzi korekte ilin.
- mi ne povas uzi HaskellNet-SSL ;( 

## kodplibonigoj

- mi simpligis la uzadon de la parametroj « app/Main.hs »
- mi rekodis la « app/Main.hs » kun la skemo komando kaj aldonis novajn tipojn por faciligi la kodlegado 
- mi aldonis la « kajTiam » skemon por plibonigi la legadon de la funkcio « havigiParametrojnProgramo » en la dosiero « app/Main.hs »
- mi deklaris la komandtipon kiel Monado. La legado de la funkcio « haviParametrojn » tre pliboniĝis. Oni povas diri ke ni havas krei novan programlingvon surbazitan la ĉefaj funkciaĵoj de nia bezono. 


## la bazoj de la projekto

Mi lernas kiel funkcias : 

- html-parse
- JuicyPixels
- smtp-mail
- Monadojn

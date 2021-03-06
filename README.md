# hs-rss-al-retposxto

# Projekto RSS al retpoŝto

La celo de tiu projekto estas la kreado de ilo kiu povas ekstrakti retpaĝojn kaj simpligi ilin.
Ĉi tiu ilo eblas elsendi ilin retpoŝten, tiel ke vi povas legi viajn ŝatitajn blogojn facile en via majllegilo. 
Se vi volas vi povas inkluzivi la bildojn en la artikoloj kaj elsendi la majlojn al via ŝatita legilo tiel la Kindle.

> $> stack build

> $> stack exec hs-rss-al-retposxto-exe

## kion vi povas faris 

- simpligi retpaĝojn
- surdiskigi simpligitajn retpaĝojn
- elsendi simpligitjan retpaĝojn majlen
- uzi TLS servo kiel gmx.com
- la nomo de la dosiero en la majloj korektitas, sed ĉi tiu majlsistemo uzas nur usonajn signojn. 
- uzi RSS2
- uzi Mediun

## kion mi devas korekti

- mi devas uzi la maldiligentajn versiojn de la tipoj se eblas. 
- se la ligiloj estas en CDATA, la ilo ne povas uzi korekte ilin.
- mi ne povas uzi HaskellNet-SSL ;( 

## la bazoj de la projekto

Mi lernas kiel funkcias : 

- html-parse
- JuicyPixels
- smtp-mail

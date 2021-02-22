# hs-rss-al-retposxto

# Projekto RSS al retpoŝto

La celo de tiu projekto estas la kreo de ilo kiu povas ekstrakti retpagxon, simpligi gxin kaj sendas gxin retposxten, 
tiel vi povas legi viajn ŝatitajn blogojn. 
Se vi volas vi povas inkluzivi la bildojn en la artikoloj kaj redirekti la retposxtojn al via ŝatita legilo.

> $> stack build

> $> stack exec hs-rss-al-retposxto-exe

## kion vi povas fari

- simpligi retpaĝojn
- surdiskigi simpligitan retpaĝon
- elsendi simpligitan retpaĝon majlen
- elsendi (kelkfoje) simpligitan retpaĝon al Kindel-o
- uzi TLS servo kiel gmx.com por la majloj
- la nomo de la dosiero en la majloj korektitas, sed ĉi tiu majlsistemo uzas nur usonajn signojn. 

## kion mi devas korekti

- mi devas uzi la maldiligentajn versiojn de la tipoj se eblas. 
- la simpligo de la retpaĝoj el Medium

## la bazoj de la projekto

Mi lernas kiel funkcias : 

- html-parse
- JuicyPixels
- smtp-mail

# KRWQCprotocol - R package voor QC controle

<!-- badges: start -->
[![R-CMD-check](https://github.com/jspijker/KRWQCprotocol/workflows/R-CMD-check/badge.svg)](https://github.com/jspijker/KRWQCprotocol/actions)
<!-- badges: end -->


Het QC protocol beschrijft de werkwijze om grondwaterkwaliteitsgegevens
te controleren en beoordelen en het protocol wordt door
provincies en RIVM gebruikt. Het protocol beschrijft een een uniforme aanpak geborgd
door de verschillende bronhouders alvorens de kwaliteitsgegevens worden
opgenomen in de Basisregistratie Ondergrond (BRO).  

Het QC protocol kan
ook worden gehanteerd voor de controle en beoordeling van de historische
kwaliteitsgegevens.  De bestaande werkwijze voor controle en beoordeling
van grondwaterkwaliteitsgegevens wordt beschreven in het Handboek
Monitoring Grondwaterkwaliteit KRW. Deze werkwijze is ontwikkeld
door het RIVM en vanaf de inrichting van het LMG gebruikt voor de
controle en beoordeling van de LMG-gegevens.

Het QC protocol bestaat uit verschillende tests die deels ook
aansluiten op de vereisten vanuit de BRO Het zo veel als mogelijk
automatiseren van de werkwijze voor controle en beoordeling van
grondwaterkwaliteitsgegevens, leidt op een efficiente manier tot een
uniforme en eenduidige dataset tussen de verschillende bronhouders. Dit
R package bevat een groot aantal tests volgens het QC prototol

# Documentatie & help pagina's

De documentatie en help pagina's van dit pakket kan je [hier](https://jspijker.github.io/KRWQCprotocol/index.html) vinden.



# Brongegevens / invoerbestand

In het QC protocol worden controles uitgevoerd m.b.t. parameters, meettechnieken, 
observaties en meetwaardes uit zowel het veld als uit het lab. Door de veelvoud 
aan controles is het complex om de vele gegevens in één bestand te vatten. Om 
deze reden is gekozen om de datastromen in 5 datasets op te knippen zodat het 
automatiseren van de controles eenvoudiger wordt. De datasets zijn als volgt:

1) Veldbestand: databestand met veldobservaties betreffende locatie en toestand van de put;
2) Putbestand: databestand met referentie putlocatie gegevens;
3) Filterbestand: databestand met referentie putfilter gegevens;
4) Parameterbestand: opzoekbestand voor parameter kenmerken;
5) Metingenbestand: bestand met gemeten parameters uit monsteranalyses en veldmetingen.

Zie de vignette `krwqcprotocol` voor beschrijving van de tabel inhoud. 

# Resultaat / uitvoerbestand

In de verschillende QC functies worden de testen uitgevoerd beschreven in het
QC Protocol. Als bij deze stappen een afwijking geconstateerd wordt dan krijgt
het monster, de parameter of de waarde een concept oordeel toegekend. Deze 
afwijkingen worden aan het eind van de functie als attribuut meegegeven aan 
het metingen bestand (`d_metingen`) en kunnen dus na het runnen van elke test
bekeken worden. 

*BELANGRIJK:* alle resultaten en concept oordelen worden per QC-test als attribuut 
aan het metingenbestand gehangen. Het is daarom noodzakelijk om voor elke test 
hetzelfde bestand als invoer te gebruiken, zodat alle test resultaten uiteindelijk 
als attribuut beschikbaar zijn onder eenzelfde bestand. 

# Licentie

Dit pakket wordt beschikbaar gesteld onder een GPL-3 Open Source
licentie, zie LICENCE.md

# Contact

Voor vragen en/of opmerkingen kan een _issue_ aagemaakt worden op
GitHub. 

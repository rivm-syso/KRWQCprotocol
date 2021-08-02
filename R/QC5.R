#' QC5. Status
#'
#' In QC5 wordt aan de metingen een QC status toegekend.
#'      
#' Bij elke individuele meting van een parameter in het grondwatersamenstellings-
#' onderzoek geeft de bronhouder (of een externe partij in opdracht van de 
#' brondhouder) een eindoordeel over de kwaliteit van de meting. Dit eindoordeel
#' wordt gevormd aan de hand van een voor het hele grondwatersamenstellings-
#' onderzoek gebruikte beoordelingsprocedure. Het eindoordeel wordt geregistreerd
#' in de status kwaliteitscontrole. Het is een oordeel over de kwaliteit van de
#' meting van de parameter, geen oordeel over het grondwatermonster als geheel.
#' 
#' Er wordt gebruik gemaakt van vier statusdefinities: goedgekeurd, afgekeurd,
#' onbekend en onbeslist. Zie QC5: Status van het QC protocol voor de methodiek.
#'                       
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met eindoordeel per meting.
#'
#' @export
#'


QC5 <- function(d_metingen, verbose = F) {
  
  # Warning om erop te wijzen dat alle QC resultaten als attribute aan
  # eenzelfde dataset moeten hangen
  # Als deze check binnen de functie wordt uitgevoerd kan deze weg
  warning(paste0("Controleer of alle QC functies met hetzelfde ",
                 "metingen bestand zijn uitgevoerd en dat voor iedere stap ",
                 "attributes zijn toegevoegd."))
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Verzamel de testresultaten uit een data.frame met tests. De
  # testresultaten worden per regel in de data.frame aangegeven.
  d <- collect_result(d_metingen)
  
  # Check of alle QC stappen zijn uitgevoerd?
  #
  
  # Hier wordt het QC eindoordeel toegevoegd per meting
  # Hier moeten de regels uit QC Protocol - QC5 nog geautomatiseerd uitgevoerd
  # worden
  #
  
  # goedgekeurd
  
  # afgekeurd
  
  # onbekend
  
  # onbeslist
  
  
  # Tijdelijke warning dat deze functie nog niet compleet is
  warning(paste0("Deze functie is nog niet compleet. ",
                 "De QC5 regels voor het bepalen van het eindoordeel zijn ",
                 "nog niet geautomatiseerd binnen deze functie. ",
                 "De output dataset kan worden gebruikt om deze ",
                 "regels verder toe te passen."))
  
  
  return(d)
  
}


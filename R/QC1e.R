#' QC1e. Controle ontbreken analysegegevens
#'
#' Controle op aanwezigheid van analysegegevens voor alle genomen monsters
#'
#' Controleer of ieder monster in het bestand meetwaarden bevat van de 
#' parameters die zijn uitgevraagd bij het laboratorium. Indien dit niet
#' het geval is, ken het concept oordeel ontbrekend toe aan het monster.
#' 
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` blijkt of de locatie/monster 'onverdacht' of 'verdacht' is.
#'
#' @export
#'


QC1e <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Controle op aanwezigheid van analysegegevens
  d <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::summarise(aantal.afwezig = length(which(is.na(waarde)))) %>%
    dplyr::filter(aantal.afwezig > 0)
  
  # Voeg oordeel toe per monster als een meting ontbreekt
  resultaat_df <- d_metingen %>%
    dplyr::filter(monsterid %in% d$monsterid) %>%
    dplyr::mutate(oordeel = "ontbrekend",
           reden = "afwezigheid meetwaarden bij een parameter in dit monster")

  rapportageTekst <- paste("Er zijn in totaal", 
                           resultaat_df$monsterid %>% 
                               dplyr::n_distinct(), 
                           "monsters waar een meting ontbreekt")
  
  if(verbose) {
    if(nrow(resultaat_df) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er ontbreken geen analysegegevens"))
    }
  }

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid 
  test <- "QC1e"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "ontbrekend",
                                  ids = verdacht_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}


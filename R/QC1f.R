#' QC1f. Controle negatieve waarden
#'
#' Controle op aanwezigheid van negatieve waarden
#'
#' Indien de waarde negatief is, ken het concept
#' oordeel verdacht toe aan de betreffende parameter(s).
#' Met uitzondering van de temperatuur
#' 
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` blijkt of de parameter 'onverdacht' of 'verdacht' is.
#'
#' @export
#'


QC1f <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Controle op aanwezigheid negatieve waardes
  resultaat_df <- d_metingen %>%
    # met uitzondering van temperatuur
    dplyr::filter(parameter != "temperatuur") %>%
    dplyr::mutate(oordeel = ifelse(waarde < 0, "verdacht", 
                                   "onverdacht")) %>%
    dplyr::mutate(reden = ifelse(oordeel == "verdacht", 
                                 "negatieve waarde", "")) %>%
    dplyr::filter(oordeel == "verdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           nrow(resultaat_df), 
                           "metingen met een negatieve waarde,",
                           "verdeeld over", dplyr::n_distinct(resultaat_df$parameter),
                           "parameters.")
  
  if(verbose) {
    if(nrow(resultaat_df) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er zijn geen metingen met een negatieve waarde"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid 
  test <- "QC1f"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "verdacht",
                                  ids = verdacht_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}


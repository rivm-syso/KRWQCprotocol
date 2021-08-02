#' QC3a. Controle datum veldonderzoek voor labanalyse
#'
#' Controle op de plausibiliteit van de waarden van datum
#' veldonderzoek en datum labanalyse
#'
#' Ga na of de datum labanalyse na datum veldonderzoek ligt. 
#' Indien dit niet het geval is, ken het concept oordeel
#' verdacht toe aan:
#' - het gehele monster (in geval van afwijkingen bij meerdere 
#' parameters), of
#' - de betreffende parameter (in geval van afwijking bij
#' 1 parameter)
#'    
#' @param d_veld dataframe met veldobservaties   
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC3a <- function(d_veld, d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # creeer velddatum en koppel deze aan metingen bestand 
  d <- d_veld %>%
    dplyr::select(putcode, filter, jaar, maand, dag) %>%
    dplyr::mutate(velddatum = lubridate::make_date(jaar, maand, dag)) %>%
    dplyr::select(-c(maand, dag))
  
  res <- d_metingen %>%
    dplyr::select(-c(detectieteken, rapportagegrens, waarde)) %>%
    dplyr::mutate(labdatum = lubridate::make_date(jaar, maand, dag)) %>%
    dplyr::left_join(., d, by = c("jaar", "filter", "putcode"))  %>%
    dplyr::mutate(oordeel = ifelse(labdatum < velddatum,
                            "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht")

  rapportageTekst <- paste("Er zijn in totaal", nrow(res),
                           "metingen waar de labdatum niet na de velddatum komt")


  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)

    } else {
      print(paste("Alle labdatums zijn na de velddatums."))
    }
  }

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  resultaat_df <- res

  verdacht_id <- resultaat_df %>%
      dplyr::filter(oordeel == "verdacht") %>%
      dplyr::distinct(qcid) %>%
      dplyr::pull(qcid)
  test <- "QC3a"

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


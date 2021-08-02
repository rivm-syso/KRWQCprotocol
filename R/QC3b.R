#' QC3b. Controle registratie putfilter in de BRO
#'
#' Controleer of de betreffende putfilter is opgenomen in de BRO. 
#' 
#' Ga na of het BRO-ID van het putfilter voorkomt in de lijst met 
#' BRO-gerigistreerde putfilters. Als deze niet hierin voorkomt, ken
#' het concept oordeel verdacht toe aan het monster.
#'
#' @param d_filter dataframe met putfilter informatie
#' @param d_metingen metingen bestand met monster ID's om bij afwijkingen in de
#' putcode een oordeel toe te kennen.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#' 
#'
#' @export
#'

QC3b <- function(d_filter, d_metingen, verbose = F) {
  
  warning(paste0("Dit is een tijdelijke functie waarbij handmatige ",
                 "input vereist is van putcodes welke niet in de ", 
                 "BRO puttenlijst voorkomen, zie de handleiding."))
  
  # Test of relevante kolommen aanwezig zijn
  testKolommenFilter(d_filter)
  testKolommenMetingen(d_metingen)
  
  # voeg kolom oordeel toe
  # alle putfilters uit d_filter zijn nu al verdacht doordat tijdelijk
  # handmatige subselectie buiten de functie heeft plaatsgevonden 
  res <- d_filter %>%
    dplyr::mutate(oordeel = "verdacht") %>%
    dplyr::mutate(iden = paste(putcode, filter, sep = "-"))
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "putfilters welke BRO-ID's niet geregistreerd",
                           "zijn in de BRO.")
  
  # Als printen gewenst is (T)
  # Print preview van afwijkende XY-coordinaten 
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% select(putcode, filter, bro_id))
      
    } else {
      print("Elke putfilter is met BRO-ID geregisteerd in de BRO.")
    }
  }
  
  # voeg concept oordeel van afwijkende putfilters toe aan monsters op die 
  # locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, filter, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::select(-iden)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC3b"
  
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


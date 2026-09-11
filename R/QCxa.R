#' QCxa. Controle ontbreken filterlengte
#'
#' Controle op aanwezigheid van gemeten onder- en bovenkant van het betreffende 
#' filter.
#'
#' Controleer of bij ieder putfilter in het veld bestand een lengte voor de 
#' onder- en bovenkant van het filter is opgenomen.
#' Indien de lengte voor de onder- en/of bovenkant van het filter niet aanwezig 
#' is, ken het concept oordeel ontbrekend toe aan het monster.
#'  
#' @param d_veld dataframe met veldobservaties   
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` komt per monster te staan of de lengte van de onder- en/of bovenkant 
#' van het filter ontbreekt.
#'
#' @export
#'

QCxa <- function(d_veld, d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # Controle op aanwezigheid lengte onder- en bovenkant filter
  res <- d_veld %>%
    dplyr::select(monsterid, putcode, filter, okf, bkf) %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(afwezig = is.na(okf)|is.na(bkf)) %>%
    dplyr::filter(afwezig == TRUE)
  
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           res %>% nrow(),
                           "monsters waar de lengte van de onder- en/of bovenkant van het filter ontbreekt")
  
  # Printen monsterid indien gewenst
  if(verbose) {
    if(nrow(res) > 0 ) {
      rapportageTekst %>% write.table(., row.names = F, col.names = F)
      
      res %>% 
        dplyr::select(monsterid, putcode, filter, okf, bkf) %>% 
        print()
    } else {
      print(paste("De filterlengte is voor alle monsters aanwezig"))
    }
  }
  
  # Voeg oordeel toe per monster als een meting ontbreekt
  resultaat_df <- d_metingen %>%
    dplyr::filter(monsterid %in% res$monsterid) %>%
    dplyr::mutate(oordeel = "ontbrekend") %>% 
    dplyr::left_join(., res %>% dplyr::select(monsterid, okf, bkf), by = "monsterid")

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid 
  test <- "QCxa"
  
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


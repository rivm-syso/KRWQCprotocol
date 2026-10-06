#' QCxb. Controle plausibiliteit filterlengte
#'
#' Controle op een filterlengte van 2 meter.
#'
#' Controleer of het verschil tussen onderkant filter (okf) en 
#' bovenkant filter (bkf) tussen de 1.90 en 2.10 ligt.
#' Indien de lengte van het filter niet tussen de 1.90 en 2.10 ligt, ken het 
#' concept oordeel twijfelachtig toe aan het monster.
#'  
#' @param d_veld dataframe met veldobservaties   
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` komt per monster te staan of de grondwaterstand voor en/of na 
#' ontbreekt.
#'
#' @export
#'

QCxb <- function(d_veld, d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # Controle op grondwaterstanden voor en na het voorpompen
  res <- d_veld %>%
    tibble::tibble() %>% 
    dplyr::select(monsterid, putcode, filter, okf, bkf) %>%
    dplyr::mutate(filterlengte = okf - bkf,
                  oordeel = ifelse(
      # filterlengte = +/- 2.0 m
      filterlengte < 2.1 &
        filterlengte > 1.9 |
        filterlengte < -1.9 &
        filterlengte > -2.1,
      "onverdacht", "twijfelachtig")) %>%
    dplyr::filter(oordeel == "twijfelachtig")
  
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           res %>% nrow(),
                           "monsters waar de filterlengte niet binnen 10cm van 2 meter ligt")
  
  # Printen monsterid indien gewenst
  if(verbose) {
    if(nrow(res) > 0 ) {
      rapportageTekst %>% write.table(., row.names = F, col.names = F)
      
      res %>% 
        dplyr::select(monsterid, putcode, filter, okf, bkf) %>% 
        print()
    } else {
      print(paste("Alle filterlengtes liggen binnen 10 cm van 2 meter"))
    }
  }
  
  # Voeg oordeel toe per monster als een meting ontbreekt
  resultaat_df <- d_metingen %>%
    dplyr::filter(monsterid %in% res$monsterid) %>%
    dplyr::mutate(oordeel = "twijfelachtig") %>% 
    dplyr::left_join(., res %>% select(monsterid, okf, bkf, filterlengte), by = "monsterid")
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df$qcid 
  test <- "QCxb"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = twijfel_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}


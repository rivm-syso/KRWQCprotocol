#' QCxc. Controle duplo metingen
#'
#' Controle op verschil van 10% in duplo metingen.
#'
#' Controleer per stof per duplo meting of het verschil boven de 10% ligt.
#' Indien het verschil 10% of groter is, ken het concept oordeel twijfelachtig 
#' toe aan het monster.
#'  
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` komt per monsterid en stof te staan of de waarde afwijkt van de
#' duplo meting (indien aanwezig).
#'
#' @export
#'

QCxc <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Haal duplo's uit data
  res <- d_metingen %>% 
    dplyr::select(qcid, monsterid, jaar, maand, dag, filter, putcode, waarde, parameter) %>% 
    group_by(putcode, filter, jaar, maand, dag) %>% 
    filter(length(unique(monsterid)) == 2)
  
  if(d_metingen %>% 
     group_by(putcode, filter, jaar, maand, dag) %>% 
     filter(length(unique(monsterid)) > 2) %>% 
     nrow() > 0){
    print("triplo metingen en meer worden niet ondersteund")
  }
  
  # Controle op verschil van 10%
  res <- res %>% 
    group_by(parameter, .add = TRUE) %>% 
    dplyr::mutate(percentage_verschil = 200*(sum(waarde[monsterid == min(monsterid)] - waarde[monsterid == max(monsterid)]))/(sum(waarde)),
                  oordeel = case_when(percentage_verschil >= 10 ~ "twijfelachtig",
                                      percentage_verschil <= -10 ~ "twijfelachtig",
                                      TRUE ~ "onverdacht")) %>% 
    ungroup() %>% 
    dplyr::filter(oordeel == "twijfelachtig")
  
  # Controle op grondwaterstanden voor en na het voorpompen
  rapportageTekst <- paste("Er zijn in totaal", 
                           res %>% nrow(),
                           "stoffen waar het verschil niet binnen 10% ligt")
  
  # Printen monsterid indien gewenst
  if(verbose) {
    if(nrow(res) > 0 ) {
      rapportageTekst %>% write.table(., row.names = F, col.names = F)
      
      res %>% 
        dplyr::select(monsterid, putcode, filter, jaar, maand, dag, waarde, parameter, percentage_verschil, oordeel) %>% 
        print()
    } else {
      print(paste("Alle filterlengtes liggen binnen 10 cm van 2 meter"))
    }
  }
  
  # Voeg oordeel toe per monster als een meting ontbreekt
  resultaat_df <- d_metingen %>%
    dplyr::filter(monsterid %in% res$monsterid) %>%
    dplyr::mutate(oordeel = ifelse(qcid %in% res$qcid,
                                   "twijfelachtig", "onverdacht")) %>% 
    dplyr::left_join(., res %>% select(qcid, percentage_verschil), by = "qcid")
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df$qcid 
  test <- "QCxc"
  
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


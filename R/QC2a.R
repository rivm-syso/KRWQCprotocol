#' QC2a. Controle toestroming filter
#'
#' Controle op de toestroming van grondwater naar het filter
#'
#' Controleer of de grondwaterstand voor het voorpompen 
#' hoger is dan de grondwaterstand na het voorpompen.
#' Indien de grondwaterstand <10 cm hoger is na het voorpompen,
#' ken het concept oordeel twijfelachtig toe aan het monster.
#' Indien de grondwaterstand >10 cm hoger is na het voorpompen,
#' ken het concept oordeel verdacht toe aan het monster.
#'  
#' @param d_veld dataframe met veldobservaties   
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` blijkt of de locatie/monster 'onverdacht' of 'verdacht' is.
#'
#' @export
#'

QC2a <- function(d_veld, d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # Controle op grondwaterstanden voor en na het voorpompen
  res <- d_veld %>%
    dplyr::select(qcid, monsterid, putcode, filter, 
           gws_voor, gws_na) %>%
    dplyr::mutate(oordeel = ifelse(gws_na - gws_voor > 0 & gws_na - gws_voor < 0.1,
                            "twijfelachtig",
                            ifelse(gws_na - gws_voor >= 0.1, 
                                   "verdacht", "onverdacht")),
           iden = monsterid) %>%
    dplyr::mutate(reden = ifelse(oordeel == "twijfelachtig", "gws na voorpompen is <10 cm hoger",
                          ifelse(oordeel == "verdacht", "gws na voorpompen is >10 cm hoger", ""))) %>%
    dplyr::filter(oordeel != "onverdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           nrow(res %>% dplyr::filter(oordeel == "twijfelachtig")), 
                           "monsters waar de grondwaterstand na voorpompen <10 cm hoger is en",
                           nrow(res %>% dplyr::filter(oordeel == "verdacht")),
                           "monsters waar de grondwaterstand na voorpompen >10 cm hoger is")
  
  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("De grondwaterstand na voorpompen is altijd lager dan voor het voorpompen"))
    }
  }

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  resultaat_df <- d_metingen %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::filter(iden %in% res$iden) %>%
    dplyr::left_join(., res %>% dplyr::select(iden, gws_voor, gws_na, oordeel, reden), by = "iden") %>%
    dplyr::select(-iden)
  
  twijfel_id <- resultaat_df %>% 
      dplyr::filter(oordeel == "twijfelachtig") %>% 
      dplyr::distinct(qcid) %>%
      dplyr::pull(qcid)
  verdacht_id <- resultaat_df %>% 
      dplyr::filter(oordeel == "verdacht") %>% 
      dplyr::distinct(qcid) %>%
      dplyr::pull(qcid)

  test <- "QC2a"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = sort(unique(resultaat_df$oordeel))[1],
                                  ids = twijfel_id)
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = sort(unique(resultaat_df$oordeel))[2],
                                  ids = verdacht_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)

  return(d_metingen)

}


#' QC3f. Controle pH-veld en pH-lab
#'
#' Vergelijk pH-veld en pH-lab
#'
#' De signaleringswaarde voor monsters is delta-pH >= 2 pH-eenheden.
#' Als de delta-pH boven de signaleringswaarde ligt, ken het
#' concept QC oordeel twijfelachtig toe aan het monster.
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


QC3f <- function(d_veld, d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # pH naam aanpassen alleen voor LMG
  d <- d_metingen
  d$parameter <- d$parameter %>%
    dplyr::recode("h_5__veld" = "hv",
                  .default = d$parameter)
  
  # selecteer pH veld en lab gegevens
  # afhankelijk van de dataset kan dit 'pH' of 'zuurgraad' zijn
  d <- d %>%
    dplyr::filter(parameter %in% c("h", "hv"))
  
  # Check of pH veld en lab gegevens beschikbaar zijn
  if(dplyr::n_distinct(d$parameter) < 2) {
    stop("Geen veld of lab pH aanwezig")
  }
  if(dplyr::n_distinct(d$parameter) > 2) {
    stop("Meer dan 2 parameters voor veld en lab pH")
  }
  
  # Vergelijk pH lab en pH veld
  res <- d %>%
    dplyr::select(-c(qcid, detectieteken, rapportagegrens)) %>%
    tidyr::pivot_wider(names_from = parameter,
                       values_from = waarde) %>%
    dplyr::mutate(oordeel = ifelse(abs(h - hv) >= 2,
                                   "twijfelachtig", "onverdacht"),
                  iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::filter(oordeel != "onverdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "metingen waar pH-lab en pH-veld 2 pH-eenheden of meer afwijken")
  
  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er zijn geen metingen waar pH-lab en pH-veld 2 pH-eenheden of meer afwijken"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  cols <- c(1:5, 7, 6, 14, 15, 13)
  
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., res %>% select(h, hv, iden)) 
  resultaat_df <- resultaat_df[, cols]
  
  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid)
  
  test <- "QC3f"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = unique(resultaat_df$oordeel)[1],
                                  ids = twijfel_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}
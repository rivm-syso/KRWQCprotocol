#' QC3g. Controle pH en HCO3
#'
#' Controleer relatie pH en HCO3
#' 
#' Controleer per putfilter of aan de volgende condities wordt voldaan:
#' 1) pH <5 & HCO3 >15 mg/l
#' 2) pH <5.5 & HCO3 > 50 mg/l
#' 
#' Indien aan ten minste één van de condities wordt voldaan, ken het concept
#' QC oordeel twijfelachtig toe aan de betreffende parameters. Vergelijk 
#' veldwaarden enkel met veldwaarden en labwaarden enkel met labwaarden.
#'
#' @param d_metingen metingen bestand met monster ID's om bij afwijkingen 
#' te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#' 
#'
#' @export
#'

QC3g <- function(d_metingen, verbose = F) {
  
  # Test of relevante kolommen aanwezig zijn
  # deze hulpfuncties staan in utils, deze eerst nu nog runnen.
  testKolommenMetingen(d_metingen)
  
  # pH naam aanpassen alleen voor LMG
  d <- d_metingen %>%
    dplyr::filter(parameter != "hco3")
  d$parameter <- d$parameter %>%
    dplyr::recode("h_5__veld" = "hv",
                  # bij LMG wordt HCO3 in het veld bepaald, 
                  # maar nu vergelijken met pH lab dus andere naam
                  "hco3_veld" = "hco3",
                  .default = d$parameter)
  
  # In de meeste gevallen wordt HCO3 niet in het veld bepaald bij provincies, 
  # maar in het lab. Dus HCO3 (lab) vergelijken met pH-lab
  d <- d %>%
    dplyr::filter(parameter %in% c("h", "hco3")) 
  
  # Check of maar 1 voor pH en 1 voor HCO3 beschikbaar zijn
  if(dplyr::n_distinct(d$parameter) < 2) {
    stop("Geen pH of HCO3 beschikbaar")
  }
  if(dplyr::n_distinct(d$parameter) > 2) {
    stop("Meer dan 2 parameters voor pH en HCO3")
  }
  
  # Vergelijk pH lab HCO3 lab
  # namen nog onzeker vanuit provinciale dataset, mogelijk nog aanpassen!
  res <- d %>%
    dplyr::select(-c(qcid, detectieteken, rapportagegrens)) %>%
    tidyr::pivot_wider(names_from = parameter,
                       values_from = waarde) %>%
    dplyr::mutate(oordeel = ifelse(h < 5 & hco3 > 15 |
                                     h < 5.5 & hco3 > 50,
                                   "twijfelachtig", "onverdacht"),
                  iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::filter(oordeel != "onverdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "monsters waar de relatie tussen pH en HCO3",
                           "afwijkend is.")
  
  # Als printen gewenst is (T)
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% select(-iden))
      
    } else {
      print("Voor alle monsters klopt de relatie tussen pH en HCO3.")
    }
  }
  
  # voeg concept oordeel van afwijkende pH-HCO3 relatie toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, h, hco3)) %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter, 
                  h, hco3, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid)
  
  test <- "QC3g"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = unique(resultaat_df$oordeel),
                                  ids = twijfel_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
}
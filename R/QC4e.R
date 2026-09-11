#' QC4e. Controle pH-veld en pH-lab
#'
#' Vergelijk pH-veld en pH-lab
#'
#' De signaleringswaarde voor monsters is delta-pH >= 0.5 pH-eenheden.
#' Als de delta-pH boven de signaleringswaarde ligt, ken het
#' concept QC oordeel twijfelachtig toe aan het monster.
#'         
#' @param d_veld dataframe met veldobservaties   
#' @param d_metingen dataframe met metingen
#' @param ph_veld_naam character string om te gebruiken als pH 
#' veld. Staat standaard op "pH_veld".
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC4e <- function(d_veld, d_metingen, ph_veld_naam = "pH_veld", verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenVeld(d_veld)
  testKolommenMetingen(d_metingen)
  
  # Selecteer pH veld en lab gegevens en pH naam aanpassen
  d <- d_metingen
  d <- d %>% 
    dplyr::filter(parameter %in% c("pH", ph_veld_naam)) %>% 
    dplyr::mutate(
      parameter = dplyr::case_match(
        parameter,
        ph_veld_naam ~ "pH_veld",
        .default = parameter
      )
    )
  
  # Check of pH veld en lab gegevens beschikbaar zijn
  if(dplyr::n_distinct(d$parameter) < 2) {
    stop("Geen veld of lab pH aanwezig. Gebruik: x <- QC_niet_uitvoerbaar(x, \"QC4e\")")
  }
  if(dplyr::n_distinct(d$parameter) > 2) {
    stop("Meer dan 2 parameters voor veld en lab pH")
  }
  
  # Vergelijk pH lab en pH veld
  res <- d %>%
    dplyr::select(-c(qcid, detectieteken, rapportagegrens)) %>%
    tidyr::pivot_wider(names_from = parameter,
                       values_from = waarde) 
  
  # Rijen met missende waardes op niet uitvoerbaar zetten
  niet_uitvoerbaar_id <- qcidNietUitvoerbaar(res, d_metingen, c("pH", "pH_veld"))
  
  # Rijen met missende waardes weghalen
  res <- res %>% drop_na(c("pH", "pH_veld"))
  
  res <- res %>%
    dplyr::mutate(absoluut_verschil_pH = abs(pH - pH_veld),
                  oordeel = ifelse(absoluut_verschil_pH >= .5,
                                   "twijfelachtig", "onverdacht"),
                  iden = monsterid) %>%
    dplyr::filter(oordeel != "onverdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "metingen waar pH-lab en pH-veld 0.5 pH-eenheden of meer afwijken.",
                           "Controleer de datum (analyse en veld) en historische gegevens van alle afwijkingen")
  
  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)
      
      res %>% 
        dplyr::select(monsterid, putcode, filter, jaar, pH, pH_veld, absoluut_verschil_pH) %>% 
        print()
      
    } else {
      print(paste("Er zijn geen metingen waar pH-lab en pH-veld 0.5 pH-eenheden of meer afwijken"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., res %>% select(pH, pH_veld, absoluut_verschil_pH, iden)) 
  resultaat_df <- resultaat_df %>% select(qcid, monsterid, jaar, maand, dag, putcode, filter, pH, pH_veld, absoluut_verschil_pH, oordeel)
  
  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid) %>% 
    dplyr::pull(qcid)
  
  test <- "QC4e"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = twijfel_id)
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "niet uitvoerbaar",
                                  ids = niet_uitvoerbaar_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}

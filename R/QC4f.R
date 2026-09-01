#' QC4f. Controle pH en HCO3
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

QC4f <- function(d_metingen, ph_veld_naam = NA, hco3_veld_naam = NA, verbose = F) {
  
  # Test of relevante kolommen aanwezig zijn
  # deze hulpfuncties staan in utils, deze eerst nu nog runnen.
  testKolommenMetingen(d_metingen)
  
  d <- d_metingen
  
  # Filter te gebruiken data
  d <- d %>%
    dplyr::filter(parameter %in% c("pH", "HCO3", ph_veld_naam, hco3_veld_naam)) %>% 
    tidyr::drop_na(parameter) %>% 
    dplyr::mutate(
      parameter = dplyr::case_match(
        parameter,
        ph_veld_naam ~ "pH_veld",
        hco3_veld_naam ~ "HCO3_veld",
        .default = parameter
      )
    )
  
  # Check of maar 1 voor pH en 1 voor HCO3 beschikbaar zijn
  unique_params <- d$parameter %>% unique()
  any_ph <- unique_params %in% c("pH", "pH_veld") %>% any()
  any_hco3 <- unique_params %in% c("HCO3", "HCO3_veld") %>% any()
  
  if(all(any_ph, any_hco3) == FALSE) {
    stop("Geen pH of HCO3 beschikbaar. Gebruik: x <- QC_niet_uitvoerbaar(x, \"QC4f\")")
  }
  
  # Vergelijk pH lab HCO3 lab
  # namen nog onzeker vanuit provinciale dataset, mogelijk nog aanpassen!
  res <- d %>%
    dplyr::select(-c(qcid, detectieteken, rapportagegrens)) %>%
    tidyr::pivot_wider(names_from = parameter,
                       values_from = waarde) 
  
  # Eventueel ontbrekende kolommen toevoegen
  params <- c("pH", "pH_veld", "HCO3", "HCO3_veld")
  res[params[!(params %in% colnames(res))]] <- NA
  
  # Na waardes van pH en HCO3 waar ofwel lab of welmeting aanwezig is op -9999 zetten
  # Als een van beide aanwezig is kan de test worden uitgevoerd. Anders gaat qcidNietUitvoerbaar niet goed
  res <- res %>% mutate(
    pH_veld = case_when(is.na(pH_veld) & !is.na(pH) ~ -9999,
                        TRUE ~ pH_veld),
    HCO3_veld = case_when(is.na(HCO3_veld) & !is.na(HCO3) ~ -9999,
                          TRUE ~ HCO3_veld),
    pH = case_when(is.na(pH) & !is.na(pH_veld) ~ -9999,
                   TRUE ~ pH),
    HCO3 = case_when(is.na(HCO3) & !is.na(HCO3_veld) ~ -9999,
                     TRUE ~ HCO3)
  )
  
  # Rijen met missende waardes op niet uitvoerbaar zetten
  niet_uitvoerbaar_id <- qcidNietUitvoerbaar(res, d_metingen, c("pH", "HCO3"))
  
  # Rijen met missende waardes weghalen
  res <- res %>% drop_na(c("pH", "HCO3"))
  
  # Terugzetten NA waardes
  res <- res %>% mutate(
    across(all_of(params), ~ case_match(.x,
                                        -9999 ~ NA,
                                        .default = .x)))
  
  # Controle verhouding pH HCO3
  res <- res %>% 
    rowwise() %>% 
    dplyr::mutate(oordeel = ifelse(min(pH, pH_veld, na.rm = TRUE) < 5 & max(HCO3, HCO3_veld, na.rm = TRUE) > 15 |
                                     min(pH, pH_veld, na.rm = TRUE) < 5.5 & max(HCO3, HCO3_veld, na.rm = TRUE) > 50,
                                   "twijfelachtig", "onverdacht"),
                  iden = monsterid) %>%
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
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, pH, pH_veld, HCO3, HCO3_veld), by = "iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter, 
                  pH, pH_veld, HCO3, HCO3_veld, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid) %>% 
    dplyr::pull(qcid)
  
  test <- "QC4f"
  
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

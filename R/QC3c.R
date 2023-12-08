#' QC3c. Controle ionenbalans
#'
#' Controle en berekening van de ionenbalans (IB)
#' op monsterniveau
#'
#' Bereken de ionenbalans volgens de methode in 
#' Bijlage II van het QC Protocol. De signaleringswaarde
#' voor monsters is IB >10%. Als de ionenbalans
#' boven de signaleringswaarde ligt, ken het concept
#' QC oordeel twijfelachtig toe aan het monster.  
#'      
#' @param d_metingen dataframe met metingen
#' @param ph_naam character string om te gebruiken als pH. Staat standaard
#' op "pH". Enkel in het geval dat HCO3 in het veld is gemeten vul hier de naam van 
#' de ph veld parameter in.
#' @param hco3_naam character string om te gebruiken als HCO3. Staat standaard
#' op "HCO3". Enkel in het geval dat HCO3 in het veld is gemeten vul hier de naam van
#' de hco3 veld parameter in.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC3c <- function(d_metingen, ph_naam = "pH", hco3_naam = "HCO3", verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # pH en HCO3 naam aanpassen alleen voor LMG
  d <- d_metingen
  #d <- d %>%  mutate(
  #  parameter = case_when(parameter == ph_veld_naam ~ "pH_veld",
  #                        TRUE ~ parameter)
  #)
  
  #d$parameter <- d$parameter %>%
  #  recode("h_5__veld" = "hv",
  #         "hco3_veld" = "hco3v",
  #         "nh4_n" = "nh4",
  #         "no3_n" = "no3",
  #         "po4_p" = "po4",
  #         .default = d$parameter)
  
  # selecteer relevante ionen om mee te nemen
  an <- c("Cl", hco3_naam, "NO3", "SO4", "CO3", "PO4")
  cat <- c("Al", "Ca", "Fe", "K", "Mg", "Mn", "NH4", "Na", "Zn", ph_naam)
  
  res <- d %>%
    # pH ook meenemen -> omrekenen naar h3o
    dplyr::filter(parameter %in% c(an, cat)) %>%
    # parameter namen naar niet veld variant zodat de rest van de berekeningen niet aangepast hoeft te worden
    mutate(parameter = case_when(parameter == ph_naam ~ "pH",
                                 parameter == hco3_naam ~ "HCO3",
                                 TRUE ~ parameter)) %>% 
    # alle NA's op 0 zetten, behalve pH en de belangrijkste ionen
    dplyr::mutate(waarde_ib = ifelse(!parameter %in% c("pH", "Ca", "Na", "Mg", "K", 
                                                       "Cl", "SO4") & is.na(waarde),
                                     0, waarde)) %>%
    # soms staat RG als NA, < of "", eerst NA veranderen in ""
    dplyr::mutate(detectieteken = ifelse(is.na(detectieteken), "", 
                                         detectieteken)) %>%
    # waardes <RG niet meenemen maar op 0 zetten 
    dplyr::mutate(waarde_ib = ifelse(parameter != "pH" & detectieteken != "",
                                     0, waarde_ib)) %>%
    # als geen pH bekend is, dan is de pH 7 
    dplyr::mutate(waarde_ib = ifelse(parameter == "pH" & is.na(waarde),
                                     7, waarde_ib)) %>%
    #  zet kolommen naar wide format
    dplyr::select(-c(qcid, detectieteken, rapportagegrens, waarde)) %>%
    tidyr::pivot_wider(., 
                       names_from = parameter,
                       names_glue = "{parameter}_meq",
                       values_from = waarde_ib) 
  
  # Rijen met missende waardes op niet uitvoerbaar zetten
  niet_uitvoerbaar_id <- qcidNietUitvoerbaar(res, d_metingen, c("Ca_meq", "Na_meq", "Mg_meq", "K_meq", "Cl_meq", "SO4_meq"))
  
  # Rijen met missende waardes weghalen
  res <- res %>% drop_na(c("Ca_meq", "Na_meq", "Mg_meq", "K_meq", "Cl_meq", "SO4_meq"))
  
  
  # reken om naar meq/l volgens BRO
  # Al is in microgram
  res$Al_meq <- 0.003 * res$Al_meq / 26.98
  res$Ca_meq <- 2 * res$Ca_meq / 40.08
  res$Cl_meq <- res$Cl_meq / 35.453
  # Fe is in microgram
  res$Fe_meq <- 0.002 * res$Fe_meq / 55.85
  # pH omrekenen naar mili-equivalent dus maal 1000
  res$H3O_meq <- 1000 * 10^-res$pH_meq
  res$K_meq <- res$K_meq / 39.102
  res$Mg_meq <- 2 * res$Mg_meq / 24.31
  # Mn is in microgram
  res$Mn_meq <- 0.002 * res$Mn_meq / 54.94
  res$NH4_meq <- res$NH4_meq / 18.0383
  res$NO3_meq <- res$NO3_meq / 62.0049
  res$Na_meq <- res$Na_meq / 22.9898
  res$PO4_meq <- 3 * res$PO4_meq / 94.9712
  res$SO4_meq <- 2 * res$SO4_meq / 96.062
  # Zn is in microgram
  res$Zn_meq <- 0.002 * res$Zn_meq / 65.39
  res$HCO3_meq <- res$HCO3_meq / 61.0168
  res$CO3_meq <- 2 * res$CO3_meq / 60.0168

  
  # bereken ionenbalans
  res <- res %>%
    dplyr::mutate(pos = Al_meq + Ca_meq + 0.6*Fe_meq + K_meq + Mg_meq + Mn_meq + NH4_meq + Na_meq + Zn_meq + H3O_meq,
                  neg = Cl_meq + HCO3_meq + NO3_meq + SO4_meq + CO3_meq + PO4_meq) %>%
    dplyr::mutate(ib = round(100 * (pos - neg) / (pos + neg), digits = 2)) %>%
    # markeer afwijkingen
    # voor oligominerale of geconcentreerde watermonsters (bijv. met SOM kationen < 2.0
    # of > 8.0 meq/l) mag een hogere signaleringswaarde van IB worden gehanteerd. Dit
    # ter beoordeling van de validerende instantie. 
    # Eventueel nog keuze opnemen?
    dplyr::mutate(oordeel = ifelse(abs(ib) > 10,
                                   "twijfelachtig", "onverdacht"),
                  iden = monsterid) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::rename(`som cat` = pos,
                  `som an` = neg) 
  
  # rapportagetekst
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "monsters waarbij het verschil in ionenbalans",
                           "groter dan 10% is.", "Bij", nrow(res %>% 
                                                               dplyr::filter(ib > 10)),
                           "monsters is de ionenbalans sterk positief (>10%) en bij",
                           nrow(res %>% 
                                  dplyr::filter(ib < -10)), "monsters sterk negatief (< -10%).")
  
  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Bij alle monsters is het verschil in ionenbalans <10%"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  resultaat_df <- d_metingen %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, `som cat`, `som an`, ib)) 
  
  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid) %>% 
    dplyr::pull(qcid)
  
  test <- "QC3c"
  
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

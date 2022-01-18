#' QC4b. Controle technisch waardebereik
#'
#' Controleer of een meting fysisch niet mogelijk is.
#'      
#' Parameters met een fysisch waardebereik:
#' - pH: 0 - 14 [-]
#' 
#' Indien de meetwaarde buiten het fysisch waardebereik valt, ken
#' de betreffende parameter het concept QC oordeel verdacht toe.         
#'               
#' @param d_metingen dataframe met metingen
#' @param ph_veld_naam character string om te gebruiken als pH 
#' veld. Staat standaard op "pH_veld".
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'

#' De benodigde naam voor pH lab is pH.
#' De benodigde naam voor pH veld staat standaard op "pH_veld", maar kan worden 
#' aangepast door een character string in te vullen voor ph_veld_naam.
#'
#' @export
#'


QC4b <- function(d_metingen, ph_veld_naam = "pH_veld", verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # pH naam aanpassen alleen voor LMG
  d <- d_metingen
  d <- d %>%  mutate(
    parameter = ifelse(parameter == ph_veld_naam, "pH_veld", parameter)
  )
  # d$parameter <- d$parameter %>%
  #   dplyr::recode("h" = "ph",
  #                 "h_1__veld" = "ph_1__veld",
  #                 "h_5__veld" = "ph_5__veld",
  #                 .default = d$parameter)
  # 
  # selecteer pH veld en lab gegevens
  # afhankelijk van de dataset kan dit 'pH' of 'zuurgraad' zijn
  d <- d %>%
    dplyr::filter(parameter %in% c("pH", "pH_veld")) #%>%
    # NA's verwijderen?
    #filter(!is.na(waarde))
  
  # Check of pH veld en lab gegevens beschikbaar zijn
  if(dplyr::n_distinct(d$parameter) < 2) {
    stop("Geen veld of lab pH aanwezig")
  }
  if(dplyr::n_distinct(d$parameter) > 2) {
    stop("Meer dan 2 parameters voor veld en lab pH")
  }

  # Controleer voor alle pH metingen (veld en lab) of
  # de meting in het meetbereik ligt
  res <- d %>%
    dplyr::mutate(oordeel = ifelse(waarde < 0 | waarde > 14,
                                   "verdacht", "onverdacht"),
           iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::filter(oordeel != "onverdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "metingen waar de pH-lab en/of pH-veld niet ",
                           "binnen het meetbereik liggen.")
  
  if(verbose) {
    if(nrow(res) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er zijn geen metingen waar pH-lab en/of pH-veld buiten het meetbereik liggen"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  resultaat_df <- d_metingen %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                   "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") 
  
  verdacht_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "verdacht") %>% 
    dplyr::distinct(qcid) %>%
    pull(qcid)
  
  test <- "QC4b"
  
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


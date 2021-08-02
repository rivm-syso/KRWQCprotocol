#' QC1c. Controle rapportagegrens
#'
#' Controle op aanwezigheid rapportagegrens en detectiesymbool
#'
#' Indien de rapportagegrens voor een parameter ontbreekt, ken het
#' concept oordeel verdacht toe aan de betreffende parameter.
#' Indien waardes <rapportagegrens geen detectiesymbool bevatten,
#' ken het concept oordeel verdacht toe aan de betreffende parameter.
#' 
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` blijkt of de locatie/monster 'onverdacht' of 'verdacht' is.
#'
#' @export
#'


QC1c <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Controle op aanwezigheid rapportagegrens
  d <- d_metingen %>%
    dplyr::mutate(oordeel = ifelse(is.na(rapportagegrens), "verdacht",
                            "onverdacht"),
           reden = ifelse(is.na(rapportagegrens), 
                          "rapportagegrens ontbreekt", "")) 
  
  # controle op waardes <RG en aanwezigheid detectietekens
  d1 <- d %>%
    dplyr::filter(!is.na(rapportagegrens)) %>%
    dplyr::mutate(oordeel = ifelse(waarde < rapportagegrens & detectieteken != "<",
                            "verdacht", oordeel),
           reden = ifelse(waarde < rapportagegrens & detectieteken != "<",
                         "detectiesymbool ontbreekt bij waardes <RG", reden)) %>%
    dplyr::filter(!qcid %in% d$qcid)
  
  # voeg samen
  resultaat_df <- rbind(d, d1) %>%
    dplyr::filter(oordeel == "verdacht")
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           nrow(resultaat_df %>% 
                                  dplyr::filter(reden == "rapportagegrens ontbreekt")), 
                           "metingen waar de rapportagegrens ontbreekt en",
                           nrow(resultaat_df %>% 
                                  dplyr::filter(reden == "detectiesymbool ontbreekt bij waardes <RG")),
                           "metingen waar het detectiesymbool ontbreek bij waardes <RG")

  if(verbose) {
    if(nrow(resultaat_df) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Bij alle metingen is een rapportagegrens opgegeven",
                  "en metingen <RG zijn voorzien van een detectieteken"))
    }
  }

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "verdacht") %>% 
    dplyr::distinct(qcid) %>%
    dplyr::pull(qcid)
  
  test <- "QC1c"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = unique(resultaat_df$oordeel),
                                  ids = verdacht_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  return(d_metingen)
  
}


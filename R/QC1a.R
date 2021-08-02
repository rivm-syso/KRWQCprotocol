
#' QC1a. Controle parameterlijst BRO
#'
#' Controle op ID, aquocode, CASnummer, omschrijving,
#' eenheid en hoedanigheid.
#'
#' Controleer of de identificatiekenmerken van de individuele
#' metingen overeenkomen met de parameterlijst BRO. In geval van
#' afwijken, ken het concept QC oordeel verdacht toe aan de betreffende
#' parameters.
#'
#' Indien een parameter niet op de parameterlijst BRO aanwezig is, dan
#' dient deze te worden aangevraagd via de servicedesk BRO. De parameterlijst
#' wordt hiermee uitgebreid wanneer het een legitieme, nieuwe parameter betreft.
#'
#' @param d_parameter dataframe met putcode, watertype en redoxklasses
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F).
#' Staat standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters.
#'
#' @export
#'


QC1a <- function(d_parameter, d_metingen, verbose = F) {

  # laad parametertabel uit de BRO
  # eventueel nog toevoegen dat de parameterlijst uit de BRO automatisch wordt
  # ingeladen vanuit de BRO dmv API?
    data(BRO_parameterlijst)

  # Check datasets op kolommen en unieke informatie
  testKolommenParameter(d_parameter)
  valideParamInfo(d_parameter)

  # controleer of parameters op de BRO lijst staan
  # hiervoor worden de cas-nummers gebruikt
  niet_in_BRO <- d_parameter %>%
    dplyr::select(qcid, naam, aquocode, cas) %>%
    dplyr::filter(!cas %in% BRO_parameterlijst$casnummer) %>%
    dplyr::mutate(oordeel = "verdacht",
           reden = "parameter komt niet voor in BRO-lijst")
  
  # Als printen gewenst is (T)
  if(verbose) {
    if(nrow(niet_in_BRO) > 0) {
        print(paste("Er zijn", nrow(niet_in_BRO),
              "parameters welke nog niet in de BRO-lijst voorkomen"))
    } else {
      print("Alle parameters komen voor in de BRO-lijst")
    }
  }
  
  # controleer of parameters dezelfde identificatiekenmerken uit de BRO bevatten
  # dit gebeurt alleen voor de parameters welke wél obv CASnr gekoppelt konden worden
  res <- d_parameter %>%
    dplyr::filter(cas %in% BRO_parameterlijst$casnummer) %>%
    dplyr::left_join(BRO_parameterlijst, by = c("cas" = "casnummer")) %>%
    # Aquocode vergelijken
    dplyr::mutate(oordeel = ifelse(aquocode.x != aquocode.y |
                            # omschrijving
                            omschrijving.x != omschrijving.y |    
                            # eenheid
                            eenheid.x != eenheid.y |    
                            # hoedanigheid
                            hoedanigheid.x != hoedanigheid.y,
                          "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::rename(aquocode_VAL = aquocode.x,
           aquocode_BRO = aquocode.y,
           stofnaam_VAL = omschrijving.x,
           stofnaam_BRO = omschrijving.y,
           eenheid_VAL = eenheid.x,
           eenheid_BRO = eenheid.y,
           hoedanigheid_VAL = hoedanigheid.x,
           hoedanigheid_BRO = hoedanigheid.y)

  rapportageTekst <- paste("Er zijn in totaal", nrow(niet_in_BRO), 
                           "parameters welke niet in de parameterlijst van de BRO staan.",
                           "Van de parameters welke wel gecontrolleerd konden worden wijken",
                           nrow(res), "parameters af van de informatie uit de BRO.")

  
  # print resultaat indien gewenst
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res)
    } else {
      print("Elke parameter heeft dezelfde informatie als in de BRO staat.")
    }
  }

  # voeg concept oordeel van afwijkende parameters toe aan de parameters in metingen bestand
  resultaat_df <- d_metingen %>%
    dplyr::mutate(oordeel = ifelse(parameter %in% res$naam,
                            "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht")
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC1a"
  
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
  
  # return beoordeelde d_metingen
  return(d_metingen)
}


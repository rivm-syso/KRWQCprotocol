#' QC0a. Controle XY-coordinaten
#'
#' Controleer of de geregistreerde XY-coordinaten uit het veld
#' overeenkomen met de in de BRO geregistreerde putcoordinaten uit 
#' opzoekbestand. 
#' 
#' De veldwerker stelt op basis van GPS-coordinaten de locatie vast.
#' Aan de BRO mogen de gegevens in de volgende referentiestelsels 
#' aangeleverd worden: RD en ETR89.
#' 
#' Indien de coordinaten meer dan 20 meter afwijken van de oorspronkelijke
#' putcoordinaten, ken het concept QC oordeel verdacht toe aan het monster.
#' 
#' Bij putlocaties met een of meerdere andere putlocaties binnen 20 meter
#' afstand, vergelijk de putlocatie met de informatie meegeleverd vanuit
#' 'de eigen organisatie' ter identificatie van de te bemonsteren put.
#' - RIVM: vergelijk de put met de meegeleverde foto's van de put
#' - Provincies: vergelijk het label van de put met meegeleverde code.
#'
#'
#' @param d_veld dataframe met putcode en XY-coordinaten voor het bemonsterde jaar X 
#' welke vergeleken wordt met de historische XY-coordinaten doorgegeven aan de 
#' BRO als referentie.
#' @param d_put BRO geregisteerde metadata behorende bij de put of putfilter.
#' @param d_metingen metingen bestand met monster ID's om bij afwijkingen in de
#' xy-coordinaten te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#' 
#' @export
#'

QC0a <- function(d_veld, d_put, d_metingen, verbose = F) {
  
  # Test of relevante kolommen aanwezig zijn
  # deze hulpfuncties staan in utils, deze eerst nu nog runnen.
  testKolommenVeld(d_veld)
  testKolommenPut(d_put)
  testKolommenMetingen(d_metingen)

  # Test of coordinaten in Nederland liggen
  testCoordinaten(d_veld)
  testCoordinaten(d_put)

  # Selecteer alleen putnummer, jaar, xy 
  d <- d_veld %>%
    dplyr::select(putcode, jaar, maand, dag, xcoord, ycoord) 
  
  # Vergelijk coordinaten met opzoektabel van aangeleverde BRO data (d_put)
  res <- merge(d, d_put %>% dplyr::select(putcode, xcoord, ycoord), by = "putcode") %>%
    dplyr::mutate(oordeel = ifelse(xcoord.x - xcoord.y > 20 | xcoord.y - xcoord.x > 20,
                            "verdacht",
                            ifelse(ycoord.x - ycoord.y > 20 | ycoord.y - ycoord.x > 20,
                                   "verdacht", "onverdacht")),
           iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::rename(X_val = xcoord.x,
           Y_val = ycoord.x,
           X_BRO = xcoord.y,
           Y_BRO = ycoord.y)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "bemonsterde putlocaties met XY-coordinaten afwijkend",
                           "van de BRO geregistreerde putcoordinaten.")
  
  # Als printen gewenst is (T)
  # Print preview van afwijkende XY-coordinaten 
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% dplyr::select(putcode, X_val, Y_val, X_BRO, Y_BRO))
    
    } else {
      # als elke put XY-coordinaten heeft die binnen 20m 
      # van de BRO-geregistreerde putcoordinaten liggen
      print("Elke putlocatie XY-coordinaten ligt binnen 20m van de BRO geregistreerde putcoordinaten.")
    }
  }

  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                            "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, X_val, Y_val, X_BRO, Y_BRO),by="iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter, 
           X_val, Y_val, X_BRO, Y_BRO, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC0a"
  
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


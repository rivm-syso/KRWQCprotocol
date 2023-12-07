#' QC0f. Controle filterdiepte
#'
#' Controle op meetfout in de filterdiepte of aanwezigheid van 
#' sediment in het filter.
#' 
#' Indien de filterdiepte meer dan 10 centimeter afwijkt van de
#' geregistreerde filterdiepte, ken het concept QC oordeel
#' twijfelachtig toe aan het monster.
#' 
#' @param d_veld dataframe met putcode en filterdiepte waargenomen in het veld
#' voor het bemonsterde jaar X.
#' @param d_filter BRO geregisteerde metadata behorende bij de put of putcode.
#' @param d_metingen metingen bestand met monster ID's om bij verkeerde filterbemonstering 
#' te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0f <- function(d_veld, d_filter, d_metingen, verbose = F) {
  
  # Check datasets
  testKolommenVeld(d_veld)
  testKolommenFilter(d_filter)
  testKolommenMetingen(d_metingen)
  
  # Selecteer alleen putcode, filter, jaar, maand, dag, okf
  d <- d_veld %>%
    dplyr::select(monsterid, putcode, filter, okf) 
  
  # Vergelijk filterdiepte met opzoektabel van aangeleverde BRO data
  res <- merge(d, d_filter %>% dplyr::select(qcid, putcode, filter, diepte_onder), 
               by = c("putcode", "filter")) %>%
    dplyr::mutate(oordeel = ifelse(abs(okf - diepte_onder) > 0.1,  
                            "twijfelachtig", "onverdacht"),
           iden = monsterid) %>%
    dplyr::filter(oordeel == "twijfelachtig") %>%
    dplyr::rename(`onderkant filter_VAL` = okf,
           `onderkant filter_BRO` = diepte_onder)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "bemonsterde putfilters met >10 cm afwijkende filterdieptes",
                           "t.o.v. de BRO geregistreerde putcoordinaten.")
  
  # Als er een afwijkende filterdiepte is, print deze
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% dplyr::select(putcode, filter,
                           `onderkant filter_VAL`,
                           `onderkant filter_BRO`))
      
    } else {
      # als er geen afwijkende filterdieptes benoemd zijn 
      print("Voor elke putcode komt de filterdiepte overeen met de BRO gegevens.")
    }
  }

  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                            "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel == "twijfelachtig") %>%
    # voeg resultaten toe uit res
    dplyr::left_join(., res %>% dplyr::select(iden, `onderkant filter_VAL`, `onderkant filter_BRO`), by = "iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter, 
           `onderkant filter_VAL`, `onderkant filter_BRO`, oordeel)

  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC0f"
  
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

  # return beoordeelde putten in d_filter en beoordeelde monsters in d_metingen
  return(d_metingen)
  
}


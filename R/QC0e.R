#' QC0e. Controle filter
#'
#' Controle op bemonstering van het juiste filter.
#' 
#' Als de diepte onderkant filter (okf) meer dan de helft van de 
#' filterlengte (meestal 1-2 meter) afwijkt dan in de BRO 
#' geregistreerde diepte, markeer de waarde als verdacht. 
#'
#' @param d_veld dataframe met putcode en filterdiepte waargenomen in het veld
#' voor het bemonsterde jaar X.
#' @param d_filter BRO geregisteerde metadata behorende bij de putcode.
#' @param d_metingen metingen bestand met monster ID's om bij verkeerde filterbemonstering 
#' te kunnen markeren.
#' @param fl filterlengte. Meestal is dit 1-2 meter. Standaard ingevuld is 
#' nu 2 meter, maar dit kan aangepast worden met deze parameter.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0e <- function(d_veld, d_filter, d_metingen, fl = 2, verbose = F) {
  
  # Check datasets
  testKolommenVeld(d_veld)
  testKolommenFilter(d_filter)
  testKolommenMetingen(d_metingen)
  
  # Selecteer alleen putcode, filter, datum, okf
  d <- d_veld %>%
    select(monsterid, putcode, filter, okf) 
  
  # Vergelijk filterdiepte met opzoektabel van aangeleverde BRO data
  # Nu genomen dat okf zowel niet ondieper als dieper mag liggen. Wijkt af van protocol!
  res <- merge(d, d_filter %>% select(qcid, putcode, filter, diepte_onder), 
               by = c("putcode", "filter")) %>%
    mutate(oordeel = ifelse(abs(okf - diepte_onder) > 0.5 * fl,  
                            "verdacht", "onverdacht"),
           iden = monsterid) %>%
    filter(oordeel == "verdacht") %>%
    rename(`onderkant filter_val` = okf,
           `onderkant filter_BRO` = diepte_onder)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "bemonsterde putfilters met afwijkende filterdieptes",
                           "van de BRO geregistreerde putcoordinaten.")
  
  # Als er een afwijkende filterdiepte is, print deze
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% select(putcode, filter,
                           `onderkant filter_val`,
                           `onderkant filter_BRO`))
      
    } else {
      # als er geen afwijkende filterdieptes benoemd zijn 
      print("Voor elke putcode komt de filterdiepte overeen met de BRO gegevens.")
    }
  }
  
  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    group_by(monsterid) %>%
    mutate(iden = monsterid) %>%
    mutate(oordeel = ifelse(iden %in% res$iden,
                            "twijfelachtig", "onverdacht")) %>%
    filter(oordeel == "twijfelachtig") %>%
    left_join(., res %>% select(iden, `onderkant filter_val`, `onderkant filter_BRO`), by = "iden") %>%
    select(qcid, monsterid, jaar, maand, dag, putcode, filter,
           `onderkant filter_val`, `onderkant filter_BRO`, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC0e"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = verdacht_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  # return beoordeelde monsters in d_metingen
  return(d_metingen)
  
}


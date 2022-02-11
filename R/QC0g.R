#' QC0g. Controle zoet-zout grondwater
#'
#' Controle op de chloridegegevens en of er sprake is van
#' eenheidsfouten.
#'   
#' Controleer of de samenstelling van het grondwater is veranderd
#' van zoet (<300 mg/l Cl) naar brak/zout (>300 mg/l Cl) of vice
#' versa. Vergelijk de laatst gemeten Cl concentratie uit het invoerbestand 
#' met de gemiddelde Cl concentratie over de voorlaatste 5 metingen (of het 
#' gemiddelde over alle voorgaande Cl metingen indien minder 
#' dan 5 metingen aanwezig zijn). Is de samenstelling veranderd,
#' ken het concept QC oordeel twijfelachtig toe aan het monster.
#'   
#' @param d_filter dataframe met putcode en watertype vastgesteld
#' @param d_metingen metingen uit de nieuwe meetronde met chloride concentraties
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0g <- function(d_filter, d_metingen, verbose = F) {
  
  # Check datasets
  testKolommenFilter(d_filter)
  testKolommenMetingen(d_metingen)

  hist <- d_metingen %>%
    # verwijder meest recente meting en selecteer chloride
    dplyr::filter(jaar != max(d_metingen$jaar), parameter == "Cl") %>%
    # selecteer voorlaatste 5 metingen
    dplyr::group_by(putcode, filter) %>%
    dplyr::arrange(jaar) %>%
    # bepaal gemiddelde chloride conc en ken watertype toe
    dplyr::top_n(n = 5, jaar) %>%
    dplyr::summarise(gem.cl = mean(waarde),
              watertype.hist = ifelse(gem.cl < 300,
                                      "zoet", "brak/zout"))

    # watertype huidige meetronde
    huidig <- d_metingen %>%
        dplyr::filter(jaar == max(d_metingen$jaar), parameter == "Cl") %>%
        dplyr::mutate(watertype.val = ifelse(waarde < 300,
                                      "zoet", "brak/zout"))

        # samenvoegen en vergelijken zodat oordeel kan worden gegeven
        res <- merge(huidig, hist, by = c("putcode", "filter")) %>%
            dplyr::group_by(putcode, filter) %>%
            dplyr::mutate(oordeel = ifelse(watertype.val != watertype.hist,
                                    "twijfelachtig", "onverdacht"),
                   iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
            dplyr::filter(oordeel == "twijfelachtig") %>%
            dplyr::ungroup()

  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "bemonsterde putfilters waar het watertype (zoet/brak)",
                           "afwijkt van de gemiddelde over de laatste 5 meetrondes.")
  
  # Als een putcode een afwijkend watertype heeft, print deze
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% dplyr::select(putcode, filter,
                           watertype_VAL = watertype.val,
                           watertype_HIST = watertype.hist,
                           cl_VAL = waarde,
                           cl_HIST = gem.cl))       #Bij de 1e optie kan ook de cl van historische metingen getoond worden
      
    } else {
      # als er geen afwijkende watertypes gevonden zijn
      print(paste("Er zijn geen putfilters waar de samenstelling van het grondwater",
                  "is veranderd van zoet naar brak/zout of vice versa."))
    }
  }

  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                            "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel == "twijfelachtig") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, watertype.val, watertype.hist, gem.cl), by = "iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter,
           watertype.val, watertype.hist, gem.cl, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC0g"
  
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

  # return beoordeelde putten in d_filter en beoordeelde monsters in d_metingen
  return(d_metingen)
  
}


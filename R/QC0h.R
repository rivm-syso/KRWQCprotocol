#' QC0h. Controle redoxklasse grondwater
#'
#' Controle op fouten in de redoxparameters
#'    
#' Controleer of de redoxklasse (bepaald volgens werkwijze in 
#' Bijlage II QC Protocol) overeenkomt met de redoxklasse uit
#' het bronbestand. Wijkt de redoxklasse 1 klasse af, ken het QC oordeel
#' twijfelachtig toe aan het monster. Wijkt de redoxklasse meer
#' dan 1 klasse af, ken het QC oordeel verdacht toe. 
#'       
#' @param d_filter dataframe met putcode, watertype en redoxklasses
#' @param d_metingen metingen uit de nieuwe meetronde 
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0h <- function(d_filter, d_metingen, verbose = F) {
  
  # Check datasets
  testKolommenFilter(d_filter)
  testKolommenMetingen(d_metingen)
  
  # Check geldige waardes redoxklasses
  # alleen als getoetst wordt aan puttenbestand
  #
  
  # Check ontbrekende redox parameters?
  #
  #
  
  # Wat te doen met meerdere metingen per meetronde?
  #
  #
  
  # Selecteer alleen redoxparameters
  d <- d_metingen %>%
    filter(parameter %in% c("NO3", "Fe", "Mn", "SO4", "Cl")) %>%  #no3_n is in LMG
    select(-qcid) %>%
    # zet Redox parameters naar wide format
    pivot_wider(names_from = parameter,
                values_from = c(detectieteken, rapportagegrens, waarde),
                names_glue = "{parameter}_{.value}") 
  
  d <- d %>%
    select(monsterid, jaar, maand, dag, putcode, filter,
           sort(colnames(.)))
  
  # Rijen met missende waardes op niet uitvoerbaar zetten
  niet_uitvoerbaar_id <- qcidNietUitvoerbaar(d, d_metingen, c("NO3_waarde", "Fe_waarde", "Mn_waarde", "SO4_waarde", "Cl_waarde"))
  
  # Rijen met missende waardes weghalen
  d <- d %>% drop_na(c("NO3_waarde", "Fe_waarde", "Mn_waarde", "SO4_waarde", "Cl_waarde"))
  
  # Bepaal redoxklasse voor alle jaren
  d <- d %>%
    # bepaal SO4f
    mutate(so4f = ((SO4_waarde * 19000) / (Cl_waarde * 2700)) -1 ) %>%
    mutate(redoxklasse = 
             ifelse(NO3_waarde > 2,
                    # als NO3 > 2 mg/l
                    ifelse(Fe_waarde < 1,
                           # als Fe < 1 mg/l
                           ifelse(Mn_waarde < 0.5,
                                  # als Mn < 0.5 mg/l
                                  "subox",
                                  # als Mn >= 0.5 mg/l
                                  "mn-anox"),
                           # als Fe >= 1 mg/l
                           "fe-anox"),
                    # als NO3 <= 2 mg/l
                    ifelse(so4f > -0.2,
                           # als SO4f > -0.2 
                           "fe-anox",
                           # als SO4f <= -0.2
                           ifelse(so4f > -0.98,
                                  # als SO4f > -0.98
                                  "so4-red",
                                  # als SO4f <= -0.98
                                  "methano"))
             )
    ) %>%
    # als redoxklasse niet te berekenen is door ontbreken van een parameter dan 'ntb'
    mutate(redoxklasse = ifelse(is.na(redoxklasse), "ntb",
                                redoxklasse)) %>%
    # per redoxklasse wordt een oplopende index meegegeven zodat de grootte van het
    # verschil in redoxklasse tussen verschillende jaren vergeleken kan worden
    mutate(redoxnr = ifelse(redoxklasse == "subox", 1,
                            ifelse(redoxklasse == "mn-anox", 2,
                                   ifelse(redoxklasse == "fe-anox", 3,
                                          ifelse(redoxklasse == "so4-red", 4,
                                                 ifelse(redoxklasse == "methano", 5, -99))))))
  
  # selecteer huidige meetronde
  d_huidig <- d %>%
    filter(jaar == max(d_metingen$jaar))
  
  # selecteer voorgaande redoxbepaling per putfilter ter vergelijking
  d_hist <- d %>%
    filter(jaar != max(d_metingen$jaar)) %>%
    group_by(putcode, filter) %>%
    top_n(n = 1, jaar) %>%
    distinct()
  
  # Vergelijk redoxklasse huidige meetronde met voorgaande meting
  res <- left_join(d_huidig, d_hist %>% select(putcode, filter, redoxklasse, redoxnr),
                   by = c("putcode", "filter")) %>%
    # OF alleen kijken naar verschil in redoxklasse
    # mutate(oordeel = ifelse(redoxklasse.x != redoxklasse.y,
    #                         "twijfelachtig", "onverdacht"),
    #        iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    # OF ook kijken naar de redoxklasse afwijking
    # als 1 redoxklasse maar is opgeschoven dan twijfelachtig, en meer dan 1 verdacht
    mutate(oordeel = ifelse(abs(redoxnr.x - redoxnr.y) == 0, "onverdacht",
                            ifelse(abs(redoxnr.x - redoxnr.y) == 1, "twijfelachtig",
                                   "verdacht")),
           iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    filter(oordeel != "onverdacht") %>%
    rename(redoxklasse_VAL = redoxklasse.x,
           redoxklasse_HIS = redoxklasse.y)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "putfilters waar de redoxklasse afwijkt", 
                           "t.o.v. de voorgaande meetronde.",
                           "Bij", nrow(res %>% filter(oordeel == "twijfelachtig")),
                           "putfitlers gaat het om 1 klasse verschuiving (twijfelachtig).",
                           "Bij", nrow(res %>% filter(oordeel == "verdacht")), 
                           "putfilters gaat het om >1 klasse verschuiving (verdacht).")
  
  # Als een putcode een afwijkende redoxklasse heeft, print deze
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% select(putcode, filter,
                           redoxklasse_VAL,
                           redoxklasse_HIS,
                           oordeel))
      
    } else {
      # als er geen afwijkende redoxklasses gevonden zijn
      print(paste("Er zijn geen putcodes waar de redoxklasse van het grondwater",
                  "is veranderd t.o.v. de voorgaande redoxklasse."))
    }
  }
  
  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    group_by(monsterid) %>%
    mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    filter(iden %in% res$iden) %>%
    # voeg resultaten test toe
    left_join(., res %>% select(iden, redoxklasse_VAL, redoxklasse_HIS, oordeel), by = "iden") %>%
    select(qcid, monsterid, jaar, maand, dag, putcode, filter,
           redoxklasse_VAL, redoxklasse_HIS, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df %>% filter(oordeel == "twijfelachtig") %>% distinct(qcid) %>% pull(qcid)
  verdacht_id <- resultaat_df %>% filter(oordeel == "verdacht") %>% distinct(qcid) %>% pull(qcid)
  test <- "QC0h"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = twijfel_id)
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "verdacht",
                                  ids = verdacht_id)
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
  
  # return beoordeelde monsters in d_metingen
  return(d_metingen)
}



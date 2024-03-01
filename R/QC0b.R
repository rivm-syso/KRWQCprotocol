#' QC0b. Controle landgebruik
#'
#' Controle op verandering in landgebruik bij de putlocatie. 
#' 
#' Controleer het huidige dominante landgebruikstype aan de hand van
#' meegeleverde veldgegevens en vergelijk dit met het bronbestand. 
#' De landgebruiksclassificatie als gebruikt in *Representativiteit KRW
#' Monitoringprogramma Grondwaterkwaliteit* wordt gehanteerd 
#' (Wattel-Koekkoek et al., 2009). Zie tabel 7.3. Indien het
#' landgebruikstype afwijkt, ken het concept QC oordeel twijfelachtig toe
#' aan het monster.
#'
#' @param d_veld dataframe met putcode en dominant landgebruik waargenomen in het veld
#' voor het bemonsterde jaar X welke vergeleken wordt met de informatie  
#' doorgegeven aan de BRO als referentie.
#' @param d_put BRO geregisteerde metadata behorende bij de put of putfilter.
#' @param d_metingen metingen bestand met monster ID's om bij afwijkingen in het
#' landgebruik te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0b <- function(d_veld, d_put, d_metingen, verbose = F) {
  
  # Test of relevante kolommen aanwezig zijn
  # alleen voor putnummer, xcoord en ycoord in d_veld en d_put?
  testKolommenVeld(d_veld)
  testKolommenPut(d_put)
  testKolommenMetingen(d_metingen)

  # Test of coordinaten in Nederland liggen
  testCoordinaten(d_veld)
  testCoordinaten(d_put)

  
  # Selecteer alleen putcode, jaar, landgebruik 
  d <- d_veld %>%
    dplyr::select(monsterid, putcode, landgebruik) 
  
  # Bestaan de opgegeven landgebruiken uit de juiste klassen?
  if(nrow(v1<-d %>% dplyr::filter(!landgebruik %in% valideLandgebruiken())) > 0) {
    stop("Landgebruiksklasse worden niet herkend, zie valideLandgebruiken()")
  }

  # Vergelijk landgebruik met opzoektabel van aangeleverde BRO data
  res <- merge(d, d_put %>% dplyr::select(putcode, landgebruik), by = "putcode") %>%
    dplyr::mutate(oordeel = ifelse(landgebruik.x != landgebruik.y, 
                            "twijfelachtig", "onverdacht"),
           iden = monsterid) %>%
    dplyr::filter(oordeel == "twijfelachtig") %>%
    dplyr::rename(landgebruik_val = landgebruik.x,
           landgebruik_BRO = landgebruik.y)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                           "bemonsterde putlocaties met landgebruik afwijkend",
                           "van de BRO informatie")
  
  # Als er afwijkende landgebruiken zijn, print deze
  if(verbose) {
    if(nrow(res) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res %>% dplyr::select(putcode, landgebruik_val, landgebruik_BRO))
    
    } else {
    # als elk landgebruik identiek is aan het totaalbestand
    print("Voor elke putfilter is het landgebruik identiek aan de BRO geregistreerde informatie")
    }
  }

  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                            "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel == "twijfelachtig") %>%
    dplyr::left_join(., res %>% dplyr::select(iden, landgebruik_val, landgebruik_BRO),by="iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter, 
           landgebruik_val, landgebruik_BRO, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  verdacht_id <- resultaat_df$qcid
  test <- "QC0b"
  
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


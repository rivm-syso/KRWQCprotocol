#' QC0d. Controle afdekking put 
#'
#' Controle op contaminatie van het grondwater.
#' 
#' Controleer of de veldmedewerker een put zonder een goed sluitende 
#' afdekking heeft geconstateerd. Heeft deze bijzonderheden rondom
#' de afdekking genoteerd, ken het concept QG oordeel twijfelachtig
#' toe aan het monster.
#'
#' @param dir directory waarin het  QC0d_afdekking_put.csv bestand
#' staat
#' @param d_metingen metingen bestand met monster ID's om bij verkeerde afdekking 
#' te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC0d <- function(dir, d_metingen, verbose = F) {
  
  # Deze controle wordt door de beheerders uitgevoerd en bijgehouden
  # Deze informatie is wel benodigd voor het QC status
  # met QC0d_create_file wordt de benodigde datatabel gegenereerd
  # welke door de beheerders ingevuld moet worden
  
  # CSV Tabel inlezen ingevuld door beheerders
  fname <- "QC0d_afdekking_put.csv"
  if(!dir.exists(dir)) {
    stop("directory bestaat niet")
  }
  
  # check of bestanden aanwezig zijn
  if(!file.exists(file.path(dir, fname))) {
    stop("CSV bestand met controles beschadiging bestaat niet. Run eerst QC0d_create_file.")
  }
  
  # Laad CSV bestand in met put afdekkingen
  d <- read.csv(file.path(dir, fname))
  
  # Check datasets
  testKolommenMetingen(d_metingen)
  testKolommenQC0d(d)
  
  # Verwijder lege rijen met NA's
  d <- d %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.)))
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(d), 
                           "bemonsterde putcodes waar de putafdekking niet in orde was.")
  
  # Als er afwijkende beschadigingen zijn, print deze
  if(verbose) {  
    if(nrow(d) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(d %>% dplyr::select(putcode, afdekking_put))
      
    } else {
      # als er geen beschadigingen/afwijking genoemd zijn 
      print("Voor elke putcode was de putafdekking in orde.")
    }
  }
  
  # voeg concept oordeel van afwijkende putten toe aan monsters op die locaties in betreffende meetronde
  resultaat_df <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = monsterid) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% d$iden,
                                   "twijfelachtig", "onverdacht")) %>%
    dplyr::filter(oordeel != "onverdacht") %>%
    dplyr::left_join(., d %>% dplyr::select(iden, afdekking_put), by = "iden") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter,
                  afdekking_put, oordeel)
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfelachtig_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid) %>%
    dplyr::pull(qcid)
  
  test <- "QC0d"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = twijfelachtig_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = resultaat_df)
  
  # return beoordeelde putten in d_veld en beoordeelde monsters in d_metingen
  return(d_metingen)
}


#' QC0d_create_file. Maak bestand voor Controle QC0d
#'
#' Maak een csv bestand aan wat ingevuld moet worden voor de
#' handmatige controle van de representativiteit van de put en filter.
#'
#' @param dir directory waarin het bestand aangemaakt moet worden
#'
#' Deze functie maakt een csv bestand aan genaamd
#' 'QC0d_afdekking_put.csv'. Dit bestand wordt aangemaakt in de
#' opgegeven directory. Als het bestand al bestaat dan wordt het niet
#' overschreven.
#'
#' Het aangemaakte bestand moet vervolgens met de hand ingevuld worden
#' met behulp van de informatie afkomstig van de beheerders /
#' veldmedewerkers.
#'
#' Als het bestand correct is ingevuld dan kan het verwerkt worden
#' met behulp van  [QC0d()]. Let op, [QC0d()] verwacht dezelfde
#' bestandsnaam, verander deze dus niet.
#'
#' @export
#'
#'

QC0d_create_file <- function(dir) {
  
  fname <- "QC0d_afdekking_put.csv"
  
  if(!dir.exists(dir)) {
    stop("directory bestaat niet")
  }
  
  
  if(!file.exists(file.path(dir, fname))) {
    d <- data.frame(monsterid = "", 
                    afdekking_put = "")
    write.csv(d, file.path(dir, fname), row.names = FALSE)
  }
}



testKolommenQC0d <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor CSV tabellen
  
  kolommen <- c("monsterid", "afdekking_put")
  
  if(length(setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
}  

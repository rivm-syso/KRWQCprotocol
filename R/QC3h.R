#' QC3h. Controle NO3 en NH4
#'
#' Controleer relatie NO3 en NH4
#' 
#' Controleer per putfilter of aan de volgende condities wordt voldaan:
#' 1) NO3 >0.5 mg/l & NH4 >2 mg/l
#' 2) NO3 >0.5 mg/l & NH4 > NO3
#' 
#' Indien aan ten minste één van de condities wordt voldaan, ken het concept
#' QC oordeel twijfelachtig toe aan de betreffende parameters. Vergelijk 
#' veldwaarden enkel met veldwaarden en labwaarden enkel met labwaarden.
#'
#' @param d_metingen metingen bestand met monster ID's om bij afwijkingen 
#' te kunnen markeren.
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#' 
#'
#' @export
#'

QC3h <- function(d_metingen, verbose = F) {

    # Test of relevante kolommen aanwezig zijn
    # deze hulpfuncties staan in utils, deze eerst nu nog runnen.
    testKolommenMetingen(d_metingen)

    # Selecteer alleen NO3 en NH4 uit de dataset
    d <- d_metingen %>%
        dplyr::filter(stringr::str_detect(parameter, "no3|nh4")) 
    n_params <- unique(d$parameter)

    # Check of er nu maar 2 parameters beschikbaar zijn
    if(dplyr::n_distinct(d$parameter) < 2) {
        stop("Geen NO3 of NH4 beschikbaar")
    }
    if(dplyr::n_distinct(d$parameter) > 2) {
        stop("Meer dan 2 parameters voor NO3 en NH4")
    }

    # als no3 als no3_n staat, dan vervangen naar no3, idem voor nh4 -> geval voor LMG  
    d$parameter <- d$parameter %>%
        dplyr::recode("nh4_n" = "nh4",
                      "no3_n" = "no3")

    # Vergelijk NO3 en NH4
    res <- d %>%
        dplyr::select(-c(qcid,detectieteken, rapportagegrens)) %>%
        tidyr::pivot_wider(names_from = parameter,
                           values_from = waarde) %>%
        # NO3 en NH4 staan in stikstof, dus omrekenen
        dplyr::mutate(nh4 = nh4 * 1.2878,
                      no3 = no3 * 4.4268) %>%
        dplyr::mutate(oordeel = ifelse(no3 > 0.5 & nh4 > 2 |
                                       no3 > 0.5 & nh4 > no3,
                                   "twijfelachtig", "onverdacht"),
                      iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
        dplyr::filter(oordeel != "onverdacht")

    rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                             "monsters waar de relatie tussen NO3 en NH4",
                             "afwijkend is.")

    # Als printen gewenst is (T)
    if(verbose) {
        if(nrow(res) > 0) {
            write.table(
                        rapportageTekst,
                        row.names = F, col.names = F)
            print(res %>% dplyr::select(-iden))

        } else {
            print("Voor alle monsters klopt de relatie tussen NO3 en NH4.")
        }
    }

    # voeg concept oordeel van afwijkende NO3-NH4 relatie toe aan monsters op die locaties in betreffende meetronde
    cols <- c(10, 8, 7)
    cols2 <- c(1:5, 7, 6, 14, 15, 13)

    resultaat_df <- d_metingen %>%
        dplyr::group_by(monsterid) %>%
        dplyr::filter(parameter%in%n_params) %>%
        dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
        dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                       "twijfelachtig", "onverdacht")) %>%
        dplyr::filter(oordeel != "onverdacht") %>%
        dplyr::left_join(., res[, cols], by="iden") 
    resultaat_df <- resultaat_df[, cols2]

    # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
    twijfel_id <- resultaat_df %>% 
        dplyr::filter(oordeel == "twijfelachtig") %>% 
        dplyr::distinct(qcid) %>%
        dplyr::pull(qcid)

    test <- "QC3h"

    d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                    test = test,
                                    oordeel = "twijfelachtig",
                                    ids = twijfel_id)
    d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                       test = test,
                                       tekst = rapportageTekst)
    d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                      test = test,
                                      resultaat = resultaat_df)

    return(d_metingen)
}


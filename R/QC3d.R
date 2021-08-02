#' QC3d. Controle EC-veld en EC-lab
#'
#' Vergelijk EC-veld en EC-lab
#'
#' De signaleringswaarde voor monsters is delta-EC > 10 procent.
#' Als de delta-EC boven de signaleringswaarde ligt, ken het
#' concept QC oordeel twijfelachtig toe aan het monster.
#' 
#' De benodigde namen voor ec lab en ec veld zijn ec en ecv.
#'         
#' @param d_metingen dataframe met metingen
#' @param d_parameter dataframe met parameter informatie
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC3d <- function(d_metingen, d_parameter, verbose = F) {
    
    # Check datasets op kolommen en unieke informatie
    testKolommenMetingen(d_metingen)
    testKolommenParameter(d_parameter)
    
    # pH en HCO3 naam aanpassen alleen voor LMG
    d <- d_metingen
    d$parameter <- d$parameter %>%
        recode("ec_5__veld" = "ecv",
               .default = d$parameter)
    
    # selecteer EC veld en lab gegevens
    d <- d %>%
        dplyr::filter(parameter %in% c("ec", "ecv"))
    
    
    # Check of EC veld en lab gegevens beschikbaar zijn
    if(dplyr::n_distinct(d$parameter) < 2) {
        stop("Geen veld of lab EC aanwezig")
    }
    if(dplyr::n_distinct(d$parameter) > 2) {
        stop("Meer dan 2 parameters voor veld en lab EC")
    }
    
    # Check op zelfde eenheden voor EC ? 
    # dit staat in d_parameter
    d1 <- d_parameter %>%
        dplyr::filter(naam %in% d$parameter)
    if(dplyr::n_distinct(d1$eenheid) > 1) {
        stop("Er zijn meerdere eenheden voor EC opgegeven")
    }
    
    # Vergelijk EC lab en EC veld
    res <- d %>%
        dplyr::select(-c(qcid, detectieteken, rapportagegrens)) %>%
        tidyr::pivot_wider(names_from = parameter,
                           values_from = waarde) %>%
        dplyr::mutate(oordeel = ifelse(abs(ec - ecv) > 0.1 * ec |
                                           abs(ec - ecv) > 0.1 * ecv,
                                       "twijfelachtig", "onverdacht"),
                      iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
        dplyr::filter(oordeel != "onverdacht")
    
    rapportageTekst <- paste("Er zijn in totaal", nrow(res), 
                             "metingen waar EC-lab en EC-veld 10% of meer afwijken")
    
    if(verbose) {
        if(nrow(res) > 0 ) {
            print(rapportageTekst)
            
        } else {
            print(paste("Er zijn geen metingen waar EC-lab en EC-veld 10% of meer afwijken"))
        }
    }
    
    # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
    cols <- c(1:5, 7, 6, 14, 15, 13)
    
    resultaat_df <- d_metingen %>%
        dplyr::group_by(monsterid) %>%
        dplyr::mutate(iden = paste(putcode, jaar, maand, dag, sep = "-")) %>%
        dplyr::mutate(oordeel = ifelse(iden %in% res$iden,
                                       "twijfelachtig", "onverdacht")) %>%
        dplyr::filter(oordeel != "onverdacht") %>%
        dplyr::left_join(., res %>% select(ec, ecv, iden)) 
    resultaat_df <- resultaat_df[, cols]
    
    twijfel_id <- resultaat_df %>% 
        dplyr::filter(oordeel == "twijfelachtig") %>% 
        dplyr::distinct(qcid)
    
    test <- "QC3d"
    
    d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                    test = test,
                                    oordeel = unique(resultaat_df$oordeel)[1],
                                    ids = twijfel_id)
    d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                       test = test,
                                       tekst = rapportageTekst)
    d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                      test = test,
                                      resultaat = resultaat_df)
    
    return(d_metingen)
    
}


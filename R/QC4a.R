#' QC4a. Controle consistentie metingen met historische gegevens
#'
#' Beoordeling waarschijnlijk meting op basis van consistentie
#' met historische gegevens. 
#'      
#' Per putfilter wordt voor elke parameter een vergelijking met het 
#' verleden gemaakt. Dit kan worden gedaan voor ieder meetjaar, indien er 
#' voldoende voorgaande metingen ter beschikking zijn. De consistentie van 
#' metingen met de historische meetwaarden wordt in standaardafwijking 
#' uitgedrukt. Een visuele inspectie vindt plaats voor de metingen met een
#' standaardafwijking van >3.5 ten opzichte van de historische meetreeks. Van
#' de metingen die na visuele inspectie als afwijkend van de tijdreeks worden
#' beoordeeld, ken het concept QC oordeel twijfelachtig toe aan de betreffende
#' parameter. 
#' Zie bijlage II van het QC protocol voor de methodiek ter beoordeling van
#' de consistentie van meetwaarden.       
#'                       
#' @param d_metingen dataframe met metingen
#' @param d_parameter dataframe met parameter informatie
#' @param meetronde meetjaar welke gevalideerd dient te worden. Staat standaard
#' op de laatste meetjaar uit het databestand.
#' @param zscore z-score waarbij een meeting wordt bestempeld met twijfelachtig.
#' staat standaard op 3.5. Alle beschrijvingen zijn gebaseerd op z-score 3.5.
#' @param plt Voeg tijdserie plots per reeks en stof toe aan resultaat
#' @param plt_put_reeks Maak plots van alle putten waarbij 1 of meer parameters
#' een z-score > 3.5 hebben (vereist plt = T). Staat standaard op F. 
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @importFrom stringr str_detect
#'
#' @export
#'


QC4a <- function(d_metingen, d_parameter, 
                 meetronde = max(d_metingen$jaar), 
                 zscore = 3.5,
                 plt = T, plt_put_reeks = F,
                 verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Berekenen statistieken per reeks
  d <- d_metingen %>%
    # alleen labmetingen meenemen -> Aantal veldmetingen hebben waarde 0
    # wat mis gaat met log berekening
    dplyr::filter(!stringr::str_detect(parameter, "veld")) %>%
    # waardes 0 niet meenemen
    dplyr::mutate(waarde = ifelse(waarde == 0, NA, waarde)) %>%
    # verwijder NA's uit waarde kolom
    tidyr::drop_na(waarde) %>%
    # waarde <RG aanpassen naar 0.5 * RG. Geen ROS regressie
    dplyr::mutate(waarde = ifelse(detectieteken == "<",
                                  0.5 * waarde, waarde)) %>%
    dplyr::group_by(putcode, filter, parameter) %>%
    # bereken gemiddelde en sd per reeks voor lognormale verdeling
    dplyr::mutate(n.meetjaar = dplyr::n_distinct(jaar),
                  n.meting = length(waarde),
                  logobs = log(waarde),
                  loggem = sapply(1:n(), function(i) mean(log(waarde[-i]))),
                  logsdv = sapply(1:n(), function(i) sd(log(waarde[-i])))) %>%
    # bepaal z-score. Indien SD = 0, dan ook z-score = 0
    dplyr::mutate(logz = ifelse(logsdv == 0, 0 ,
                                (logobs - loggem) / logsdv) ) %>%
    # voeg oordeel toe
    # reeksen met <3 metingen/meetjaren zijn niet uitvoerbaar
    # z-score >3.5 is twijfelachtig
    dplyr::mutate(oordeel = ifelse(n.meetjaar < 3, "test niet uitvoerbaar",
                            ifelse(n.meetjaar >= 3 & abs(logz) > zscore, "twijfelachtig",
                                   "onverdacht"))) %>%
    # voeg type toe voor plotjes nieuwe en oude meetrondes
    dplyr::mutate(type = ifelse(jaar == meetronde, "nieuw", "oud")) %>%
    dplyr::ungroup()
  
  rapportageTekst <- paste("Er zijn in totaal", 
                           nrow(d %>% dplyr::filter(oordeel == "twijfelachtig")), 
                           "metingen waar de afwijking >", zscore, "standaarddeviaties",
                           "is t.o.v. de historische meetreeks.")
  
  if(verbose) {
    if(nrow(d %>% dplyr::filter(oordeel == "twijfelachtig")) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er zijn geen metingen waar de afwijking >", zscore,
                  "standaarddeviaties is."))
    }
  }
  
  ## Visualiseren per stof voor historie ##
  plot_list_param = list()
  if(plt) {
    
    d <- d %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(xrank = dplyr::dense_rank(loggem)) %>%
      # kleur punten
      dplyr::mutate(kleur = dplyr::case_when(
        type == "oud" ~ "grey90",
        abs(logz) <= 2 ~ "grey70",
        abs(logz) > 3.5 ~ "black",
        abs(logz) > 3 ~ "red3",
        abs(logz) > 2.5 ~ "orange",
        abs(logz) > 2 ~ "green",
        # eventueel nog vreemd water kleuren? -> SS >100
        ),
        kleur = factor(kleur,
                       levels = c("grey90", "grey70", "green", 
                                  "orange", "red3", "black"),
                       ordered = TRUE)) %>%
      # definieer printvolgorde zodat meest afwijkende punten altijd 
      # bovenaan worden geplot
      dplyr::mutate(printvolgorde = dplyr::case_when(
        kleur == "grey90" ~ 1,
        kleur == "grey70" ~ 2,
        kleur == "green" ~ 3,
        kleur == "orange" ~ 4,
        kleur == "red3" ~ 5,
        kleur == "black" ~ 6))
    
    # plot
    col <- c("grey90", "grey70", "green", "orange", "red3", "black")
    names(col) <- c("grey90", "grey70", "green", "orange", "red3", "black")
    
    # maak figuren per parameter en voeg samen in een list
    for(i in unique(d$parameter)) {
      
      res <- d %>%
        dplyr::filter(parameter == i) 
        # eventueel nog alleen de parameters selecteren
        # welke in gekozen meetronde ook bemonsterd zijn om te plotten?
      
      # deze code later in aparte functie plaatsen?
      plot <- ggplot2::ggplot() +
        # historische data als punten
        ggplot2::geom_point(data = res %>% dplyr::filter(type == "oud"),
                   ggplot2::aes(x = xrank,
                       y = logobs,
                       group = xrank,
                       color = kleur),
                   shape = 1, size = 0.3) +
        # plot nieuwe data als punten
        ggplot2::geom_point(data = res %>% dplyr::filter(type == "nieuw") %>%
                              dplyr::arrange(printvolgorde),
                   ggplot2::aes(x = xrank,
                       y = logobs,
                       color = kleur),
                   shape = 0, size = 1) +
        # kleuren van punten
        ggplot2::scale_color_manual(name = "",
                           values = col,
                           labels = c("hist. meting",
                                      "<2 sd",
                                      "<2.5 sd",
                                      "<3 sd",
                                      "<3.5 sd",
                                      ">3.5 sd")) +
        ggplot2::theme_bw() +
        # assen 
        ggplot2::xlab("filters in rangorde") +
        ggplot2::ylab(paste0("10log (", unique(res$parameter), ")")) +
        ggplot2::ggtitle(paste(unique(res$parameter), meetronde)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      plot_list_param[[i]] <- plot
    }
  }
    
  
  ## Visualiseren reeks niveau ##
  # alleen voor de reeksen waar een meting >3.5 sd afwijkt
  afwijking <- d %>%
    # bij een afwijking wil je beide filters visualiseren 
    # ipv alleen het filter waar de afwijking zit
    # zodat evt filter verwisselingen bekeken kunnen worden
    # dus alleen putcode en parameter koppelen
    dplyr::mutate(reeks = paste(parameter, putcode, sep = "-")) %>%
    dplyr::filter(oordeel == "twijfelachtig")
  
  plot_list_reeks <- list()
  if(nrow(afwijking) > 0) {
    
    res <- d %>%
      dplyr::mutate(putfilter = paste(putcode, filter, sep = "-"),
                    reeks = paste(parameter, putcode, sep = "-")) %>%
      # voeg eenheid toe voor plot as
      dplyr::mutate(eenheid = d_parameter[match(parameter, d_parameter$parameter), "eenheid"]) %>%
      # selecteer reeksen met meting >3.5 sd
      dplyr::filter(if (plt_put_reeks) putcode %in% afwijking$putcode 
                    else reeks %in% afwijking$reeks)
      
      # test set voor figuren op reeks niveau voor beide filters
      # res <- d %>%
      #   # waardes <RG als RG plotten ivm 0.5 *RG?
      #   # mutate(waarde = ifelse(detectieteken == "<",
      #   #                        waarde * 2, waarde)) %>%
      #   mutate(putfilter = paste(putcode, filter, sep = "-"),
      #          reeks = paste(parameter, putcode, sep = "-")) %>%
      #   # voeg eenheid toe voor plot as
      #   mutate(eenheid = d_parameter[match(parameter, d_parameter$naam), 5]) %>%
      #   # selecteer reeksen met meting >3.5 sd
      #   filter(putcode == 1, parameter == "al") %>%
      #   mutate(oordeel = ifelse(filter == 1 & jaar == 2019,
      #                           "twijfelachtig", oordeel))
      #   filter(putcode == 296, parameter == "ca") %>%
      #   mutate(oordeel = ifelse(filter == 1 & jaar == 2019,
      #                           "twijfelachtig", oordeel))
      
    for(i in unique(res$reeks)) {
      
      dat <- res %>%
        filter(reeks == i) %>%
        mutate(detectieteken = ifelse(detectieteken == "",
                                      "> RG", "< RG"))
      
      plot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = dat, 
                   ggplot2::aes(x = jaar,
                       y = waarde,
                       color = putfilter,
                       shape = detectieteken)) +
        ggplot2::geom_point(data = dat %>% dplyr::filter(oordeel == "twijfelachtig"),
                   ggplot2::aes(x = jaar,
                       y = waarde,
                       shape = oordeel),
                   shape = 13, size = 4, stroke = 0.4) +
        ggplot2::theme_bw() +
        ggplot2::scale_shape_manual(values = c(16, 1)) + 
        # assen 
        ggplot2::labs(title = paste(unique(dat$parameter), meetronde),
                      subtitle = paste("gemarkeerde meting is >", zscore, "sd en",
                              "metingen <RG = 0,5*RG")) +
        ggplot2::xlab("") +
        ggplot2::ylab(paste0(unique(dat$parameter), " [", unique(dat$eenheid), "]")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(hjust = 0.5))
        
      plot_list_reeks[[i]] <- plot
      
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  # zowel afwijkingende dataset metingen als de plots
  resultaat_df <- d %>%
    dplyr::select(-c("type", "xrank", "kleur", "printvolgorde")) %>%
    dplyr::filter(oordeel != "onverdacht")

  twijfel_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "twijfelachtig") %>% 
    dplyr::distinct(qcid) %>%
    dplyr::pull(qcid)
  
  test <- "QC4a"
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = test,
                                  oordeel = "twijfelachtig",
                                  ids = twijfel_id)
  d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                     test = test,
                                     tekst = rapportageTekst)
  # voeg twijfelachtige metingen toe
  # voeg figuren per parameter toe
  # voeg figuren afwijkende metingen per reeks toe
  d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                    test = test,
                                    resultaat = list(tabel = resultaat_df,
                                                     plot_param = plot_list_param,
                                                     plot_reeks = plot_list_reeks))

  return(d_metingen)
  
}


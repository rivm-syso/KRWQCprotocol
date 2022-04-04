# helper functions for QC tests, these functions validate the data
# format

testKolommenVeld <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor veldtabel
  
  kolommen_numeriek <- c("qcid", "jaar", "maand", "dag", 
                         "xcoord", "ycoord", "okf", "gws_voor", "gws_na")  # eventueel nog kolom dag, maand, jaar apart
  kolommen <- c("putcode", "filter", "landgebruik", 
                "bem_app", "bem_proc",  kolommen_numeriek)
  
  if(length(setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
  
  for(i in kolommen_numeriek) {
    if(!is.numeric(d[[i]])) {
      stop(paste("kolom", i, "is niet numeriek"))
    }
  }
}

testKolommenPut <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor puttabel
  
  kolommen_numeriek <- c("qcid", "xcoord", "ycoord")        # eventueel nog maaiveld toevoegen, maar is deze wel nodig binnen QC?
  kolommen <- c("putcode", "rgdputnummer", "landgebruik", 
                "maaiveld", "provincie", kolommen_numeriek)
  
  if(length(setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
  
  for(i in kolommen_numeriek) {
    if(!is.numeric(d[[i]])) {
      stop(paste("kolom", i, "is niet numeriek"))
    }
  }
}

testKolommenFilter <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor putfiltertabel
  
  kolommen_numeriek <- c("qcid", "diepte_onder", "diepte_boven")  
  kolommen <- c("putcode", "filter", 
                "watertype", "redoxklasse", kolommen_numeriek)

#   print(d)
#   print(names(d))
#   print(setdiff(kolommen,names(d)))
# 
  if(length(base::setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
  
  for(i in kolommen_numeriek) {
    if(!is.numeric(d[[i]])) {
      stop(paste("kolom", i, "is niet numeriek"))
    }
  }

  return(TRUE)
}  

testKolommenParameter <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor parametertabel
  
  kolommen <- c("qcid", "parameter", "omschrijving", "aquocode", "cas", "eenheid",
                "bem_apparaat", "bem_procedure", 
                "waarde_techniek", "waarde_procedure")

  if(length(base::setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
} 

testKolommenMetingen <- function(d) {
  # test of verplichte kolommen aanwezig zijn voor metingentabel
  
  kolommen_numeriek <- c("qcid", "monsterid", "jaar", "maand", "dag", "rapportagegrens", "waarde")  
  kolommen <- c("filter", "putcode", "parameter", "detectieteken", kolommen_numeriek)

  if (length(base::setdiff(kolommen, names(d))) > 0) {
    stop("kolommen ontbreken of worden niet herkend")
  }
  
  for (i in kolommen_numeriek) {
    if (!is.numeric(d[[i]])) {
      stop(paste("kolom", i, "is niet numeriek"))
    }
  }
  return(TRUE)
}


testCoordinaten <- function(d){
  # test of coordinaten binnen bounding-box van Nederland vallen,
  # coordinaten in meters
  for(i in c("xcoord", "ycoord")) {
    if(!is.numeric(d[[i]])) {
      stop(paste("kolom", i, "is niet numeriek"))
    }
  }
  
  if(any(is.na(c(d$xcoord,d$ycoord)))){
    stop("enkele coordinaten ontbreken")
  }
  
  if(min(d$xcoord)<9000||max(d$xcoord)>280000) {
    stop("x-coordinaten buiten bereik")
  }
  
  if(min(d$ycoord)<300000||max(d$ycoord)>624000) {
    stop("y-coordinaten buiten bereik")
  }
  
}

valideLandgebruiken <- function(d) {
  # returns valide landgebruiksklasses
  # naar Wattel-Koekoek 2009, Tabel 7.2
  # https://www.rivm.nl/bibliotheek/rapporten/680721003.pdf  
  # kloppen deze klasses? In LMG komen 8 klasses voor..
  return(c("akkerbouw", "bebouwd", 
           "bos, natuur en water",
           "glastuinbouw, bomen- en bollenteelt",
           "grasland", 
           "industrie"))
}

valideWatertypes <- function(d) {
  # returns valide watertypes
  # deze staan nog niet goed in LMG ook
  return(c("zoet", "brak/zout"))
}
  
valideRedoxklasses <- function(d) {
  # returns valide redoxklasses
  # In LMG staat fermenterend ipv methanogeen
  # Meng wel of niet opnemen? In protocol staat dat meng onder Fe-anox komt te vallen..
  return(c("subox", "mn-anox", "fe-anox", "so4-red", "methano", "onbe"))
}  
  
# hulp functie om te kijken of parameter informatie uniek is
valideParamInfo <- function(d) {
  d <- d %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(n.Aquocode = dplyr::n_distinct(aquocode),
              n.cas = dplyr::n_distinct(cas),
              n.eenheid = dplyr::n_distinct(eenheid),
              n.bem_app = dplyr::n_distinct(bem_apparaat),
              n.bem_proc = dplyr::n_distinct(bem_procedure),
              n.techniek = dplyr::n_distinct(waarde_techniek),
              n.proc = dplyr::n_distinct(waarde_procedure)) %>%
    dplyr::filter(rowSums(.[-1]) != 7)
  
  if(nrow(d) > 0) {
    stop(paste("parameter", d$naam[1], " heeft geen unieke informatie"))
  }
}
  

rowAny <- function(x){
  rowSums(x) > 0
}  

qcidNietUitvoerbaar <- function(d, d_metingen, benodigdeKolommen){
  niet_uitvoerbaar <- d %>% 
    filter(
      rowAny(
        across(
          .cols = all_of(benodigdeKolommen),
          .fns = ~ is.na(.x)
        )
      )
    ) %>% mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-"))
  
  id <- d_metingen %>%
    group_by(monsterid) %>%
    mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    filter(iden %in% niet_uitvoerbaar$iden) %>% 
    ungroup() %>% 
    select(qcid, iden)
  
  niet_uitvoerbaar <- left_join(niet_uitvoerbaar, id, by = "iden")
  niet_uitvoerbaar_id <- niet_uitvoerbaar %>% distinct(qcid) %>% pull(qcid)
  
  return(niet_uitvoerbaar_id)
}

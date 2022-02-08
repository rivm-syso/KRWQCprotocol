#' QC5. Status
#'
#' In QC5 wordt aan de metingen een QC status toegekend.
#'      
#' Bij elke individuele meting van een parameter in het grondwatersamenstellings-
#' onderzoek geeft de bronhouder (of een externe partij in opdracht van de 
#' brondhouder) een eindoordeel over de kwaliteit van de meting. Dit eindoordeel
#' wordt gevormd aan de hand van een voor het hele grondwatersamenstellings-
#' onderzoek gebruikte beoordelingsprocedure. Het eindoordeel wordt geregistreerd
#' in de status kwaliteitscontrole. Het is een oordeel over de kwaliteit van de
#' meting van de parameter, geen oordeel over het grondwatermonster als geheel.
#' 
#' Er wordt gebruik gemaakt van vier statusdefinities: goedgekeurd, afgekeurd,
#' onbekend en onbeslist. Zie QC5: Status van het QC protocol voor de methodiek.
#'                       
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). 
#' Staat standaard op F.
#'
#' @return metingen bestand met eindoordeel per meting.
#'
#' @export
#'


QC5 <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  if(qcout_attrexists(d_metingen)){
    # Lijst met alle QC namen
    qcn <- c("QC0a", "QC0b", "QC0c", "QC0d", "QC0e", "QC0f", "QC0g", "QC0h", 
             "QC1a", "QC1b", "QC1c", "QC1e", "QC1f", 
             "QC2a", "QC2b", "QC2c",
             "QC3a", "QC3b", "QC3c", "QC3d", "QC3e", "QC3f", "QC3g", "QC3h", 
             "QC4a", "QC4b")
    # Lijst met minimaal benodigde QC namen
    qcn_req <- c("QC0a", "QC0c", "QC0d", "QC0e", 
                 "QC1f", 
                 "QC2a", "QC2b", "QC2c",
                 "QC3c", "QC3d", "QC3e", "QC3f", "QC3g", "QC3h", 
                 "QC4a", "QC4b")
    
    # Controleer of alle namen voorkomen in d_metingen
    x_attr <- attr(d_metingen, "qcout")
    qcn_inx <- qcn %in% names(x_attr)
    qcn_req_inx <- qcn_req %in% names(x_attr)
    
    # Stop als een QC test niet aanwezig is
    if(!all(qcn_req_inx)){
      stop(paste0(qcn_req[!qcn_req_inx], " niet aanwezig\n"))
    } else{
      if(!all(qcn_inx)){
        warning(paste0(qcn[!qcn_inx], " niet aanwezig\n"))
      } 
    }
    
    # Controle op format resultaten
    # for(i in qcn){
    #   if(!is.list(x_attr[[i]][["resultaat"]])){
    #     stop(paste0(i, " resultaat niet als list"))
    #   }
    #   
    # }
    
  } else{
    stop("qcout bevat geen attributen")
  }
  
  # Verzamel de testresultaten uit een data.frame met tests. De
  # testresultaten worden per regel in de data.frame aangegeven.
  d <- collect_result(d_metingen)
  
  # QC eindoordeel
  # QC namen die voorkomen in d (niet alle namen komen voor in d)
  qcn_ind <- qcn[qcn %in% names(d)]
  
  # ids afgekeurd
  ids_a1 <- if ("QC0a" %in% qcn_ind) {d %>% filter(QC0a == "verdacht") %>% pull(qcid)}
  ids_a2 <- if ("QC0e" %in% qcn_ind) {d %>% filter(QC0e == "twijfelachtig") %>% pull(qcid)}
  ids_a3 <- if ("QC1f" %in% qcn_ind) {d %>% filter(QC1f == "verdacht") %>% pull(qcid)}
  ids_a4 <- if (all(c("QC2a", "QC4a") %in% qcn_ind)) {d %>% filter(QC2a == "verdacht" & QC4a == "twijfelachtig") %>% pull(qcid)}
  ids_a5 <- if ("QC4b" %in% qcn_ind) {d %>% filter(QC4b == "verdacht") %>% pull(qcid)}
  
  # ids onbeslist
  if("QC4a" %in% qcn_ind){
    v <- d %>% filter(QC4a == "twijfelachtig")
    ids_o1 <- if ("QC0a" %in% qcn_ind) {v %>% filter(QC0a == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o2 <- if ("QC0c" %in% qcn_ind) {v %>% filter(QC0c == "twijfelachtig" | QC0c == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o3 <- if ("QC0d" %in% qcn_ind) {v %>% filter(QC0d == "twijfelachtig" | QC0d == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o4 <- if ("QC0e" %in% qcn_ind) {v %>% filter(QC0e == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o5 <- if ("QC2a" %in% qcn_ind) {v %>% filter(QC2a == "twijfelachtig" | QC2a == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o6 <- if ("QC2b" %in% qcn_ind) {v %>% filter(QC2b == "twijfelachtig") %>% pull(qcid)}
    ids_o7 <- if ("QC2c" %in% qcn_ind) {v %>% filter(QC2c == "twijfelachtig") %>% pull(qcid)}
    ids_o8 <- if ("QC3c" %in% qcn_ind) {v %>% filter(QC3c == "twijfelachtig" | QC3c == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o9 <- if ("QC3d" %in% qcn_ind) {v %>% filter(QC3d == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o10 <- if ("QC3e" %in% qcn_ind) {v %>% filter(QC3e == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o11 <- if ("QC3f" %in% qcn_ind) {v %>% filter(QC3f == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o12 <- if ("QC3g" %in% qcn_ind) {v %>% filter(QC3g == "twijfelachtig" | QC3g == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o13 <- if ("QC3h" %in% qcn_ind) {v %>% filter(QC3h == "twijfelachtig" | QC3h == "niet uitvoerbaar") %>% pull(qcid)}
    ids_o14 <- v %>% pull(qcid)
  } else {
    for(i in 1:14) {
      temp_name <- NULL
      assign(paste0("ids_o", i), temp_name)
    }
  }
  
  # Overzicht aantal ids per beoordeling
  res <- bind_rows(
    # Afgekeurd
    tibble(ids = length(ids_a1), oordeel = "afgekeurd", QC0a = "verdacht"),
    tibble(ids = length(ids_a2), oordeel = "afgekeurd", QC0e = "twijfelachtig"),
    tibble(ids = length(ids_a3), oordeel = "afgekeurd", QC1f = "verdacht"),
    tibble(ids = length(ids_a4), oordeel = "afgekeurd", QC2a = "verdacht", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_a5), oordeel = "afgekeurd", QC4b = "verdacht"),
    # Onbeslist
    tibble(ids = length(ids_o1), oordeel = "onbeslist", QC0a = "niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o2), oordeel = "onbeslist", QC0c = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o3), oordeel = "onbeslist", QC0d = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o4), oordeel = "onbeslist", QC0e = "niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o5), oordeel = "onbeslist", QC2a = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o6), oordeel = "onbeslist", QC2b = "twijfelachtig", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o7), oordeel = "onbeslist", QC2c = "twijfelachtig", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o8), oordeel = "onbeslist", QC3c = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o9), oordeel = "onbeslist", QC3d = "niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o10), oordeel = "onbeslist", QC3e = "niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o11), oordeel = "onbeslist", QC3f = "niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o12), oordeel = "onbeslist", QC3g = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o13), oordeel = "onbeslist", QC3h = "twijfelachtig of niet uitvoerbaar", QC4a = "twijfelachtig"),
    tibble(ids = length(ids_o14), oordeel = "onbeslist", QC4a = "twijfelachtig"),
  )
  
  res <- res[, order(colnames(res))]
  
  ids_a <- unique(c(ids_a1, ids_a2, ids_a3, ids_a4, ids_a5))
  ids_o <- unique(c(ids_o1, ids_o2, ids_o3, ids_o4, ids_o5, 
                    ids_o6, ids_o7, ids_o8, ids_o9, ids_o10,
                    ids_o11, ids_o12, ids_o13, ids_o14))
  
  # Eindoordeel toekennen
  d_eindoordeel <- d %>% mutate(oordeel = "goedgekeurd")
  
  if(length(ids_o) > 0){
    d_eindoordeel <- d_eindoordeel %>% 
      mutate(oordeel = case_when(qcid %in% ids_o ~ "onbeslist",
                                 TRUE ~ oordeel))
  }
  if(length(ids_a) > 0){
    d_eindoordeel <- d_eindoordeel %>% 
      mutate(oordeel = case_when(qcid %in% ids_a ~ "afgekeurd",
                                 TRUE ~ oordeel)) 
  }
  
  rapportageTekst <- paste("Er zijn in totaal", d_eindoordeel %>% 
                             filter(oordeel == "afgekeurd") %>% count() %>% pull(), 
                           "metingen met het oordeel afgekeurd en", d_eindoordeel %>% 
                             filter(oordeel == "onbeslist") %>% count() %>% pull(),
                           "metingen met het oordeel onbeslist. Onderstaand een",
                           "overzicht met het aantal ids dat per test werd",
                           "beoordeeld. Een id kan door meerdere tests worden",
                           "beoordeeld.")
  
  # Als printen gewenst is (T)
  # Print overzicht van het aantal keer dat een oordeel is gegeven bij een bepaalde test
  if(verbose) {
    if(sum(res$ids) > 0) {
      write.table(
        rapportageTekst,
        row.names = F, col.names = F)
      print(res)
      
    } else {
      print("Alle metingen zijn goedgekeurd.")
    }
  }
  
  
  return(d_eindoordeel)
  
}


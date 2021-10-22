#' QC3e. Controle EC-gemeten & EC-berekend
#'
#' Vergelijking gemeten geleidbaarheid en berekende geleidbaarheid
#' 
#' Bereken de geleidbaarheid volgens de methode in Bijlage II van het protocol.
#' 
#' De signaleringswaarde voor monsters is delta-EC > 10 procent.
#' Als de delta-EC boven de signaleringswaarde ligt, ken het
#' concept QC oordeel twijfelachtig toe aan het monster.
#'         
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return metingen bestand met verdachte locaties/monsters. 
#'
#' @export
#'


QC3e <- function(d_metingen, verbose = F) {
  
  # Check datasets op kolommen en unieke informatie
  testKolommenMetingen(d_metingen)
  
  # Parameter naam aanpassen alleen voor LMG
  d <- d_metingen
  d$parameter <- d$parameter %>%
    dplyr::recode("ec_5__veld" = "ecv",
                  "h_5__veld" = "hv",
                  "hco3_veld" = "hco3v",
                  "nh4_n" = "nh4",
                  "no3_n" = "no3",
                  "po4_p" = "po4",
                  "ptot_p" = "ptot",
                  .default = d$parameter)
  
  # gegevens apart zetten om later qcid weer toe te voegen
  id <- d %>%
    dplyr::filter(parameter == "ecv") %>%
    dplyr::select(qcid, monsterid, jaar, maand, dag, putcode, filter) 
  
  # selecteer relevante ionen om mee te nemen in ionenbalans
  # h is pH en hv is pH-veld
  an <- c("cl", "hco3", "hco3v", "no3", "so4", "co3", "po4", "ptot")
  cat <- c("al", "ca", "fe", "k", "mg", "mn", "nh4", "na", "zn", "h", "hv")
  
  # Dataset in juiste format zetten benodigd voor berekening ionenbalans en EC
  # naar wide format en data goed zetten
  res <- d %>%
    # selecteer relevante ionen en pH ook meenemen -> omrekenen naar h30
    dplyr::filter(parameter %in% c(an, cat, "ecv")) %>%
    # alle NA's op 0 zetten, behalve pH en HCO3
    dplyr::mutate(waarde_ib = ifelse(!parameter %in% c("h", "hv", "hco3", "hco3v") & is.na(waarde),
                                     0, waarde)) %>%
    # soms staat RG als NA, < of "", eerst NA veranderen in ""
    dplyr::mutate(detectieteken = ifelse(is.na(detectieteken), "", 
                                         detectieteken)) %>%
    # waardes <RG niet meenemen maar op 0 zetten 
    dplyr::mutate(waarde_ib = ifelse(!parameter %in% c("h", "hv") & detectieteken != "",
                                     0, waarde_ib)) %>%
    # als geen pH bekend is, dan is de pH 7 
    dplyr::mutate(waarde_ib = ifelse(parameter %in% c("h", "hv") & is.na(waarde),
                                     7, waarde_ib)) %>%
    #  zet kolommen naar wide format
    dplyr::select(-c(qcid, detectieteken, rapportagegrens, waarde)) %>%
    tidyr::pivot_wider(., 
                       names_from = parameter,
                       names_glue = "x{parameter}",
                       values_from = waarde_ib) 
  
  # benodigde kolommen voor Patricks functies voor EC/ionenbalans
  benodigde_col <- 
    c("xal", "xca", "xcl", "xfe", "xhv", "xk", "xmg", "xmn", "xna", 
      "xnh4", "xno3", "xpo4", "xso4", "xecv", "xzn", "xhco3", "xco3")
  
  namen <- res %>%
    dplyr::select(-c(monsterid, jaar, maand, dag, filter, putcode)) 
  
  if(length(benodigde_col[!benodigde_col %in% names(namen)]) > 0) {
    warning(paste("benodigde kolommen ontbreken:",
                  benodigde_col[!benodigde_col %in% names(namen)]))
  }
  
  # Onderstaande functie BerekenGeleidbaarheid doet 2 dingen:
  # - rekent de concentraties om naar meq/l, stelt de ionenbalans op, voegt
  # evt HCO3 en PO4 toe en kent de benodigde methode toe waarmee de EC 
  # berekend gaat worden (functie MaakKolomMeth)
  # - berekend vervolgens de geleidbaarheid (EC25) volgens Stuyfzand (1984/7)
  
  d <- BerekenGeleidbaarheid(metveldgemiddelden = res, celcius = 25,
                             add_bicarbonate = T, add_phosphate = T)
  
  # Nu check op afwijking EC berekend en gemeten EC
  resultaat_df <- d %>%
    # selecteer relevante kolommen
    dplyr::select(monsterid, jaar, maand, dag, putcode, filter, 
                  pos, neg, ib, xecv, ec25, percentageverschil_xecv_ec25) %>%
    # selecteer afwijkingen >10%
    dplyr::filter(abs(percentageverschil_xecv_ec25) > 10) %>%
    # ken oordeel twijfelachtig toe
    dplyr::mutate(oordeel = "twijfelachtig") %>%
    # qcid weer toevoegen aan afwijkende EC waardes om weg te schrijven
    dplyr::left_join(., id, 
                     by = c("monsterid", "jaar", "maand", "dag", "putcode", "filter")) %>%
    select(qcid, monsterid, jaar, maand, dag, putcode, filter, pos, neg, ib,
           xecv, ec25, percentageverschil_xecv_ec25, oordeel)
  
  rapportageTekst <- paste("Er zijn in totaal", nrow(resultaat_df), 
                           "metingen waar EC-veld en berekende EC 10% of meer afwijken")
  
  if(verbose) {
    if(nrow(resultaat_df) > 0 ) {
      print(rapportageTekst)
      
    } else {
      print(paste("Er zijn geen metingen waar EC-veld en berekende EC 10% of meer afwijken"))
    }
  }
  
  # voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
  twijfel_id <- resultaat_df %>% filter(oordeel == "twijfelachtig") %>% distinct(qcid)
  test <- "QC3e"
  
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

# Onderstaande functies zijn methodes voor de EC berekening in QC3e
# uit Patricks functies 
Blanquet<-function(z=dataframeuitMaakKolomMeth){
  #' Blanquet routine  voor ec25 berekening
  b=z[z$meth=='blanquet',]
  b$sqgem=sqrt(b$sgem)
  b$rlngem=log(b$sgem)
  # b$rlngem=log(b$sgem)/log(10)
  b$kblan=1.046*(107.73*b$cl+77.55*b$hco3+109.02*b$so4+20.97*b$k-b$sqgem*(1.452*b$cl+1.228*b$hco3+2.844*b$so4+0.112*b$k)+((6.1-0.9*b$sqgem)*b$cl+(6-2.067*b$sqgem)*b$hco3+(-3.1-7.274*b$rlngem)*b$so4)*b$ca/b$sgem+((-0.23-1.746*b$rlngem)*b$cl+(6.43-4.047*b$rlngem)*b$hco3+(-7.8-4.831*b$rlngem)*b$so4)*b$mg/b$sgem)
  #$kblan=1.046*(107.73*b$cl+77.55*b$hco3+109.02*b$so4+20.97*b$k-b$sqgem*(1.452*b$cl+1.228*b$hco3+2.844*b$so4+0.112*b$k)+((6.1-0.9*b$sqgem)*b$cl+(6-2.067*b$sqgem)*b$hco3+(-3.1-7.274*b$rlngem)*sb$o4)*b$ca/b$sgem+((-0.23-1.746*b$rlngem)*b$cl+(6.43-4.047*b$rlngem)*b$hco3+(-7.8-4.831*b$rlngem)*b$so4)*b$mg/b$sgem)
  b$rk20=b$kblan
  b[b$rhco3>=0.15&!is.na(b$rhco3),'rk20']=0.911*b[b$rhco3>=0.15&!is.na(b$rhco3),'kblan']+196
  b[b$rhco3>=0.5&!is.na(b$rhco3),'rk20']=0.98*b[b$rhco3>=0.5&!is.na(b$rhco3),'kblan']+33
  b[b$rso4>=0.33&!is.na(b$rso4),'rk20']=0.8*b[b$rso4>=0.33&!is.na(b$rso4),'kblan']+309
  b[b$rso4>=0.33&b$sgem>=100&!is.na(b$rso4)&!is.na(b$sgem),'rk20']=1.02*b[b$rso4>=0.33&b$sgem>=100&!is.na(b$rso4)&!is.na(b$sgem),'kblan']-528
  z[z$meth=='blanquet','rk20']=b$rk20
  rk20uitBlanquetIndataframeuitMaakKolomMeth=z
  return(rk20uitBlanquetIndataframeuitMaakKolomMeth)
}

Dunlap<-function(z=dataframeuitMaakKolomMeth){
  #' Dunlap routine  voor ec25 berekening
  b=z[z$meth=='dunlap',]
  b$A=35.35*b$cl+16.48*b$hco3+24.02*b$so4+75.63*b$co3+(b$na+b$k)*22.99+19.04*b$ca+24.3*b$mg
  b$B=4.3*10^-4*(log(b$A))^7.888
  b$F=0.948+1.503*10^-6*b$B
  b[b$B<10-4,'F']=1.101-3.252*10^-5*b[b$B<10-4,'B']
  b$kdun=b$F*b$B
  b$KDUN=7.456*b$kdun^0.8198
  z[z$meth=='dunlap','rk20']=b$KDUN
  rk20uitDunlapIndataframeuitMaakKolomMeth=z
  return(rk20uitDunlapIndataframeuitMaakKolomMeth)  
}

Logan<-function(z=dataframeuitMaakKolomMeth){
  #' logan routine  voor ec25 berekening
  b=z[z$meth=='logan',]
  b$klogan=(222.28*b$sgem)^0.9058
  b$rk20=b$klogan-30
  b[b$sgem>100&!is.na(b$sgem),"rk20"]=1.002*b[b$sgem>100&!is.na(b$sgem),"klogan"]-83
  z[z$meth=='logan','rk20']=b$rk20
  rk20uitLoganIndataframeuitMaakKolomMeth=z
  return(rk20uitLoganIndataframeuitMaakKolomMeth)  
}

Rossum<-function(z=dataframeuitMaakKolomMeth){
  #' Rossum volgens Ec_voor_Patrick.docx  voor ec25 berekening
  b=z[z$meth=="rossum",]
  # van H+ naar milliequivalent
  # b$h3o=b$h
  b$al=0
  b$g0an =86*b$co3+44.5*b$hco3+79.8*b$so4+76.3*b$cl+71.4*b$no3
  b$g0kat=59.5*b$ca+53.1*b$mg+50.1*b$na+73.5*b$k+349*b$h3o+73.5*b$nh4+54*b$fe+78*b$al
  b$zan =(4*(b$co3+b$so4)+b$cl+b$hco3+b$no3)  /(2*(b$co3+b$so4)+b$cl+b$hco3+b$no3)
  # Aanname 1.55*al
  b$zkat=(4*(b$ca+b$mg+b$fe+1.55*b$al)+b$na+b$k+b$h3o+b$nh4)/(2*(b$ca+b$mg+b$fe+1.55*b$al)+b$na+b$k+b$h3o+b$nh4)
  b$gaman =b$g0an /b$san
  b$gamkat=b$g0kat/b$skat
  b$q=b$zan*b$zkat*(b$gaman+b$gamkat)/((b$zan+b$zkat)*(b$zkat*b$gaman+b$zan*b$gamkat))
  b$kross=0.885*(b$g0kat+b$g0an-((b$gaman+b$gamkat)*b$zan*b$zkat*2*b$q/(115.2*(b$zan+b$zkat)*(1+sqrt(b$q)))+0.668)*((b$zan+b$zkat)*b$sgem)^1.5)
  b$KROSS=b$kross
  b[b$rcl>=0.67&!is.na(b$rcl),'KROSS']=1.0003*b[b$rcl>=0.67&!is.na(b$rcl),'kross']-2
  b[b$rso4>=0.33&!is.na(b$rso4),'KROSS']=0.989*b[b$rso4>=0.33&!is.na(b$rso4),'kross']
  b[b$rhco3>=0.67&!is.na(b$rhco3),'KROSS']=1.025*b[b$rhco3>=0.67&!is.na(b$rhco3),'kross']-8
  
  z[z$meth=='rossum','rk20']=b$KROSS
  rk20uitRossumIndataframeuitMaakKolomMeth=z
  return(rk20uitRossumIndataframeuitMaakKolomMeth)
}


McNeal<-function(z=dataframeuitMaakKolomMeth){
  #' McNeal routine voor ec25 berekening
  #McNeal volgens Ec_voor_Patrick.docx
  b=z[z$meth=="mcneal",]
  b$caT=b$ca/2000
  b$mgT=b$mg/2000
  b$so4T=b$so4/2000
  b$alfa=(b$gam2^2*204.174)^-1
  b$beta=(b$gam2^2*229.087)^-1
  b$caso4=500*(b$caT+b$so4T+b$alfa)-500*sqrt((b$caT+b$so4T+b$alfa)^2-4*b$caT*b$so4T)
  # caso4=500*(caT+so4T+alfa)-500*sqrt((caT+so4T+alfa)^2-4*caT*so4T)
  b$so4L=b$so4T-b$caso4/1000
  b$mgso4=500*(b$mgT+b$so4L+b$beta)-500*sqrt((b$mgT+b$so4L+b$beta)^2-4*b$mgT*b$so4L)
  # mgso4=500*(mgT+so4L+beta)-500*sqrt((mgT+so4L+beta)^2-4*mgT*so4L)
  b$caf=b$ca-2*b$caso4
  b$mgf=b$mg-2*b$mgso4
  b$so4f=b$so4-2*b$caso4-2*b$mgso4
  #methode 1 algemeen
  # 
  b$kmcneal=885*(0.0660*(b$cl+b$k)+0.0414*b$caf+0.0356*b$mgf+0.0452*b$na+0.0507*b$so4f+0.0470*b$co3+0.0348*b$hco3+0.0603*b$no3+0.0629*(b$caso4+b$mgso4)+(1/b$san)*(0.03*b$cl+0.029*b$hco3+0.077*b$so4f+0.034*b$no3+0.07*b$co3)+(1/b$skat)*(0.055*b$caf+0.06*b$mgf+0.023*b$na+0.03*b$k+0.183*(b$caso4+b$mgso4)))
  # kmcneal=885*(0.0660*(cl+k)+0.0414*caf+0.0356*mgf+0.0452*na+0.0507*so4f+0.0470*co3+0.0348*hco3+0.0603*no3+0.0629*(caso4+mgso4)+(1/san)*(0.03*cl+0.029*hco3+0.077*so4f+0.034*no3+0.07*co3)+(1/skat)*(0.055*caf+0.06*mgf+0.023*na+0.03*k+0.183*(caso4+mgso4)))
  b$KMCNEAL=0.964*b$kmcneal+8
  #  methode 1 speciaal geval 
  b[b$rso4>=0.33&!is.na(b$rso4),'KMCNEAL']=1.181*b[b$rso4>=0.33&!is.na(b$rso4),'kmcneal']-275
  b[b$rso4>=0.33&b$kmcneal<1100&!is.na(b$kmcneal)&!is.na(b$rso4),'KMCNEAL']=1.052*b[b$rso4>=0.33&b$kmcneal<1100&!is.na(b$kmcneal)&!is.na(b$rso4),'kmcneal']-45
  
  # b[b$rso4>=0.33&b$kmcneal<1100,'KMCNEAL']=1.052*b[b$rso4>=0.33&b$kmcneal<1100,'kmcneal']-45
  # methode 2 overschrijft methode 1
  b$kmcneal=885*(0.0620*(b$cl+b$k)+0.0355*b$caf+0.0269*b$mgf+0.0402*b$na+0.0407*b$so4f+0.0382*b$co3+0.0291*b$hco3+0.0528*b$no3+0.0492*(b$caso4+b$mgso4)+(1/b$san)*(0.23*b$cl+0.320*b$hco3+0.590*b$so4f+0.400*b$no3+0.51*b$co3)+(1/b$skat)*(0.260*b$caf+0.44*b$mgf+0.270*b$na+0.23*b$k+0.870*(b$caso4+b$mgso4)))
  # kmcneal=885*(0.0620*(cl+k)+0.0355*caf+0.0269*mgf+0.0402*na+0.0407*so4f+0.0382*co3+0.0291*hco3+0.0528*no3+0.0492*(caso4+mgso4)+(1/san)*(0.23*cl+0.320*hco3+0.590*so4f+0.400*no3+0.51*co3)+(1/skat)*(0.260*caf+0.44*mgf+0.270*na+0.23*k+0.870*(caso4+mgso4)))
  
  b[b$sgem>50&!is.na(b$sgem),'KMCNEAL']=0.953*b[b$sgem>50&!is.na(b$sgem),'kmcneal']+58
  z[z$meth=='mcneal','rk20']=b$KMCNEAL
  rk20uitMcNealIndataframeuitMaakKolomMeth=z
  return(rk20uitMcNealIndataframeuitMaakKolomMeth)
}
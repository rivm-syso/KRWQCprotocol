
#' QC1b. Controle apparaat, techniek en procedure BRO
#'
#' Controle op bemonsteringsapparaat, bemonsteringsprocedure,
#' waardebepalingstechniek en waardebepalingsprocedure
#'    
#' Controleer of de bemonstering en labbepaling conform de BRO is gedaan.
#'
#' Indien het bemonsteringsapparaat of de -procedure afwijkt van de BRO, 
#' ken het concept QC oordeel verdacht toe aan het monster.
#' Indien de waardebepalingstechniek of -procedure afwijkt van de BRO,
#' ken het concept QC oordeel verdacht toe aan de betreffende parameters. 
#' 
#' @param d_veld dataframe met veldwaarnemingen
#' @param d_parameter dataframe met parameter informatie      
#' @param d_metingen dataframe met metingen
#' @param verbose of tekstuele output uit script gewenst is (T) of niet (F). Staat
#' standaard op F.
#'
#' @return het metingen bestand met attribute van test resultaten. In de kolom
#' `oordeel` blijkt of de locatie/monster 'onverdacht' of 'verdacht' is.
#'
#' @export
#'


QC1b <- function(d_veld, d_parameter, d_metingen, verbose = F) {

    # laad opzoektabellen uit de BRO
    data(BRO_bemonsteringsapparaat)
    data(BRO_bemonsteringsprocedure)
    data(BRO_waardebepalingstechniek)
    data(BRO_waardebepalingsprocedure)

    # Check datasets op kolommen en unieke informatie
    testKolommenParameter(d_parameter)
    valideParamInfo(d_parameter)
    testKolommenMetingen(d_metingen)

    # eerst controle op bemonsteringsapparaat en -procedure
    bemonstering <- full_join(d_veld %>% 
                                dplyr::filter(!bem_app %in% BRO_bemonsteringsapparaat$waarde) %>%
                                dplyr::mutate(reden_apparatuur = "bemonsteringsapparatuur wijkt af van BRO"),
                              d_veld %>%
                                dplyr::filter(!bem_proc %in% BRO_bemonsteringsprocedure$waarde) %>%
                                dplyr::mutate(reden_procedure = "bemonsteringsprocedure wijkt af van BRO")) %>%
   
    dplyr::select(qcid, putcode, filter, jaar, maand, dag, bem_app, bem_proc, reden_apparatuur, reden_procedure)

# daarna controle op waardebepalingstechniek en -procedure
waardebepaling <- full_join(d_parameter %>%
                              dplyr::filter(!waarde_techniek %in% BRO_waardebepalingstechniek$waarde) %>%
                              dplyr::mutate(reden_techniek = "waardebepalingstechniek wijkt af van BRO"), 
                            d_parameter %>%
                              dplyr::filter(!waarde_procedure %in% BRO_waardebepalingsprocedure$waarde) %>%
                              dplyr::mutate(reden_procedure = "waardebepalingsprocedure wijkt af van BRO")) %>%
  dplyr::select(qcid, parameter, cas, waarde_techniek, waarde_procedure, reden_techniek, reden_procedure)

  rapportageTekst <- paste("Er zijn in totaal", 
                           nrow(bemonstering %>% dplyr::filter(reden_apparatuur == "bemonsteringsapparatuur wijkt af van BRO")), 
                           "bemonsteringen met afwijkende bemonsteringsapparatuur en",
                           nrow(bemonstering %>% dplyr::filter(reden_procedure == "bemonsteringsprocedure wijkt af van BRO")),
                           "bemonstering met afwijkende bemonsteringsprocedure.",
                           "Daarnaast zijn er",
                           nrow(waardebepaling %>% dplyr::filter(reden_techniek == "waardebepalingstechniek wijkt af van BRO")),
                           "parameters met een afwijkende waardebepalingstechniek en",
                           nrow(waardebepaling %>% dplyr::filter(reden_procedure == "waardebepalingsprocedure wijkt af van BRO")),
                           "met een afwijkende waardebepalingsprocedure t.o.v. de BRO.")

if(verbose) {
    if(nrow(bemonstering) > 0 | nrow(waardebepaling) > 0) {
        print(rapportageTekst)

    } else {
        print(paste("Alle bemonsteringapparaten en -procedures",
                    "en waardebepalingstechnieken en -procedures zijn conform de BRO"))
    }
}

# voeg concept oordeel van afwijkende bemonstering toe aan monsters op die locaties in betreffende meetronde
bemonstering <- bemonstering %>%
    dplyr::mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-"))

resultaat_df_meting <- d_metingen %>%
    dplyr::group_by(monsterid) %>%
    dplyr::mutate(iden = paste(putcode, filter, jaar, maand, dag, sep = "-")) %>%
    dplyr::mutate(oordeel = ifelse(iden %in% bemonstering$iden,
                            "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::left_join(., bemonstering %>% dplyr::select(iden, reden_apparatuur, reden_procedure), by = "iden") %>%
    dplyr::select(-iden)

# voeg concept oordeel van afwijkende bemonstering toe aan parameters op die locaties in betreffende meetronde
resultaat_df_waarde <- d_metingen %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(oordeel = ifelse(parameter %in% waardebepaling$parameter,
                            "verdacht", "onverdacht")) %>%
    dplyr::filter(oordeel == "verdacht") %>%
    dplyr::left_join(., waardebepaling %>% dplyr::select(parameter, reden_techniek, reden_procedure), by = "parameter")

# voeg samen om als attribute weg te schrijven
resultaat_df <- rbind(resultaat_df_meting, resultaat_df_waarde)

# voeg attribute met uitkomsten tests toe aan relevante dataset (d_metingen)
verdacht_id <- resultaat_df %>% 
    dplyr::filter(oordeel == "verdacht") %>% 
    dplyr::distinct(qcid) %>%
    dplyr::pull(qcid)

test <- "QC1b"

d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                test = test,
                                oordeel = "verdacht",
                                ids = verdacht_id)
d_metingen <- qcout_add_rapportage(obj = d_metingen,
                                   test = test,
                                   tekst = rapportageTekst)
d_metingen <- qcout_add_resultaat(obj = d_metingen,
                                  test = test,
                                  resultaat = resultaat_df)

return(d_metingen)
}


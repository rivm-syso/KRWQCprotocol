#' QC_niet_uitvoerbaar
#'
#' Ken het oordeel niet uitvoerbaar toe aan alle locaties/monsters van een 
#' speciefieke test uit het metingen bestand
#' 
#' @param d_metingen data.frame waarbij een QC test niet uivoerbaar is
#' @param qctest character string om aan te geven welke QC test niet uitvoerbaar is
#' 
#' @return metingen bestand met niet uitvoerbare locaties/monsters.
#'
#' @details
#' 
#' Indien een QC test niet uitvoerbaar is omdat het metingen bestand niet 
#' volledig is wordt deze functie gebruikt om het oordeel "niet uitvoerbaar" 
#' toe te voegen aan het metingen bestand.
#' 
#' @export
#'


QC_niet_uitvoerbaar <- function(d_metingen, qctest){
  
  d_metingen <- qcout_add_oordeel(obj = d_metingen,
                                  test = qctest,
                                  oordeel = "niet uitvoerbaar",
                                  ids = d_metingen$qcid)
  
  return(d_metingen)
}

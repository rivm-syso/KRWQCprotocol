#' Collect_result , verzamel testresultaten
#'
#' Verzamel de testresultaten  uit een data.frame met tests. De
#' testresultaten worden per regel in de data.frame aangegeven.
#'
#' @param x data.frame dat getest is door 1 of meerdere QC tests
#'
#' @return Een data.frame met voor iedere test in x en voor iedere
#' record / regel met gegeven het test-oordeel. Het test-oordeel wordt
#' gegeven als een factor
#'
#' @details
#' 
#' Deze functie verzameld de testresultaten die door de afzonderlijk
#' tests zijn toegevoegd aan de data.frame met metingen. Voor iedere
#' test wordt het testresultaat opgehaald en en per meting
#' weergegeven. Het resultaat is een data.frame met per kolom de
#' afzonderlijke test en per meting (regel/record) het resultaat. 
#' 
#' De waarden in de data.frame zijn opgeslagen als factors. Hierdoor
#' is het eenvoudig om een snel een overzicht te krijgen door
#' bijvoorbeeld het gebruik van sumarise of plot functies.
#' 
#' @examples
#'  data(veld)
#'  data(put)
#'  data(metingen)
#'  data(filter)
#'  x <- QC0a(d_veld = veld, d_put = put, d_metingen = metingen, verbose = TRUE)
#'  d <- veld %>% dplyr::filter(landgebruik != "heide")
#'  x <- QC0b(d_veld = d, d_put = put, d_metingen = x, verbose = FALSE)
#'  x <- QC0f(d_veld = veld, d_filter = filter, d_metingen = x)
#'  x <- QC0g(d_filter = filter, d_metingen = x)
#'  x <- QC1a(d_parameter = parameter, d_metingen = x)
#'  x <- QC1b(d_veld = veld, d_parameter = parameter, d_metingen = x)
#'  x <- QC1e(d_metingen = x)
#'  oordeel <- collect_result(x)
#'  summary(oordeel)
#'
#'
#' 
#' @export
#'




collect_result <- function(x) {

    res <- collect_result_raw(x) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("QC", ignore.case = FALSE), set_onverdacht))
    return(res)


}



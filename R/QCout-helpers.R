# Helper functions for QCout output

qcout_check_waarde <- function(test,obj,waarde) {

    if(!waarde%in%c("oordeel","rapportage","resultaat")) {
        stop("qcout_check_waarde: onbekende waarde toegevoegd")
    }

    # check if qcout attr exists, if not create
    if(qcout_attrexists(obj)) {
        qcout <- attributes(obj)[["qcout"]]
    } else {
        qcout <- list()
    }

    # check if qcout allready containts test
    if(is.null(qcout[[test]])) {
           qcout[[test]] <- list()
    }

    # check if qcout contains waarde
    if(is.null(qcout[[test]][[waarde]])) {
        qcout[[test]][[waarde]] <- list()
    }



    return(qcout)


}

qcout_attrexists <- function(obj) {
    res <- any(names(attributes(obj))=="qcout")
    return(res)

}

qcout_add_oordeel <- function(test,obj,oordeel,ids) {

    if(length(oordeel)!=1) {
        stop("qcout_add_oordeel: length(oordeel)!=1")
    }

    if(!oordeel%in%c("onverdacht","twijfelachtig","verdacht",
                     "ontbrekend","niet uitvoerbaar")) {
        stop("qcout_add_oordeel: onbekend oordeel toegevoegd")
    }
       

    qcout <- qcout_check_waarde(test,obj,"oordeel")

    qcout[[test]][["oordeel"]][[oordeel]] <- ids

        

    attr(obj,"qcout") <- qcout
    return(obj)

}


qcout_add_rapportage <- function(test,obj,tekst) {
    qcout <- qcout_check_waarde(test,obj,"rapportage")
    qcout[[test]][["rapportage"]] <- tekst
    attr(obj,"qcout") <- qcout
    return(obj)


}

qcout_add_resultaat <- function(test,obj,resultaat) {
    qcout <- qcout_check_waarde(test,obj,"resultaat")
    qcout[[test]][["resultaat"]] <- resultaat
    attr(obj,"qcout") <- qcout
    return(obj)

}



#### Functions for collecting result


collect_result_raw <- function(d_metingen) {

    qcout <- attr(d_metingen, "qcout")
    v <- d_metingen %>%
        dplyr::select(qcid)
    for (i in names(qcout)) {
        vt <- names(qcout[[i]][["oordeel"]])

        for (j in vt) {
            ids <- qcout[[i]][["oordeel"]][[j]]
            if (length(ids > 0)) {
                v_sub <- data.frame(qcid = ids, v2 = j)
                names(v_sub)  <- c("qcid", i)
                v <- v %>% dplyr::left_join(v_sub, by = "qcid")
            }

        }
    }
    return(v)
}

set_onverdacht <- function(x) {
    y <- ifelse(is.na(x) | x == "", "onverdacht", x)
    return(as.factor(y))
}


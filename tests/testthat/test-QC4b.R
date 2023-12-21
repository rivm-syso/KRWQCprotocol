test_that("QC4b T1", {


              data(metingen)
              data(parameter)


              d <- metingen
              d$parameter <- d$parameter %>%
                  dplyr::recode( "h_1__veld" = "pH_veld",
                                .default = d$parameter) 
              d  <- d %>%
                  dplyr::filter(parameter == "pH" | parameter == "pH_veld")

              x <- QC4b(d_metingen = d, ph_veld_naam = "pH_veld")

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC4b"]]))
              expect_true(is.list(x_attr[["QC4b"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids <- x_attr[["QC4b"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) ==  0)
              expect_false(any(v1 != ids))

              expect_true(nrow(d) == nrow(x))


})

test_that("QC4b T2, ph>15", {



              data(metingen)
              data(parameter)


              d <- metingen
              d$parameter <- d$parameter %>%
                  dplyr::recode( "h_1__veld" = "pH_veld",
                                .default = d$parameter) 
              d  <- d %>%
                  dplyr::filter(parameter == "pH" | parameter == "pH_veld") 
              d$waarde[1] <- 15

              x <- QC4b(d_metingen = d, ph_veld_naam = "pH_veld")

              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC4b"]]))
              expect_true(is.list(x_attr[["QC4b"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids <- x_attr[["QC4b"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(d) == nrow(x))
              expect_true(d$qcid[1]%in%ids)


})

test_that("QC4b T3, ph_veld default value", {



              data(metingen)
              data(parameter)


              d <- metingen
              d$parameter <- d$parameter %>%
                  dplyr::recode( "h_1__veld" = "pH_veld",
                                .default = d$parameter) 
              d  <- d %>%
                  dplyr::filter(parameter == "pH" | parameter == "pH_veld") 

              x <- QC4b(d_metingen = d)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC4b"]]))
              expect_true(is.list(x_attr[["QC4b"]][["resultaat"]]))


})

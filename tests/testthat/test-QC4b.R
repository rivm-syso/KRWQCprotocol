test_that("QC4b T1", {


              data(metingen)
              data(parameter)


              d <- metingen
              d$parameter <- d$parameter %>%
                  dplyr::recode("h" = "ph",
                                "h_1__veld" = "ph_veld",
                                "h_5__veld" = "ph_5__veld",
                                .default = d$parameter) 
              d  <- d %>%
                  dplyr::filter(parameter == "ph" | parameter == "ph_veld")

              x <- QC4b(d_metingen = d)

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

test_that("QC4b T2", {


              data(metingen)
              data(parameter)


              d <- metingen
              d$parameter <- d$parameter %>%
                  dplyr::recode("h" = "ph",
                                "h_1__veld" = "ph_veld",
                                "h_5__veld" = "ph_5__veld",
                                .default = d$parameter) 
              d  <- d %>%
                  dplyr::filter(parameter == "ph" | parameter == "ph_veld") 
              d$waarde[1] <- 15

              x <- QC4b(d_metingen = d)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
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

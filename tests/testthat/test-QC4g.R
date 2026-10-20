
test_that("QC4g",{

              data(metingen)

              x <- QC4g(d_metingen = metingen,verbose = FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x,"qcout")
              expect_false(is.null(x_attr[["QC4g"]]))
              expect_true(is.list(x_attr[["QC4g"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids <- x_attr[["QC4g"]][["oordeel"]][["twijfelachtig"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids,qcids)
              expect_true(length(v1)>0)
              expect_false(any(v1!=ids))
              expect_true(nrow(metingen) == nrow(x))

              # oordeel toekennen aan parameters, dus maar twee
              # parameters (no3,nh4) met oordeel in data
              v1 <- x_attr[["QC4g"]][["resultaat"]]
              v2 <- metingen %>%
                  dplyr::filter(qcid%in%ids) %>%
                  dplyr::distinct(parameter) %>%
                  dplyr::pull(parameter)
              expect_equal(length(v2), 2)
})

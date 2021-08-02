

test_that("QC1a", {


              data(metingen)
              data(parameter)

              x <- QC1a(d_parameter = parameter, d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1a"]]))
              expect_true(is.list(x_attr[["QC1a"]][["resultaat"]]))

              # refiddle some parameters, change aquocode

              d <- parameter
              d$aquocode[24] <- 99999

              x <- QC1a(d_parameter = d, d_metingen = metingen, verbose = FALSE)
              x_attr <- attr(x, "qcout")
              # test if ids are from metingen data.frame
              ids <- x_attr[["QC1a"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(metingen) == nrow(x))


})

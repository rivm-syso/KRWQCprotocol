test_that("QC1d", {

              data(parameter)
              data(veld)
              data(metingen)

              x <- QC1d(d_vel = veld, d_parameter = parameter, d_metingen = metingen, verbose = FALSE)
              x_attr <- attr(x, "qcout")

              ids <- x_attr[["QC1d"]][["oordeel"]][["verdacht"]]
              x_attr[["QC1d"]][["rapportage"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(metingen) == nrow(x))


})

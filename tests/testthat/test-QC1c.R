test_that("QC1c", {


              data(metingen)

              x <- QC1c(d_metingen = metingen, verbose = FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1c"]]))
              expect_true(is.list(x_attr[["QC1c"]][["resultaat"]]))

              ids <- x_attr[["QC1c"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(metingen) == nrow(x))


})

test_that("QC3b", {


              data(filter)
              data(metingen)

              suppressWarnings(x <- QC3b(d_filter = filter, d_metingen = metingen))

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3b"]]))
              expect_true(is.list(x_attr[["QC3b"]][["resultaat"]]))


              ids <- x_attr[["QC3b"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(metingen) == nrow(x))



})

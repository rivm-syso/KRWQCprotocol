test_that("QC2a", {



              data(metingen)
              data(veld)

              x <- QC2a(d_veld = veld, d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC2a"]]))
              expect_true(is.list(x_attr[["QC2a"]][["resultaat"]]))

              qcids <- metingen$qcid
              ids1 <- x_attr[["QC2a"]][["oordeel"]][["verdacht"]]
              ids2 <- x_attr[["QC2a"]][["oordeel"]][["twijfelachtig"]]

              v1 <- intersect(ids1, qcids)
              expect_true(length(v1) > 0)
              v2 <- intersect(ids2, qcids)
              expect_true(length(v2) > 0)

              expect_true(length(intersect(v1, v2)) == 0)

              expect_true(nrow(metingen) == nrow(x))



})

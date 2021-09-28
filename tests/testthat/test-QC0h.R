
test_that("QC0h",{


              data(filter)
              data(metingen)

              x <- QC0h(d_metingen = metingen, d_filter = filter, verbose = TRUE )

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC0h"]]))
              expect_true(is.list(x_attr[["QC0h"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids1 <- x_attr[["QC0h"]][["oordeel"]][["twijfelachtig"]]
              ids2 <- x_attr[["QC0h"]][["oordeel"]][["verdacht"]]
              print(ids1)
              print(ids2)

              qcids <- metingen$qcid
              v1 <- intersect(ids1, qcids)
              v2 <- intersect(ids2, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids1))
              expect_true(length(v2) > 0)
              expect_false(any(v2 != ids2))

              expect_true(nrow(metingen) == nrow(x))


})

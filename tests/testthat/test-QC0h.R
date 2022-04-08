
test_that("QC0h",{


              data(filter)
              data(metingen)

              sink(tempfile())
              x <- QC0h(d_metingen = metingen, d_filter = filter, verbose = TRUE )
              sink()
              x <- QC0h(d_metingen = metingen, d_filter = filter, verbose = FALSE )

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC0h"]]))
              expect_true(is.list(x_attr[["QC0h"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids1 <- x_attr[["QC0h"]][["oordeel"]][["twijfelachtig"]]
              ids2 <- x_attr[["QC0h"]][["oordeel"]][["verdacht"]]

              qcids <- metingen$qcid
              v1 <- intersect(ids1, qcids)
              v2 <- intersect(ids2, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids1))
              expect_true(length(v2) > 0)
              expect_false(any(v2 != ids2))

              expect_true(nrow(metingen) == nrow(x))


})

test_that("QC0h - niet uitvoerbaar",{

              d <- metingen %>%
                  mutate(waarde = if_else(parameter == "NO3", NA_real_, waarde ))

              x <- QC0h(d_metingen = metingen, d_filter = filter)
              x_attr <- attr(x, "qcout")
              ids1 <- x_attr[["QC0h"]][["oordeel"]][["twijfelachtig"]]
              ids2 <- x_attr[["QC0h"]][["oordeel"]][["verdacht"]]
              ids3 <- x_attr[["QC0h"]][["oordeel"]][["niet uitvoerbaar"]]
              goede_ids <- setdiff(metingen$qcid, c(ids1, ids2, ids3))

              d <- metingen %>%
                  filter(qcid %in% goede_ids)

              x <- QC0h(d_metingen = d, d_filter = filter)
              x_attr <- attr(x, "qcout")
              ids1 <- x_attr[["QC0h"]][["oordeel"]][["twijfelachtig"]]
              ids2 <- x_attr[["QC0h"]][["oordeel"]][["verdacht"]]
              ids3 <- x_attr[["QC0h"]][["oordeel"]][["niet uitvoerbaar"]]
              alle_ids <- c(ids1, ids2, ids3)
              expect_true(length(alle_ids) == 0)

              d2 <- d %>%
                  mutate(waarde = if_else(parameter == "NO3", NA_real_, waarde ))

              x <- QC0h(d_metingen = d2, d_filter = filter)
              x_attr <- attr(x, "qcout")
              ids3 <- x_attr[["QC0h"]][["oordeel"]][["niet uitvoerbaar"]]
              expect_true(length(ids3) == nrow(d2))

})

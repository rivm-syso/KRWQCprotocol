test_that("QC1e T1", {


              data(metingen)

              x <- QC1e(d_metingen = metingen, verbose = FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1e"]]))
              expect_true(is.list(x_attr[["QC1e"]][["resultaat"]]))


              # test if ids are from metingen data.frame
              ids <- x_attr[["QC1e"]][["oordeel"]][["ontbrekend"]]

              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(qcids != ids))

              expect_true(nrow(metingen) == nrow(x))
})

test_that("QC1e T2", {

              data(metingen)

              metingen <- metingen %>% 
                  dplyr::filter(parameter %in% c("fe", "cl", "cu")) %>%
                  na.omit()

              x <- QC1e(d_metingen = metingen, verbose = FALSE)
              x_attr <- attr(x, "qcout")
              ids <- x_attr[["QC1e"]][["oordeel"]][["ontbrekend"]]
              expect_true(length(ids) == 0)


})

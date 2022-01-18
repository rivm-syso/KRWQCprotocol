test_that("QC3e T1", {
              # Test default values

              data(metingen)
              data(parameter)
              #example data contains multiple EC measurements

              x <- QC3e( d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3e"]]))
              expect_true(is.list(x_attr[["QC3e"]][["resultaat"]]))

              expect_equal(nrow(metingen),nrow(x))


})



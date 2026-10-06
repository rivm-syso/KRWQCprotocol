test_that("QC1a", {


              data(metingen)
              data(veld)

              x <- QC1a(d_veld = veld, d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1a"]]))
              expect_true(is.list(x_attr[["QC1a"]][["resultaat"]]))

              expect_true(nrow(metingen) == nrow(x))

})

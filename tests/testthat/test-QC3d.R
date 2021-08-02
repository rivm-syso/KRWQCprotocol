test_that("QC3d", {

              data(metingen)
              data(parameter)
              #example data contains multiple EC measurements
              metingen <- metingen %>%
                  dplyr::filter(parameter != "ec_1__veld")


              x <- QC3d( d_metingen = metingen,d_parameter=parameter)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3d"]]))
              expect_true(is.list(x_attr[["QC3d"]][["resultaat"]]))

              expect_equal(nrow(metingen),nrow(x))


})


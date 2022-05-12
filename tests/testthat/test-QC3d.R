test_that("QC3d T1", {
              # test with 'default' LMG parameter

              data(metingen)
              data(parameter)
              #example data contains multiple EC measurements
              metingen <- metingen %>%
                  dplyr::filter(parameter != "ec_1__veld")


              x <- QC3d( d_metingen = metingen,d_parameter=parameter, 
                        geleidendheid_veld_naam = "ec_5__veld")

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3d"]]))
              expect_true(is.list(x_attr[["QC3d"]][["resultaat"]]))

              expect_equal(nrow(metingen),nrow(x))


})


test_that("QC3d T2", {
              # test with default value voor geleidendheid_veld_naam.

              data(metingen)
              data(parameter)

              d <- metingen  %>%
                  mutate(parameter = if_else(parameter == "ec_5__veld",
                                                     "GELDHD_VELD", parameter))

              x <- QC3d( d_metingen = d,d_parameter=parameter)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3d"]]))
              expect_true(is.list(x_attr[["QC3d"]][["resultaat"]]))

              expect_equal(nrow(metingen),nrow(x))

})


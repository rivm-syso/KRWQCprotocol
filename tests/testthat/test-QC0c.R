
test_that("QC0c_create_file", {

    
              fname <- "QC0c_beschadiging_put.csv"
              expect_error(QC0c_create_file(dir="nonexistingdirectory"))

              expect_false(file.exists(file.path(tempdir(),fname)))
              QC0c_create_file(dir=tempdir())
              expect_true(file.exists(file.path(tempdir(),fname)))
              unlink(file.path(tempdir(),fname))
})


test_that("QC0c",{

              data(metingen)
              data(put)
              fname <- "QC0c_beschadiging_put.csv"
              QC0c_create_file(dir=tempdir())

              data(metingen)

              x <- QC0c(dir = tempdir(), d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC0c"]]))
              expect_true(is.list(x_attr[["QC0c"]][["resultaat"]]))
              expect_true(length(x_attr[["QC0c"]][["oordeel"]][["twijfelachtig"]]) == 0)
              expect_true(nrow(metingen) == nrow(x))


              fouteput <- metingen %>% 
                  dplyr::select(monsterid, putcode, filter, jaar, maand, dag ) %>%
                  dplyr::distinct()
              fouteput <- fouteput[sample(1:nrow(fouteput),size=10),] %>%
                  dplyr::mutate(beschadiging_put="stuk")

              write.csv(fouteput, file.path(tempdir(),fname),row.names = FALSE)
              x <- QC0c(dir = tempdir(), d_metingen = metingen, verbose = FALSE)
              x_attr <- attr(x, "qcout")
              expect_true(length(x_attr[["QC0c"]][["oordeel"]][["twijfelachtig"]])  > 1)
              unlink(file.path(tempdir(),fname))
})

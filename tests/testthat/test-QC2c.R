
test_that("QC2c_create_file", {
  
  
  fname <- "QC2c_veldwaarnemingen.csv"
  expect_error(QC2c_create_file(dir="nonexistingdirectory"))
  
  expect_false(file.exists(file.path(tempdir(),fname)))
  QC2c_create_file(dir=tempdir())
  expect_true(file.exists(file.path(tempdir(),fname)))
  unlink(file.path(tempdir(),fname))
  
  
  
  
  
  
})



test_that("QC2c",{
  
  fname <- "QC2c_veldwaarnemingen.csv"
  QC2c_create_file(dir=tempdir())
  
  data(metingen)
  
  x <- QC2c(dir = tempdir(), d_metingen = metingen)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC2c"]]))
  expect_true(is.list(x_attr[["QC2c"]][["resultaat"]]))
  expect_true(length(x_attr[["QC2c"]][["oordeel"]][["twijfelachtig"]]) == 0)
  expect_true(nrow(metingen) == nrow(x))
  
  
  bijzondereput <- metingen %>% 
    dplyr::select(monsterid, putcode, filter, jaar, maand, dag ) %>%
    dplyr::distinct()
  bijzondereput <- bijzondereput[sample(1:nrow(bijzondereput),size=10),] %>%
    dplyr::mutate(bijzonderheden="bijzonder")
  
  write.csv(bijzondereput, file.path(tempdir(),fname),row.names = FALSE)
  x <- QC2c(dir = tempdir(), d_metingen = metingen, verbose = FALSE)
  x_attr <- attr(x, "qcout")
  expect_true(length(x_attr[["QC2c"]][["oordeel"]][["twijfelachtig"]])  > 1)
  unlink(file.path(tempdir(),fname))
})
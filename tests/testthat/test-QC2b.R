
test_that("QC2b_create_file", {
  
  
  fname <- "QC2b_bemonsteringsprocedure.csv"
  expect_error(QC2b_create_file(dir="nonexistingdirectory"))
  
  expect_false(file.exists(file.path(tempdir(),fname)))
  QC2b_create_file(dir=tempdir())
  expect_true(file.exists(file.path(tempdir(),fname)))
  unlink(file.path(tempdir(),fname))
  
  
  
  
  
  
})



test_that("QC2b",{
  
  fname <- "QC2b_bemonsteringsprocedure.csv"
  QC2b_create_file(dir=tempdir())
  
  data(metingen)
  
  x <- QC2b(dir = tempdir(), d_metingen = metingen)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC2b"]]))
  expect_true(is.list(x_attr[["QC2b"]][["resultaat"]]))
  expect_true(length(x_attr[["QC2b"]][["oordeel"]][["twijfelachtig"]]) == 0)
  expect_true(nrow(metingen) == nrow(x))
  
  
  afwijkend_bemp <- metingen %>% 
    dplyr::select(monsterid, putcode, filter, jaar, maand, dag ) %>%
    dplyr::distinct()
  afwijkend_bemp <- afwijkend_bemp[sample(1:nrow(afwijkend_bemp),size=10),] %>%
    dplyr::mutate(afwijking_bemonsteringsprocedure="afwijkend")
  
  write.csv(afwijkend_bemp, file.path(tempdir(),fname),row.names = FALSE)
  x <- QC2b(dir = tempdir(), d_metingen = metingen, verbose = FALSE)
  x_attr <- attr(x, "qcout")
  expect_true(length(x_attr[["QC2b"]][["oordeel"]][["twijfelachtig"]])  > 1)
  unlink(file.path(tempdir(),fname))
})
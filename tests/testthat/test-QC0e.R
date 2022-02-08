-test_that("QC0e T1", {
  # test with numeric value 1 for fl
  
  data(metingen)
  data(filter)
  data(veld)
  
  d <- metingen
  
  x <- QC0e(d_veld = veld, d_filter = filter, d_metingen = d, fl = 1)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC0e"]]))
  expect_true(is.list(x_attr[["QC0e"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC0e"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) > 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
}) 

test_that("QC0e T2", {
  # test with default value for fl
  
  data(metingen)
  data(filter)
  data(veld)
  
  d <- metingen
  
  x <- QC0e(d_veld = veld, d_filter = filter, d_metingen = d)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC0e"]]))
  expect_true(is.list(x_attr[["QC0e"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC0e"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) > 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
}) 
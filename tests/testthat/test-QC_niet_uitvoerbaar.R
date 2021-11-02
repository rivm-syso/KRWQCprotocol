test_that("QC_niet_uitvoerbaar", {
  
  data(metingen)
  
  x <- QC_niet_uitvoerbaar(d_metingen = metingen, qctest = "QC0a")
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC0a"]]))
  
  expect_true(nrow(metingen) == nrow(x))
  
})

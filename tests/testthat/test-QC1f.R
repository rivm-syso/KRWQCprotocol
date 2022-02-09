test_that("QC1f T1 standaard waarden", {

  data(metingen)
  d <- metingen
  
  x <- QC1f(d_metingen = d)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC1f"]]))
  expect_true(is.list(x_attr[["QC1f"]][["resultaat"]]))


  ids <- x_attr[["QC1f"]][["oordeel"]][["verdacht"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) == 0)

  expect_true(nrow(metingen) == nrow(x))


})


test_that("QC1f T2 negatieve waarden", {

              data(metingen)
              d <- metingen
              d$waarde[1] <- -1

              x <- QC1f(d_metingen = d)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1f"]]))
              expect_true(is.list(x_attr[["QC1f"]][["resultaat"]]))
              
              ids <- x_attr[["QC1f"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) == 1)
})

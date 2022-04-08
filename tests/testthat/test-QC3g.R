test_that("QC3g T1", {
  
  
  data(metingen)
  
  x <- QC3g(d_metingen = metingen)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC3g"]]))
  expect_true(is.list(x_attr[["QC3g"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC3g"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) == 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
})


test_that("QC3g T2", {
              # test niet uitvoerbaar


      data(metingen)
      data(veld)

      d <- metingen %>%
          mutate(waarde = if_else(parameter == "HCO3", NA_real_, waarde))
      x <- QC3g(d_metingen = metingen)
      x_attr <- attr(x, "qcout")
      ids <- x_attr[["QC3g"]][["oordeel"]][["niet uitvoerbaar"]]
      expect_true(length(ids) == nrow(d))


})

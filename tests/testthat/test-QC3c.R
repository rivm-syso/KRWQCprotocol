test_that("QC3c T1", {
  
  
  data(metingen)
  
  x <- QC3c(d_metingen = metingen)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC3c"]]))
  expect_true(is.list(x_attr[["QC3c"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC3c"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) > 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
})


test_that("QC3c T3", {
      # test niet uitvoerbaar

      data(metingen)
      data(veld)

      d <- metingen %>%
          mutate(waarde = if_else(parameter == "Cl", NA_real_, waarde),
                 detectieteken = if_else(parameter == "Cl", NA_character_, detectieteken))
  x <- QC3c(d_metingen = d)
      x_attr <- attr(x, "qcout")
      ids <- x_attr[["QC3c"]][["oordeel"]][["niet uitvoerbaar"]]
      expect_true(length(ids) == nrow(d))
})



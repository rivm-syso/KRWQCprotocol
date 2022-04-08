test_that("QC3f T1", {
  # test with 'default' LMG parameter
  
  data(metingen)
  data(veld)
  
  d <- metingen
  
  x <- QC3f(d_veld = veld, d_metingen = d, ph_veld_naam = "h_1__veld")
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC3f"]]))
  expect_true(is.list(x_attr[["QC3f"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC3f"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) == 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
}) 

test_that("QC3f T2", {
  # test with default value voor ph_veld_naam.
  
  data(metingen)
  data(veld)
  
  d <- metingen
  d <- d %>%  mutate(
    parameter = case_when(parameter == "h_1__veld" ~ "pH_veld",
                          TRUE ~ parameter)
  )
  
  x <- QC3f(d_veld = veld, d_metingen = d)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC3f"]]))
  expect_true(is.list(x_attr[["QC3f"]][["resultaat"]]))
  
  
  ids <- x_attr[["QC3f"]][["oordeel"]][["twijfelachtig"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) == 0)
  expect_false(any(v1 != ids))
  
  expect_true(nrow(metingen) == nrow(x))
  
  
  
}) 

test_that("QC3f T3", {
      # test niet uitvoerbaar

      data(metingen)
      data(veld)

      d <- metingen %>%
          mutate(waarde = if_else(parameter == "h_1_veld", NA_real_, waarde))
      x <- QC3f(d_veld = veld, d_metingen = d, ph_veld_naam = "h_1__veld")
      x_attr <- attr(x, "qcout")
      ids3 <- x_attr[["QC3f"]][["oordeel"]][["niet uitvoerbaar"]]
      expect_true(length(ids3) == nrow(d))
})



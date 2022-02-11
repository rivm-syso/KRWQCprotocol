test_that("QC4a T1", {

             data(metingen)
             data(parameter)

             x <- QC4a(d_metingen = metingen, d_parameter = parameter)

             # test if attributes exist
             expect_true(qcout_attrexists(x))
             x_attr <- attr(x, "qcout")
             expect_false(is.null(x_attr[["QC4a"]]))
             expect_true(is.list(x_attr[["QC4a"]][["resultaat"]]))

             ids <- x_attr[["QC1a"]][["oordeel"]][["twijfelachtig"]]
             qcids <- metingen$qcid
             v1 <- intersect(ids, qcids)
             expect_true(length(v1) == 0)

             expect_true(nrow(metingen) == nrow(x))

             x1 <- x_attr[["QC4a"]][["resultaat"]]
             expect_equal(length(x1), 3)

             x1 <- x_attr[["QC4a"]][["resultaat"]][["plot_param"]][[1]]
             expect_s3_class(x1, "gg")
             expect_s3_class(x1, "ggplot")

})


test_that("QC4a T2", {
             # test met afwijkende meting

             data(metingen)
             data(parameter)

             d1 <- metingen %>%
                 filter(parameter == "Cl" | parameter == "NO3")

             d2 <- metingen %>%
                 filter(parameter == "Cl" | parameter == "NO3") %>%
                 mutate(jaar = jaar - 5) %>%
                 bind_rows(d1)

             d2$waarde[1] <- 1e6
             qcid_twijfel <- d2$qcid[1]

             x <- QC4a(d_metingen = d2, d_parameter = parameter)
             x_attr <- attr(x, "qcout")
             ids <- x_attr[["QC4a"]][["oordeel"]][["twijfelachtig"]]
             qcids <- d2$qcid
             v1 <- intersect(ids, qcids)
             expect_true(length(v1) > 0)
             expect_false(any(v1 != ids))
             expect_equal(qcid_twijfel, ids)



})

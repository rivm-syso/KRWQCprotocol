test_that("QC0a",{

              data(veld)
              data(put)
              data(metingen)

              x <- QC0a(d_veld=veld,d_put=put,d_metingen=metingen,verbose=FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x,"qcout")
              expect_false(is.null(x_attr[["QC0a"]]))
              expect_true(is.list(x_attr[["QC0a"]][["resultaat"]]))

              # test if ids are from metingen data.frame
              ids <- x_attr[["QC0a"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids,qcids)
              expect_true(length(v1)>0)
              expect_false(any(v1!=ids))
              expect_true(nrow(metingen) == nrow(x))
})

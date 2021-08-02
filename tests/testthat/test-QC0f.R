
test_that("QC0f",{


              data(veld)
              data(put)
              data(metingen)

              x <- QC0f(d_veld=veld,d_filter=filter,d_metingen=metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x,"qcout")
              expect_false(is.null(x_attr[["QC0f"]]))
              expect_true(is.list(x_attr[["QC0f"]][["resultaat"]]))


              # test if ids are from metingen data.frame
              ids <- x_attr[["QC0f"]][["oordeel"]][["twijfelachtig"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids,qcids)
              expect_true(length(v1)>0)
              expect_false(any(v1!=ids))

              expect_true(nrow(metingen) == nrow(x))

})

test_that("QC0b T1", {

              data(veld)
              data(put)
              data(metingen)

              # error because we have 'heide' as landuse in the data
              expect_error(QC0b(d_veld=veld,d_put=put,d_metingen=metingen,verbose=TRUE))

})



test_that("QC0b T2", {

              data(veld)
              data(put)
              data(metingen)

              d <- veld %>% dplyr::filter(landgebruik!="heide")
              x <- QC0b(d_veld=d,d_put=put,d_metingen=metingen,verbose=FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              qcout <- attr(x,"qcout")
              expect_false(is.null(qcout[["QC0b"]]))
              expect_true(is.list(qcout[["QC0b"]][["resultaat"]]))

               # no twijfelachtige cases
               ids <- qcout[["QC0b"]][["oordeel"]][["twijfelachtig"]]
               qcids <- metingen$qcid
               v1 <- intersect(ids,qcids)
               expect_false(length(v1)>0)

               expect_false(any(v1!=ids))

              expect_true(nrow(metingen) == nrow(x))

})

test_that("QC0b T2", {

              data(veld)
              data(put)
              data(metingen)

               # verwissel paar landgebruiksgegevens in veld tabel
               ids <- veld %>% dplyr::filter(landgebruik=="grasland") %>%dplyr::pull(qcid)
               ids <- sample(ids,size=10,replace=TRUE)
               d <- veld %>% dplyr::mutate(landgebruik=ifelse(qcid%in%ids,"akkerbouw",landgebruik)) %>%
                   dplyr::filter(landgebruik!="heide")

               v2 <- QC0b(d_veld=d,d_put=put,d_metingen=metingen,verbose=FALSE)


               qcout <- attributes(v2)$qcout

               ids <- qcout[["QC0b"]][["oordeel"]][["twijfelachtig"]]
               resultaat_df <- qcout[["QC0b"]][["resultaat"]]

               qcids <- metingen$qcid
               v1 <- intersect(ids,qcids)
               expect_true(length(v1)>0)
               expect_false(any(intersect(qcids,ids)!=ids))

              expect_true(nrow(metingen) == nrow(v2))

})

test_that("QC3e T1", {
              # Test default values

              data(metingen)
              data(parameter)
              #example data contains multiple EC measurements

              x <- QC3e(d_metingen = metingen)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC3e"]]))
              expect_true(is.list(x_attr[["QC3e"]][["resultaat"]]))

              ids <- x_attr[["QC3e"]][["oordeel"]][["twijfelachtig"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_equal(nrow(metingen),nrow(x))


})


test_that("QC3e T2 Stuyfzand data ", {

              st31 <- stuyfzandtable31 %>%
                  mutate(al = 0, fe = 0, mn = 0) %>%
                  rename(hv = h)
              names(st31) <- paste("x", names(st31), sep="")
              x <- BerekenGeleidbaarheid(metveldgemiddelden = st31, celcius = 25,
                                         add_bicarbonate = FALSE, 
                                         add_phosphate = FALSE)
              expect_false(any(abs(x$percentageverschil_xecv_ec25) > 6))

})



test_that("QC3e T3", {
      # test niet uitvoerbaar

      data(metingen)
      data(veld)

      v2 <- metingen %>% 
          filter(monsterid<=20)

      d2_id <- v2 %>%
          filter(parameter == "Ca") %>%
          pull(qcid)

      d2 <- v2 %>%
          mutate(waarde = if_else(qcid == d2_id[1], NA_real_, waarde))

      monster <- metingen %>% 
          filter(qcid == d2_id[1]) %>%
          pull(monsterid)


      idnum <- d2 %>%
          filter(monsterid == monster) %>%
          nrow()


      x <- QC3e(d_metingen = d2)
      x_attr <- attr(x, "qcout")
      ids <- x_attr[["QC3e"]][["oordeel"]][["niet uitvoerbaar"]]

      expect_true(length(ids) == idnum)

})



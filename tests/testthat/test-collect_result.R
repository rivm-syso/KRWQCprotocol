
test_that("collect_result", {

              qctest <- list()
              test_results <- 1:10
              qcids <- 20
              tests <- c("QC1a", "QC2a", "QC3a")
              x <- data.frame(qcid = 1:qcids, v1 = rnorm(qcids), v2 = rnorm(qcids))
              for (i in tests) {
                  qctest[[i]] <- list("oordeel" = list("verdacht" = test_results))
              }
              attr(x, "qcout") <- qctest

              x2 <- collect_result(x)

              expect_equal(nrow(x2), qcids)
              expect_equal(names(x2), c("qcid", tests))
              expect_equal(x2[[tests[1]]][1], factor("verdacht",
                                                     levels = c("onverdacht", "verdacht")))
              expect_equal(x2[[tests[2]]][11], factor("onverdacht",
                                                      levels = c("onverdacht", "verdacht")))

})





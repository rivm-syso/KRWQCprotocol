#context("QCout helpers")


test_that("qcout_check_waarde",{

              data(veld)
              expect_error(qcout_check_waarde(obj=veld,waarde="onbekend",test="QCa0"))
              object <- data.frame(x=rnorm(10),y=rnorm(10))
              qcout <- qcout_check_waarde(obj=object,waarde="rapportage",test="QC0a")
              expect_false(is.null(qcout[["QC0a"]]))
              expect_equal(names(qcout[["QC0a"]]),"rapportage")

})


test_that("qcout_attrexists",{

              # willekeurig R object
              x <- c(1)
              expect_false(qcout_attrexists(x))
              attr(x,"qcout") <- 1
              expect_true(qcout_attrexists(x))


})


test_that("qcout_add_oordeel",{

              data(veld)
              expect_error(qcout_add_oordeel("QCa0",veld,oordeel="verkeerd",ids=c(1:10)))
              expect_error(qcout_add_oordeel("QCa0",veld,oordeel=character(),ids=c(1:10)))

              veld <- qcout_add_oordeel("QCa0",veld,oordeel="verdacht",ids=c(1:10))
              expect_true(qcout_attrexists(veld))
              qcout <- attributes(veld)[["qcout"]]
              expect_equal(qcout[["QCa0"]][["oordeel"]][["verdacht"]],1:10)

              veld <- qcout_add_oordeel("QCa0",veld,oordeel="twijfelachtig",ids=c(11:20))
              qcout <- attributes(veld)[["qcout"]]
              expect_equal(qcout[["QCa0"]][["oordeel"]][["verdacht"]],1:10)
              expect_equal(qcout[["QCa0"]][["oordeel"]][["twijfelachtig"]],11:20)

              rm(veld)
              data(veld)

              veld <- qcout_add_oordeel("QCa0",veld,oordeel="verdacht",ids=c())

              rm(veld)
})


test_that("qcout_add_rapportage",{
              data(veld)

              testtekst <- "dit is een test tekstje \nmet newline karakter"
              veld <- qcout_add_rapportage(test="QCa0",obj=veld,
                                             tekst=testtekst)
              qcout <- attributes(veld)[["qcout"]]
              expect_equal(qcout[["QCa0"]][["rapportage"]],testtekst)



})


test_that("qcout_add_resultaat",{

              resultaat_df <- data.frame(x=rnorm(10),y=rnorm(10))
              veld <- qcout_add_resultaat(test="QCa0",obj=veld,
                                            resultaat=resultaat_df)
              qcout <- attributes(veld)[["qcout"]]
              expect_equal(qcout[["QCa0"]][["resultaat"]],resultaat_df)
})


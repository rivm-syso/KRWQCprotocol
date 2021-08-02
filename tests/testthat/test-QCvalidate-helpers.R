test_that("testKolommenVeld",{
              NULL

})

test_that("testKolommenPut",{
              NULL
})

test_that("testKolommenFilter",{
              fa <- data(filter)
              f <- get(fa)
              expect_true(testKolommenFilter(f))
              x <- f %>% rename(fout = "filter")
              expect_error(testKolommenMetingen(x))
              x <- f %>%
                  mutate(diepte_onder = as.character(diepte_onder))

})

test_that("testKolommenParameter",{
              NULL
})

test_that("testKolommenMetingen",{

              data(metingen)
              expect_true(testKolommenMetingen(metingen))
              x <- metingen %>% rename(fout = "filter")
              expect_error(testKolommenMetingen(x))
              x <- metingen %>%
                  mutate(waarde = as.character(waarde))

})


test_that("testCoordinaten",{
              NULL
})


test_that("valideLandgebruiken",{
              NULL
})

test_that("valideRedocklasses",{
              NULL
})

test_that("valideParaminfo",{
              NULL
})

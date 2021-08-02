

# lokaal pad naar de data (dit gaat dus niet werken als we package
# publiceren

pad <- "../KRWQCprotocol-proto/data"

BRO_bemonsteringsapparaat <- readRDS(file.path(pad,"BRO_bemonsteringsapparaat.rds"))
usethis::use_data(BRO_bemonsteringsapparaat, overwrite = TRUE)

BRO_bemonsteringsprocedure <- readRDS(file.path(pad,"BRO_bemonsteringsprocedure.rds"))
usethis::use_data(BRO_bemonsteringsprocedure, overwrite = TRUE)

BRO_beoordelingsprocedure <- readRDS(file.path(pad,"BRO_beoordelingsprocedure.rds"))
usethis::use_data(BRO_beoordelingsprocedure, overwrite = TRUE)

BRO_parameterlijst <- readRDS(file.path(pad,"BRO_parameterlijst.rds"))
usethis::use_data(BRO_parameterlijst, overwrite = TRUE)

BRO_waardebepalingsprocedure <- readRDS(file.path(pad,"BRO_waardebepalingsprocedure.rds"))
usethis::use_data(BRO_waardebepalingsprocedure, overwrite = TRUE)

BRO_waardebepalingstechniek <- readRDS(file.path(pad,"BRO_waardebepalingstechniek.rds"))
usethis::use_data(BRO_waardebepalingstechniek, overwrite = TRUE)







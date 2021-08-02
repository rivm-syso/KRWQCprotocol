
# lokaal pad naar de data (dit gaat dus niet werken als we package
# publiceren

pad <- "../KRWQCprotocol-proto/data"

veld <- readRDS(file.path(pad,"veld.rds"))
usethis::use_data(veld, overwrite = TRUE)

metingen <- readRDS(file.path(pad,"metingen.rds"))
usethis::use_data(metingen, overwrite = TRUE)

put <- readRDS(file.path(pad,"put.rds"))
usethis::use_data(put, overwrite = TRUE)

parameter <- readRDS(file.path(pad,"parameter.rds"))
usethis::use_data(parameter, overwrite = TRUE)

filter <- readRDS(file.path(pad,"filter.rds"))
usethis::use_data(filter, overwrite = TRUE)

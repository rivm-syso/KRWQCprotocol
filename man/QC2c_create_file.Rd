% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC2c.R
\name{QC2c_create_file}
\alias{QC2c_create_file}
\title{QC2c_create_file. Maak bestand voor Controle QC2c}
\usage{
QC2c_create_file(dir)
}
\arguments{
\item{dir}{directory waarin het bestand aangemaakt moet worden

Deze functie maakt een csv bestand aan genaamd
'QC2c_veldwaarnemingen.csv'. Dit bestand wordt aangemaakt in de
opgegeven directory. Als het bestand al bestaat dan wordt het niet
overschreven.

Het aangemaakte bestand moet vervolgens met de hand ingevuld worden
met behulp van de informatie afkomstig van de beheerders /
veldmedewerkers.

Als het bestand correct is ingevuld dan kan het verwerkt worden
met behulp van  \code{\link[=QC2c]{QC2c()}}. Let op, \code{\link[=QC2c]{QC2c()}} verwacht dezelfde
bestandsnaam, verander deze dus niet.}
}
\description{
Maak een csv bestand aan wat ingevuld moet worden voor de
handmatige controle van de representativiteit van de put en filter.
}

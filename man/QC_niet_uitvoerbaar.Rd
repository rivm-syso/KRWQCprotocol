% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_niet_uitvoerbaar.R
\name{QC_niet_uitvoerbaar}
\alias{QC_niet_uitvoerbaar}
\title{QC_niet_uitvoerbaar}
\usage{
QC_niet_uitvoerbaar(d_metingen, qctest)
}
\arguments{
\item{d_metingen}{data.frame waarbij een QC test niet uivoerbaar is}

\item{qctest}{character string om aan te geven welke QC test niet uitvoerbaar is}
}
\value{
metingen bestand met niet uitvoerbare locaties/monsters.
}
\description{
Ken het oordeel niet uitvoerbaar toe aan alle locaties/monsters van een
speciefieke test uit het metingen bestand
}
\details{
Indien een QC test niet uitvoerbaar is omdat het metingen bestand niet
volledig is wordt deze functie gebruikt om het oordeel "niet uitvoerbaar"
toe te voegen aan het metingen bestand.
}

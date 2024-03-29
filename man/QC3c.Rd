% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC3c.R
\name{QC3c}
\alias{QC3c}
\title{QC3c. Controle ionenbalans}
\usage{
QC3c(d_metingen, ph_naam = "pH", hco3_naam = "HCO3", verbose = F)
}
\arguments{
\item{d_metingen}{dataframe met metingen}

\item{ph_naam}{character string om te gebruiken als pH. Staat standaard
op "pH". Enkel in het geval dat HCO3 in het veld is gemeten vul hier de naam van
de ph veld parameter in.}

\item{hco3_naam}{character string om te gebruiken als HCO3. Staat standaard
op "HCO3". Enkel in het geval dat HCO3 in het veld is gemeten vul hier de naam van
de hco3 veld parameter in.}

\item{verbose}{of tekstuele output uit script gewenst is (T) of niet (F). Staat
standaard op F.}
}
\value{
metingen bestand met verdachte locaties/monsters.
}
\description{
Controle en berekening van de ionenbalans (IB)
op monsterniveau
}
\details{
Bereken de ionenbalans volgens de methode in
Bijlage II van het QC Protocol. De signaleringswaarde
voor monsters is IB >10\%. Als de ionenbalans
boven de signaleringswaarde ligt, ken het concept
QC oordeel twijfelachtig toe aan het monster.
}

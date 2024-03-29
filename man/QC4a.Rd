% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC4a.R
\name{QC4a}
\alias{QC4a}
\title{QC4a. Controle consistentie metingen met historische gegevens}
\usage{
QC4a(
  d_metingen,
  d_parameter,
  meetronde = max(d_metingen$jaar),
  zscore = 3.5,
  plt = T,
  plt_put_reeks = F,
  verbose = F
)
}
\arguments{
\item{d_metingen}{dataframe met metingen}

\item{d_parameter}{dataframe met parameter informatie}

\item{meetronde}{meetjaar welke gevalideerd dient te worden. Staat standaard
op de laatste meetjaar uit het databestand.}

\item{zscore}{z-score waarbij een meeting wordt bestempeld met twijfelachtig.
staat standaard op 3.5. Alle beschrijvingen zijn gebaseerd op z-score 3.5.}

\item{plt}{Voeg tijdserie plots per reeks en stof toe aan resultaat}

\item{plt_put_reeks}{Maak plots van alle putten waarbij 1 of meer parameters
een z-score > 3.5 hebben (vereist plt = T). Staat standaard op F.}

\item{verbose}{of tekstuele output uit script gewenst is (T) of niet (F).
Staat standaard op F.}
}
\value{
metingen bestand met verdachte locaties/monsters.
}
\description{
Beoordeling waarschijnlijk meting op basis van consistentie
met historische gegevens.

Per putfilter wordt voor elke parameter een vergelijking met het
verleden gemaakt. Dit kan worden gedaan voor ieder meetjaar, indien er
voldoende voorgaande metingen ter beschikking zijn. De consistentie van
metingen met de historische meetwaarden wordt in standaardafwijking
uitgedrukt. Een visuele inspectie vindt plaats voor de metingen met een
standaardafwijking van >3.5 ten opzichte van de historische meetreeks. Van
de metingen die na visuele inspectie als afwijkend van de tijdreeks worden
beoordeeld, ken het concept QC oordeel twijfelachtig toe aan de betreffende
parameter.
Zie bijlage II van het QC protocol voor de methodiek ter beoordeling van
de consistentie van meetwaarden.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_map.R
\name{custom_map}
\alias{custom_map}
\title{Extract Geotiffs for custom map design}
\usage{
custom_map(N = 5778800, E = 268000, S = 5757000, W = 266400,
  fname = "I:/Maps/dgk5gru", type = "tfw", name = "Sub")
}
\arguments{
\item{N}{northern coordinate}

\item{E}{eastern coordinate}

\item{S}{southern coordinate}

\item{W}{western coordinate}

\item{fname}{path to a folder containing the files}

\item{type}{file ending of the source file containing "Rechts" und "Hochwerte"}

\item{name}{prefix of a subfolder in fname}
}
\description{
copies all files from the DGK5 (NRW) to a new folder in order to desingn a map spanning the requested area
}
\details{
coordinates correspond to ETRS89/UTM zone 32N (EPSG:25832)
}

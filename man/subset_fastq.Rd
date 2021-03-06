% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_fastq.R
\name{subset_fastq}
\alias{subset_fastq}
\title{Subset a FASTQ file by another FASTQ file}
\usage{
subset_fastq(reference = NULL, query = NULL, out = "subset.fastq")
}
\arguments{
\item{reference}{name of the reference file}

\item{query}{name of the query file}

\item{out}{path to the output file}
}
\description{
By sampling sequence headers of a reference file, corresponding sequences of a query file is selected to
return the query file with the same dimension as the reference. A common case is to subset a mapping file after
reads were filtererd.
}

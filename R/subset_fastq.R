#' Subset a FASTQ file by another FASTQ file
#'
#' @description
#' By sampling sequence headers of a reference file, corresponding sequences of a query file is selected to
#' return the query file with the same dimension as the reference. A common case is to subset a mapping file after
#' reads were filtererd.
#'
#' @param reference
#' name of the reference file
#'
#' @param query
#' name of the query file
#'
#' @param out
#' path to the output file
#'
#' @export
#'
subset_fastq <- function(reference = NULL, query = NULL, out = "subset.fastq") {

## define helper functions
read_header <- function(path, line) {
    temp <- read.table(file = path, skip = ifelse(line > 1, line - 1,0), nrows = 1)
    paste(temp$V1, temp$V2)
}


## count lines
n_q <- as.integer(ShortRead::countLines(query))
n_r <- as.integer(ShortRead::countLines(reference))

## get headers
pbapply::pboptions(type = "timer", char = "+", style = 1)
header_q <- as.factor(unlist(pbapply::pblapply(X = seq(1, n_q, 4), function(y) read_header(path = query, line = y))))
header_r <- as.factor(unlist(pbapply::pblapply(X = seq(1, n_r, 4), function(y) read_header(path = reference, line = y))))

header_q[1:3]
header_r[1:3]

wanted <- which(header_q %in% header_r)

}


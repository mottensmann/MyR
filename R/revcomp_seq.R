#' Return the reverse complement of a DNA sequence
#'
#' @inheritParams comp_seq
#'
#' @export
#'
revcomp_seq <- function(x) {
    xx <- comp_seq(x)
    xx <- unlist(strsplit(toupper(xx),NULL))
    xx <- xx[seq(length(xx),1,-1)]
    xx <- paste(xx, collapse = "")
    return(xx)
}




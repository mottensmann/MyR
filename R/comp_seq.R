#' Return the complement of a DNA sequence
#'
#' @param x
#' a DNA sequence
#'
#' @export
#'
comp_seq <- function(x) {
bases = c("A","C","G","T")
xx <- unlist(strsplit(toupper(x),NULL))
        paste(unlist(lapply(xx,function(bbb) {
            if (bbb == "A") compString <- "T"
            if (bbb == "C") compString <- "G"
            if (bbb == "G") compString <- "C"
            if (bbb == "T") compString <- "A"
            if (!bbb %in% bases) compString <- "N"
            return(compString)
        })),collapse = "")
}

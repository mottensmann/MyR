#' Find local Maxima in a time series
#'
#' @param df a data frame
#'
#' @param y
#' variable name of the y coordinate
#'
#' @export
#'
find_peaks <- function(df, y = "y") {
    shape <- diff(sign(diff(df[[y]], na.pad = FALSE)))
    pks <- which(shape < 0) # find potential turning points
    pks <- pks[which(df[[y]][pks] > 0)] # remove zero intensity "peaks", they are chunk
    pks <- pks + 1
    if (any(diff(pks) == 1)) pks <- pks[-which(diff(pks) == 1)]
    return(df[pks,])
}

#' insert minor axis ticks with blank label
#'
#' @param x vector of main axis labels
#' @param nth number of blanks between two elements of major_labs
#' @param empty logigcal
#' @param inverse logical
#' @source https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r
#' @export
#'
minor_ticks <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if (empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if (empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

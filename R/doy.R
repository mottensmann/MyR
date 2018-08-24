#' Day of year from date
#'
#' @param date either object class date or a string of format "YYYY-MM-DD" that can be coerced into date format
#'
#' @examples
#' ## day of current date
#' doy()
#'
#' @export
#'
doy <- function(date = Sys.Date()) {
if (is.character(date)) as.Date(date, origin = substr(date, 1,4))

  out <- strftime(date, format = "%j")
  return(as.numeric(out))
}

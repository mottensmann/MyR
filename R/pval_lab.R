#' Add p.value to plot with sensible precision
#'
#' @param p.value numeric value
#' @param significance logical
#' @param digits integer
#' @export
#'
pval_lab <- function(p.value, digits = 2, significance = F) {
  # check alpha
  if (p.value < 0.05 & p.value > 0.01) { # alpha .05*
    out <- ifelse(round(p.value, digits) != 0, paste0("=",round(p.value, digits)), '<0.05')
    if (isTRUE(significance)) out <- paste0(out, '*')
  } else if (p.value < 0.01 & p.value > 0.001) { # alpha .01**
    out <- ifelse(round(p.value, digits) != 0, paste0("=",round(p.value, digits)), '<0.01')
    if (isTRUE(significance)) out <- paste0(out, '**')
  } else if (p.value < 0.001) { # alpha .001***
    out <- ifelse(round(p.value, digits) != 0, paste0("=",round(p.value, digits)), '<0.001')
    if (isTRUE(significance)) out <- paste0(out, '***')
  } else {# > .5 ns
    out <- ifelse(round(p.value, digits) != 1, paste0("=",round(p.value, digits)), '>0.05')
    if (isTRUE(significance)) out <- paste0(out, 'ns')
  }
  return(out)
}


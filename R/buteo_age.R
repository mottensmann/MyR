#' Estimate the age of juvenile buzzards based on wing length
#'
#' @description Buzzard age is estimated based on a standard growth curve modeled from published data by Rob G. Bijlsma (1999)
#'
#' @param df data frame containing data
#' @param wing column name for wing measurements [mm]
#' @param sex column name giving the sex of individuals
#' @references
#' Bijlsma, RG 1999: Sex determination of nestling Common Buzzards Buteo buteo. Limosa 72, 1.10
#'
#' @example
#' ## 240 mm wing lenght
#' buteo_age(data.frame(wing = c(240,240), sex = c("male","female")))
#'
#' @export
#'
#'
#'
buteo_age <- function(df = NULL, wing = "wing", sex = "sex", se.fit = T) {

  ## format data
  to_predict <- data.frame(mean = df[[wing]],
                           sex = df[[sex]])

  ## get standard growth data
  path <- system.file("extdata", "buzzard_wing.txt", package = "MyR")
  data <- read.table(path, header = T)

  ## build model
  model <- lm(age ~ mean + sex, data)
  predict.lm(model, to_predict, se.fit = se.fit)
}


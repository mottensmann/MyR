#' Extract Geotiffs for custom map design
#'
#' @param N
#' northern coordinate
#'
#' @param E
#' eastern coordinate
#'
#' @param S
#' southern coordinate
#'
#' @param W
#' western coordinate
#'
#' @param fname
#' path to a folder containing the files
#'
#' @param name
#' prefix of a subfolder in fname
#'
#' @param type
#' file ending of the source file containing "Rechts" und "Hochwerte"
#'
#' @description
#' copies all files from the DGK5 (NRW) to a new folder in order to desingn a map spanning the requested area
#'
#' @details coordinates correspond to ETRS89/UTM zone 32N (EPSG:25832)
#'
#'
#'@export
#'
custom_map <- function(N = 5778800, E = 268000, S = 5757000, W = 266400, fname = "I:/Maps/dgk5gru", type = "tfw", name = "Sub") {
    # rm(list = ls())
    # N <- 5776000
    # E <- 347000
    # S <- 5754000
    # W <- 345000
    # fname <- "I:/Maps/dgk5gru"
    # type <- "tfw"
    ## get file names
    files <- list.files(path = fname,pattern = type)

    ## set up a data frame
    df <- data.frame(file = files, easting = NA, northing = NA)

    df[["northing"]] <- lapply(files, FUN =  function(a) round(as.numeric(readLines(paste0(fname,"/",a))[[6]])))
    df[["easting"]] <- lapply(files, FUN =  function(a) round(as.numeric(readLines(paste0(fname,"/",a))[[5]])))


     ## find all files within the frame given by the coordinates

    file_subset <- subset(df, subset = northing <= N & northing >= S & easting >= W & easting <= E)
    files_2cp <- unlist(lapply(file_subset[["file"]],FUN = function(a) paste0(fname,"/",a)))
    files_2cp2 <- unlist(lapply(files_2cp, FUN = function(a) paste0(strsplit(a,".tfw"),".tif")))

    c <- 1
    while (file.exists(paste0(fname,"/",name))) {
            name <- paste0(name,"_",as.character(c))
            c <- c + 1
        }

    dir.create(file.path(fname, name))

    file.copy(files_2cp,paste0(fname,"/",name))
    file.copy(files_2cp2,paste0(fname,"/",name))

}

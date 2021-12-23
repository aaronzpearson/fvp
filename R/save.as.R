#' Save As
#'
#' These functions allow users to save results returned from \code{_.player.profile()}, \code{_.results._()}, and
#' \code{compare.player._()} functions as a Microsoft Excel document, CSV document, or RDS (Rdata) file
#'
#' @param data.set The player's returned data.frame
#' @param file.name The intended file name
#'
#' @export
save.as.csv <- function(data.set, file.name) {

  save.as <- paste0(file.name, ".csv")
  write.csv(data.set, save.as)

  print(paste(save.as, "was saved to", getwd()))

}

#' @describeIn save.as.csv Saves a player's data as a .csv file
#' @export
save.as.excel <- function(data.set, file.name) {

  save.as <- paste0(file.name, ".csv")
  write.csv(data.set, save.as)

  print(paste(save.as, "was saved to", getwd()))

}

#' @describeIn save.as.csv Saves a player's data as an Rdata file
#' @export
save.as.rdata <- function(data.set, file.name) {

  save.as <- paste0(file.name, ".rds")
  saveRDS(data.set, save.as)

  print(paste(save.as, "was saved to", getwd()))

}

#' @describeIn save.as.csv Saves a player's data as an RDS file
#' @export
save.as.rds <- function(data.set, file.name) {

  save.as <- paste0(file.name, ".rds")
  saveRDS(data.set, save.as)

  print(paste(save.as, "was saved to", getwd()))

}

#' @title Compare Players
#'
#' This function returns a data.frame that compares players' mechanical sprint abilities using either their
#' player profiles.
#'
#' @param player.profiles The players' profiles as a list
#'
#' @return A comparison of players' profiles or speed, acceleration, and split times at different distances
#' @export
compare.player.profiles <- function(player.profiles = list()) {

  df.compare <- do.call(rbind, player.profiles)
  df.compare

}

#' @describeIn compare.player.profiles Compare player split times at given distances
#' @export
compare.player.splits <- function(player.profiles = list(),
                                  distance = c(9.14, 18.3, 36.6 )) {

  player.names <- do.call(rbind, player.profiles)$player.name
  df.compare <- data.frame(player.name = rep(player.names, each = length(distance)))

  player.splits <- do.call(rbind, lapply(player.profiles, player.splits, distance = distance))

  df.compare <- cbind(df.compare, player.splits)
  df.compare

}

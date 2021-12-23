#' Player Split Times
#'
#' This function returns projected speed, acceleration, and split times based on their mechanical sprint abilities.
#'
#' @param player.profile A player's sprint profile
#' @param distance The distance that corresponds with the split times recorded
#'
#' @return
#' @export
player.splits <- function(player.profile,
                          distance = c(9.14, 18.3, 36.6)) {

  player.splits <- data.frame(distance = distance)
  player.splits$split.time <- time.distance(player.profile, player.splits$distance)
  player.splits$split.speed <- speed.time(player.profile, player.splits$split.time)
  player.splits$split.accel <- accel.time(player.profile, player.splits$split.time)

  player.splits

}

#' @describeIn player.splits Modeled speed, acceleration, and time observations from player tracking data
#' @export
gps.player.splits <- function(player.profile,
                              distance = c(9.14, 18.3, 36.6)) {

  player.splits(player.profile, distance = distance)

}

#' @describeIn player.splits Modeled speed, acceleration, and time observations from speed-acceleration data
#' @export
sa.player.splits <- function(player.profile,
                             distance = c(9.14, 18.3, 36.6)) {

  player.splits(player.profile, distance = distance)

}

#' @describeIn player.splits Speed, acceleration, and time observations from modeled sprint data
#' @export
scout.player.splits <- function(player.profile,
                                distance = c(9.14, 18.3, 36.6)) {

  player.splits(player.profile, distance = distance)

}

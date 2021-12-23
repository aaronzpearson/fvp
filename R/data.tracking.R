#' Player Tracking Data
#'
#' This is a generic function that builds a data.frame that can be applied in subsequent analyses. If the athlete's data
#' is not in the form provided by this function, the \code{_.player.profile()} functions will return an error because
#' of inconsistent variable names.
#'
#' Although \code{_.data()} functions return cleaned data, they do not behave the same. Rather, each function
#' is built to accomodate future analyses within the same family of functions. For example, \code{gps.data()} and
#' \code{sa.data()} functions build data sets that are comprised of a player's observed speed and acceleration. Conversely,
#' \code{scout.data()} takes on a players distance and time splits. Therefore, it is recommended that functions be
#' kept within their family.
#'
#' Functions that are of the form \code{_.data._()} do not handle player tracking data. Rather, they are utilized in
#' analyses that require more information like a player's height and mass.
#'
#' @note The input game data is assumed to be in metric. Speed must be in m/s and acceleration in m/s/s. If it is not,
#' values that are returned are not representative of a player's true sprint potential. If your data needs to be converted,
#' use the \code{convert.to.metric()} function. See the \code{convert.to.metric()} documentation for examples on automating
#' the process.
#'
#' @param game.speed The player's speed vector
#' @param game.accel The player's acceleration vector
#'
#' @return A data.frame that contains clean player tracking data
#' @export
data.tracking <- function(game.speed, game.accel) {

  player <- data.frame(game.speed = game.speed,
                       game.accel = game.accel)

}

#' @describeIn data.tracking Handles GPS and LPS player tracking data
#' @export
gps.data <- function(game.speed, game.accel) {

  athlete <- data.frame(game.speed = game.speed,
                        game.accel = game.accel)
  athlete

}

#' @describeIn data.tracking Handles speed-acceleration player tracking data
#' @export
sa.data <- function(game.speed, game.accel) {

  sa.player <- data.frame(game.speed = game.speed,
                          game.accel = game.accel)
  sa.player

}


#' Split Time Data
#'
#' This function builds the data.frame necessary to fit an athlete's mechanical sprint ability optimization
#' function. The argument \code{distance = c(9.14, 18.3, 36.6)} contains distances that are equivalent to the
#' 10, 20, and 40 yard splits.
#'
#' Although this function can handle fewer split times, it is not recommended to do so. Subsequent analyses
#' rely on models that were built, and have been validated, utilizing the distances provided.
#'
#' @note The input game data is assumed to be in metric. Speed must be in m/s and acceleration in m/s/s. If it is not,
#' values that are returned are not representative of a player's true sprint potential. If your data needs to be converted,
#' use the \code{convert.to.metric} function.
#'
#' @param distance Distance traveled
#' @param split.time Time to cover the given distance
#'
#' @return The distance and time data.frame required for future analyses
#'
#' @export
#'
scout.data <- function(distance = c(9.14, 18.3, 36.6), split.time) {

  scout.data <- data.frame(distance = distance,
                           split.time = split.time)
  rbind(data.frame(distance = 0,
                   split.time = 0),
        scout.data)

}

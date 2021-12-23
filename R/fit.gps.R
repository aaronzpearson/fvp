#' Best Sprint
#'
#' This function returns the player's best-sprint from the data set.
#'
#' This function identifies a player's best-sprint which is defined as the sprint that took the least amount
#' of time to reach top speed. The caveat being that this sprint does not need to begin at a dead-start.
#'
#' The user sets the minimum speed that the function should consider the start of the sprint. Since players do
#' not often reach max speed during games and practices, the \code{max.speed.threshold} argument allows practitioners
#' to set the percent of top speed that the function should consider as the end of the sprint. Through trial-and-error,
#' the minimum speed of 0.3 m/s and 95% of top speed are appropriate values to build a player's in-game sprint
#' ability.
#'
#' @param game.speed The player's tracking data speed vector
#' @param min.speed The minimum speed that the function should consider the start of the sprint
#' @param max.speed.threshold The percent of the player's top speed reached to be considered the end of the sprint
#' @param sample.rate The game data sample rate (Hz)
#'
#' @return A data.frame with the player's best-sprint observations
#' @export
best.sprint <- function(game.speed,
                        min.speed = 0.3,
                        max.speed.threshold = 95,
                        sample.rate = 10) {

  game.data <- data.frame(speed = game.speed)
  game.data$row.num <- 1:nrow(game.data)

  max.speed <- max(game.data$speed, na.rm = TRUE)
  max.speed.threshold <- (max.speed.threshold/ 100) * max.speed

  game.data$speed.indicator <- ifelse(game.data$speed <= min.speed, 1, 0)
  game.data$speed.indicator <- ifelse(game.data$speed >= max.speed.threshold, 2, game.data$speed.indicator)

  game.sprint.data <- subset(game.data, game.data$speed.indicator != 0)
  game.sprint.data$sprint.indicator <- c(0, diff(game.sprint.data$speed.indicator))


  game.sprint.data$sprint.length <- c(0, diff(game.sprint.data$row.num))
  game.sprint.data$sprint.length <- ifelse(game.sprint.data$sprint.indicator == 1, game.sprint.data$sprint.length, 0)

  game.sprint.data <- subset(game.sprint.data, game.sprint.data$sprint.indicator == 1)

  # if(best.sprint == TRUE) {

  game.sprint.data <- subset(game.sprint.data, game.sprint.data$sprint.length == min(game.sprint.data$sprint.length, na.rm = T))

  sprint.length <- as.numeric(game.sprint.data$sprint.length)
  sprint.start <- as.numeric(game.sprint.data$row.num) - sprint.length
  sprint.end <- as.numeric(game.sprint.data$row.num)

  best.sprint.data <- subset(game.data, game.data$row.num >= sprint.start &
                               game.data$row.num <= sprint.end)

  best.sprint.data <- subset(best.sprint.data, !is.na(speed))
  best.sprint.data$split.time <- 1:nrow(best.sprint.data)

  best.sprint.data <- data.frame(split.time = best.sprint.data$split.time / sample.rate,
                                 observed.speed = best.sprint.data$speed)
  best.sprint.data <- rbind(data.frame(split.time = 0,
                                       observed.speed = 0),
                            best.sprint.data)

  best.sprint.data

}

#' @describeIn best.sprint An alternate function call specifically for player tracking data
#' @export
gps.best.sprint <- function(game.speed,
                            min.speed = 0.3,
                            max.speed.threshold = 95,
                            sample.rate = 10) {

  game.data <- data.frame(speed = game.speed)
  game.data$row.num <- 1:nrow(game.data)

  max.speed <- max(game.data$speed, na.rm = TRUE)
  max.speed.threshold <- (max.speed.threshold/ 100) * max.speed

  game.data$speed.indicator <- ifelse(game.data$speed <= min.speed, 1, 0)
  game.data$speed.indicator <- ifelse(game.data$speed >= max.speed.threshold, 2, game.data$speed.indicator)

  game.sprint.data <- subset(game.data, game.data$speed.indicator != 0)
  game.sprint.data$sprint.indicator <- c(0, diff(game.sprint.data$speed.indicator))


  game.sprint.data$sprint.length <- c(0, diff(game.sprint.data$row.num))
  game.sprint.data$sprint.length <- ifelse(game.sprint.data$sprint.indicator == 1, game.sprint.data$sprint.length, 0)

  game.sprint.data <- subset(game.sprint.data, game.sprint.data$sprint.indicator == 1)

  game.sprint.data <- subset(game.sprint.data, game.sprint.data$sprint.length == min(game.sprint.data$sprint.length, na.rm = T))

  sprint.length <- as.numeric(game.sprint.data$sprint.length)
  sprint.start <- as.numeric(game.sprint.data$row.num) - sprint.length
  sprint.end <- as.numeric(game.sprint.data$row.num)

  best.sprint.data <- subset(game.data, game.data$row.num >= sprint.start &
                               game.data$row.num <= sprint.end)

  best.sprint.data <- subset(best.sprint.data, !is.na(speed))
  best.sprint.data$split.time <- 1:nrow(best.sprint.data)

  best.sprint.data <- data.frame(split.time = best.sprint.data$split.time / sample.rate,
                                 observed.speed = best.sprint.data$speed)
  best.sprint.data <- rbind(data.frame(split.time = 0,
                                       observed.speed = 0),
                            best.sprint.data)

  best.sprint.data

}

#' Fit Best-Sprint
#'
#' This function uses an optimization function to fit the athlete's best-sprint data. The function utilizes
#' the same function as that found in \code{speed.time()} and optimizes the player's acceleration constant to
#' minimize MSE.
#'
#' @param sprint.data The player's best sprint data that is returned from the \code{best.sprint()} function
#'
#' @return An NLS with an optimized acceleration constant (player tau)
#'
#' @export
gps.nls <- function(sprint.data) {

  sprint.nls <- nls(observed.speed ~ max(sprint.data$observed.speed, na.rm = TRUE) *
                      (1 - exp(- sprint.data$split.time/ player.tau)),
                    data = sprint.data,
                    start = list(player.tau = 1))

  sprint.nls

}


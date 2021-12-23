#' Observed Results
#'
#' The \code{_.results.observed()} functions return observations that are found in the original game.data data set.
#' These are data that were used in subsequent analyses within their respective family of functions.
#'
#' \code{sa.results.observed()} returns the observations that were maintained within the data set that fit
#' inclusion criteria to build the model. The speed-acceleration model performs five tasks: 1. speed values below
#' 3 m/s are removed from the data set, 2. speed bins are created every 0.2 m/s, 3. the player's acceleration is
#' grouped by speed bin, 4. the player's acceleration is ordered in descending fashion, 5. the greatest two
#' acceleration rates are kept in the data set and are returned to the user. These data are then applied in
#' the \code{sa.results.fitted()} function to return the final model observations that are required to build the
#' speed-acceleration plot.
#'
#' \code{gps.results.observed{}} returns the observations that make-up the player's best sprint.
#'
#' \code{fv.results.observed()} returns the observations that build the force-velocity model.
#'
#' The \code{_.results.game()}, \code{_.results.fitted()}, and \code{_.results.observed()} return similar data.frames but
#' behave differently on the back-end. Therefore, it is suggested that these functions be used for their intended purposes.
#'
#'
#' @return The observations in the data set that are used to model their respective outcomes
#' @export
results.observed <- function() {

  NULL

}

#' @describeIn results.observed Returns observations that fit speed-acceleration model inclusion criteria
#' @export
sa.results.observed <- function(game.data) {

  sa.player <- game.data[game.data[, "game.speed"] >= 3, ]
  sa.player$speed.bins <- cut(sa.player$game.speed,
                              seq(3, max(sa.player$game.speed, na.rm = TRUE),
                                  by = 0.2))

  sa.player <- sa.player[order(sa.player$game.accel), ]

  sa.player.temp <- by(sa.player, sa.player["speed.bins"], tail, 2)
  sa.player <- Reduce(rbind, sa.player.temp)

  sa.player <- sa.player[order(sa.player$speed), ]
  rownames(sa.player) <- 1:nrow(sa.player)
  colnames(sa.player) <- c("game.speed", "game.accel", "speed.bins")

  sa.player

}

#' @describeIn results.observed Returns observations of a player's best in-game sprint
#' @export
gps.results.observed <- function(game.data,
                                 min.speed = 0.3,
                                 max.speed.threshold = 95,
                                 sample.rate = 10) {

  gps.best.sprint(game.speed = game.data$game.speed,
                  min.speed = min.speed,
                  max.speed.threshold = max.speed.threshold,
                  sample.rate = sample.rate)

}

#' @describeIn results.observed Returns force-velocity observations
#' @export
fv.results.observed <- function(player.data,
                                testing.data) {

  player.profile <- fv.player.profile(player.name = NA,
                                      player.data = player.data,
                                      testing.data = testing.data)

  f0.normalized <- player.profile$f0.normalized
  v0 <- player.profile$v0

  slope <- -1 * (f0.normalized/ v0)

  actual.model <- data.frame(velocity = seq(0, v0, by = 0.01))
  actual.model$force <- f0.normalized + actual.model$vel * slope

  actual.model

}

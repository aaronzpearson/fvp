#' Optimized Best Sprint Results
#'
#' This function compliments \code{gps.results.observed()} in that it fits an optimized speed-time model to a
#' player's observed data. The data.frame returned allows practitioners to build plots displaying an athlete's in-game
#' sprint abilities.
#'
#' @param game.data The player's cleaned game data
#' @param sample.rate The game data sample rate (Hz)
#'
#' @return A data.frame that fits an optimized speed-time model to the athlete's best sprint
#' @export
#'
results.game <- function(game.data,
                         sample.rate = 10) {

  player.profile <- gps.player.profile.game(player.name = NA,
                                            game.data = game.data,
                                            min.speed = 0.3,
                                            max.speed.threshold = 95,
                                            sample.rate = sample.rate)

  game.data <- data.frame(splits = seq(0, 5, by = (1/sample.rate)))

  game.data$model.speed <- speed.time(player.profile, game.data$splits)
  game.data$model.acceleration <- accel.time(player.profile, game.data$splits)
  game.data$model.distance <- distance.time(player.profile, game.data$splits)

  game.data

}

#' @describeIn results.game A player's in-game best-sprint modeled data
#' @export
gps.results.game <- function(game.data,
                             sample.rate = 10) {

  player.profile <- gps.player.profile.game(player.name = NA,
                                            game.data = game.data,
                                            min.speed = 0.3,
                                            max.speed.threshold = 95,
                                            sample.rate = sample.rate)

  game.data <- data.frame(splits = seq(0, 5, by = (1/sample.rate)))

  game.data$model.speed <- speed.time(player.profile, game.data$splits)
  game.data$model.acceleration <- accel.time(player.profile, game.data$splits)
  game.data$model.distance <- distance.time(player.profile, game.data$splits)

  game.data

}

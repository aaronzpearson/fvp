#' Model Results
#'
#' \code{_.results.model()} functions return a data.frame that contains all observations that are required to build
#' plots from the resulting models. Unlike \code{_.player.profile()}, a player's abilities are not returned concisely.
#' Rather, this function was built to provide practitioners the ability to plot player's modeled abilities for
#' simple visualization.
#'
#' \code{_.results._()} functions are built with consistency in mind. Therefore, the majority of them take on a player's
#' observed or tested data. The exception to this rule is \code{fvp._()} functions because secondary analyses are
#' required before returning a player's profile and modeled data.
#'
#' The \code{_.results.game()}, \code{_.results.fitted()}, and \code{_.results.observed()} return similar data.frames but
#' behave differently on the back-end. Therefore, it is suggested that these functions be used for their intended purposes.
#'
#' @param player.data The player's anthropometric data and testing day weather
#' @param testing.data The player's force-velocity testing data
#' @param game.data The player's cleaned game data
#' @param push.off.angle Push-off angle for force-velocity testing (degrees)
#' @param sample.rate The game data sample rate (Hz)
#' @param r2 The minimum r^2 when fitting the model
#'
#'
#' @return
#'
#'
#' @export
#'
results.model <- function(player.data,
                          testing.data,
                          game.data,
                          push.off.angle = 90,
                          sample.rate = 10,
                          r2 = 0.95) {

  player.profile <- gps.player.profile(player.name = NA,
                                       game.data = game.data)

  game.data <- data.frame(splits = seq(0, 5, by = (1/sample.rate)))

  game.data$model.speed <- speed.time(player.profile, game.data$splits)
  game.data$model.acceleration <- accel.time(player.profile, game.data$splits)
  game.data$model.distance <- distance.time(player.profile, game.data$splits)

  game.data

}

#' @describeIn results.model Force-velocity modeled observations
#' @export
fv.results.model <- function(player.data,
                             testing.data,
                             push.off.angle = 90) {

  player.profile <- fv.player.profile(player.name = NA,
                                      player.data = player.data,
                                      testing.data = testing.data)

  player.data <- player.profile[, 2:5]

  optim.slope <- fv.slope(hpo = player.data$push.off.distance,
                          push.off.angle = push.off.angle,
                          pmax = player.profile$pmax.normalized)

  f0.optimal = fv.f0(player.data, player.profile, push.off.angle)
  v0.optimal = fv.v0(player.data, player.profile, push.off.angle)


  fv.optim.data <- data.frame(velocity = seq(0, v0.optimal, by = 0.01))
  fv.optim.data$force <- f0.optimal + optim.slope * fv.optim.data$vel

  fv.optim.data

}

#' @describeIn results.model Player tracking data modeled observations
#' @export
gps.results.model <- function(game.data,
                              sample.rate = 10) {

  player.profile <- gps.player.profile(player.name = NA,
                                       game.data = game.data)

  game.data <- data.frame(splits = seq(0, 5, by = (1/sample.rate)))

  game.data$model.speed <- speed.time(player.profile, game.data$splits)
  game.data$model.acceleration <- accel.time(player.profile, game.data$splits)
  game.data$model.distance <- distance.time(player.profile, game.data$splits)

  game.data

}

#' @describeIn results.model Speed-acceleration modeled observations
#' @export
sa.results.model <- function(game.data,
                             r2 = .95) {

  player.profile <- sa.player.profile(player.name = NA,
                                      game.data = game.data,
                                      r2 = r2)

  x.int <- player.profile$max.speed
  y.int <- player.profile$max.accel
  slope <- -1 * (y.int/ x.int)

  sa.player <- data.frame(speed = seq(0, x.int, by = 0.01))
  sa.player$accel <- slope * sa.player$speed + y.int

  sa.player <- rbind(sa.player, data.frame(speed = x.int,
                                           accel = 0))

  colnames(sa.player) <- c("game.speed", "game.accel")
  sa.player

}

#' @describeIn results.model Sprint modeled observations
#' @export
scout.results.model <- function(sprint.data) {

  player.profile <- scout.player.profile(player.name = NA,
                                         sprint.data = sprint.data)

  game.data <- data.frame(splits = seq(0, 5, by = 0.1))

  game.data$model.speed <- speed.time(player.profile, game.data$splits)
  game.data$acceleration <- accel.time(player.profile, game.data$splits)
  game.data$distance <- distance.time(player.profile, game.data$splits)

  game.data

}

#' @describeIn results.model Force-velocity-power modeled observations
#' @export
fvp.results.model <- function(player.data,
                              player.profile,
                              sprint.duration = 5) {

  athlete <- data.frame(split.time = seq(0, sprint.duration, by = 0.01))

  athlete$speed <- speed.time(player.profile, athlete$split.time)
  athlete$acceleration <- accel.time(player.profile, athlete$split.time)
  athlete$distance <- distance.time(player.profile, athlete$split.time)

  athlete$horizontal.force <- force.horizontal(player.data, athlete$acceleration)
  athlete$air.force <- air.force(air.friction.coef(player.data), athlete$speed)
  athlete$horizontal.net.force <- athlete$horizontal.force + athlete$air.force
  athlete$horizontal.net.force.normalized <- athlete$horizontal.force/ player.data$body.mass
  athlete$horizontal.power <- power.horizontal(athlete$speed, athlete$horizontal.net.force)
  athlete$horizontal.power.normalized <- athlete$horizontal.power / player.data$body.mass
  athlete$vertical.force <- player.data$body.mass * 9.81

  athlete$net.force <- force.resultant(athlete$horizontal.net.force, player.data$body.mass)

  athlete$angle.of.forces <- atan(athlete$vertical.force/ athlete$horizontal.net.force) * (180/ pi)

  athlete$ratio.of.forces <- ratio.of.force(athlete$horizontal.net.force, athlete$net.force)

  suppressWarnings(athlete)

}

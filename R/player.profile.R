#' Player Profiling
#'
#' This is a generic function that builds player profiles from game data much like the functions \code{gps.player.profile()},
#' \code{gps.game.profile()}, \code{sa.player.profile()}, and \code{scout.player.profile}. Depending on the function used,
#' player profiles will return either observed or modeled values.
#'
#' The intention of \code{player.profile()}, and similar functions, is to provide sports practitioners with
#' pertinent information on the given athlete's mechanical sprint abilities. Practitioners can apply this
#' information in \code{_.results.model()} functions to build data sets that represent the athlete's
#' mechanical speeds, accelerations, and distances covered as a function of time.
#'
#' It is suggested that the \code{player.name} argument be filled to be effectively applied to
#' functions like \code{compare.player.profiles()}.
#'
#' Thank you to Patrick Ward for the first iteration of \code{scout.player.profile()}.
#'
#'
#' @note The functions \code{sa.player.profile()} and \code{scout.player.profile()} must be called explicitly because their
#' helper functions differ significantly.
#'
#' @note The functions \code{fv.player.profile()} and \code{fvp.player.profile()} are unique functions and are not assessed
#' in the same manner. Therefore, the generic function is not applicable.
#'
#' @note The input game data is assumed to be in metric. Speed must be in m/s and acceleration in m/s/s. If it is not,
#' values that are returned are not representative of a player's true sprint potential. If your data needs to be converted,
#' use the \code{convert.to.metric()} function. See the \code{convert.to.metric()} documentation for examples on automating
#' the process.
#'
#' @param player.name The player's name
#' @param game.data The player's cleaned game data
#' @param r2 The minimum r^2 when fitting the model
#'
#' @return An athlete's player profile which includes the player's name, their maximal attainable speed,
#' maximal attainable acceleration, and their acceleration constant.
#'
#'
#' @export
#'
player.profile <- function(player.name = NA,
                           game.data,
                           r2 = 0.95) {

  player <- data.frame(player.name = player.name,
                       max.speed = max(game.data$game.speed, na.rm = TRUE),
                       max.accel = max(game.data$game.accel, na.rm = TRUE))
  player$player.tau <- player$max.speed/ player$max.accel

  player

}


#' @describeIn player.profile Manually create a player's profile
#' @export
build.player.profile <- function(player.name = NA,
                                 max.speed,
                                 max.accel) {
  athlete <- data.frame(
    player.name = player.name,
    max.speed = max.speed,
    max.accel = max.accel
  )
  athlete$player.tau <- athlete$max.speed / athlete$max.accel

  athlete
}



#' @describeIn player.profile GPS player profile from a player's potential abilities
#' @export
gps.player.profile <- function(player.name = NA,
                               game.data) {

  athlete <- data.frame(player.name = player.name,
                        max.speed = max(game.data$game.speed, na.rm = TRUE),
                        max.accel = max(game.data$game.accel, na.rm = TRUE))
  athlete$player.tau <- athlete$max.speed/ athlete$max.accel

  athlete

}



#' @describeIn player.profile GPS player profile from a player's observed abilities
#' @export
gps.player.profile.game <- function(player.name = NA,
                                    game.data,
                                    min.speed = 0.3,
                                    max.speed.threshold = 95,
                                    sample.rate = 10) {

  sprint.data <- gps.best.sprint(game.speed = game.data$game.speed,
                                 min.speed = min.speed,
                                 max.speed.threshold = max.speed.threshold,
                                 sample.rate = sample.rate)

  sprint.nls <- gps.nls(sprint.data = sprint.data)

  player.tau <- coef(summary(sprint.nls))[1]
  max.speed <- max(predict(sprint.nls), na.rm = TRUE)
  max.accel <- max((max.speed/ player.tau) *
                     exp(-(1/ sample.rate)/ player.tau), na.rm = TRUE)

  player.profile <- data.frame(player.name = player.name,
                               max.speed = max.speed,
                               max.accel = max.accel,
                               player.tau = player.tau)
  player.profile

}



#' @describeIn player.profile Speed-acceleration player profile from a player's GPS data
#' @export
sa.player.profile <- function(player.name = NA,
                              game.data,
                              r2 = 0.95) {

  sa.player <- sa.lm.fitted(game.data = game.data, r2 = r2)

  sa.lm <- lm(game.accel ~ game.speed, data = sa.player)

  # fmax
  y.int <- coef(sa.lm)[[1]]
  slope <- coef(sa.lm)[[2]]

  # vmax
  x.int <- y.int/abs(slope)

  # returns speed-accel values in the original units
  sa.player <- data.frame(player.name = player.name,
                          max.speed = x.int,
                          max.accel = y.int,
                          player.tau = x.int / y.int,
                          r.square = as.numeric(summary(sa.lm)[8]),
                          n.obervation = nrow(sa.player))

  sa.player

}

#' @describeIn player.profile Sprint test player profile from a player's split times and distances
#' @export
scout.player.profile <- function(player.name = NA,
                                 sprint.data) {

  scout.data <- data.frame(distance = sprint.data$distance,
                           split.time = sprint.data$split.time)

  scout.nls <- nls(distance ~ max.speed *
                     (split.time + player.tau * exp(-split.time/player.tau)) -
                     max.speed * player.tau,
                   data = scout.data,
                   start = list(max.speed = 10,
                                player.tau = 1))

  player.profile <- data.frame(player.name = player.name,
                               max.speed = coef(scout.nls)[[1]],
                               max.accel = coef(scout.nls)[[1]]/ coef(scout.nls)[[2]],
                               player.tau = coef(scout.nls)[[2]])

  player.profile

}

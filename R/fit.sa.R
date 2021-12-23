#' Speed-Accel Linear Model Observations
#'
#' This function returns the observations that fit inclusion criteria when building a player's
#' speed-acceleration profile and the observations kept that subsequently build the linear model.
#'
#' @param game.data The player's cleaned game data
#'
#' @return A data.frame with observations that meet speed-acceleration inclusion criteria
#' @export
#'
sa.lm.observed <- function(game.data) {

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

#' @describeIn sa.lm.observed A data.frame with observations that were maintained to build the speed-acceleration linear model
#' @export
sa.lm.fitted <- function(game.data, r2 = 0.95) {

  sa.player <- sa.lm.observed(game.data)

  fit <- lm(game.accel ~ game.speed, data = sa.player)
  r.square <- summary(fit)[[8]]
  y.int <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  x.int <- y.int/abs(slope)


  while (r.square <= r2) {

    sa.player$fit.predict <- y.int + (slope * sa.player$game.speed)
    sa.player$residual <- abs(sa.player$game.accel - sa.player$fit.predict)
    sa.player <- sa.player[order(sa.player$residual), ]
    # returns df with one less row per loop until loop completes
    sa.player <- head(sa.player, nrow(sa.player) - 1)
    fit <- lm(game.accel ~ game.speed, data = sa.player)
    r.square <- summary(fit)[[8]]
    y.int <- coef(fit)[[1]]
    slope <- coef(fit)[[2]]

  }

  sa.player <- sa.player[order(sa.player$speed), ]
  rownames(sa.player) <- 1:nrow(sa.player)
  colnames(sa.player) <- c("game.speed", "game.accel", "speed.bins", "fit.predict", "residual")

  sa.player

}

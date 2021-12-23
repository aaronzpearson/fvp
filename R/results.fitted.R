#' Fitted Results
#'
#' This function returns the fitted speed-acceleration results. This data can then be used to build an athlete's speed-acceleration
#' linear model.
#'
#' This function is complimented by \code{sa.results.observed()}.
#'
#' The observations provided by \code{sa.results.observed()} are fit to a linear model. A looped function removes the
#' observation with the largest residual, re-fitting the linear model, and assessing whether the model has reached an
#' r^2 of equal to, or greater than, that provided by the user. The r^2 value provided (0.95) corresponds with what
#' is suggested in the literature.
#'
#' @param game.data A player's game data
#' @param r2 The minimum r^2 when fitting the model
#'
#' @return A data frame that returns fitted speed-acceleration observations
#' @export
results.fitted <- function(game.data,
                           r2 = 0.95) {

  sa.player <- sa.results.observed(game.data)

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

#' @describeIn results.fitted Fitted speed-acceleration observations
#' @export
sa.results.fitted <- function(game.data,
                              r2 = 0.95) {

  sa.player <- sa.results.observed(game.data)

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

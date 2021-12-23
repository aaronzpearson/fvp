#' Force-Velocity Player Profile
#'
#' This function return's a player's force-velocity profile. The values returned can be subsequently used to
#' build force-velocity plots and compare players' abilities.#'
#'
#'
#' @param player.name The player's name
#' @param player.data The player's anthopometric data
#' @param testing.data The athlete's jump-based testing data
#'
#' @return A player's jump-based force-velocity profile
#' @export
fv.player.profile <- function(player.name = NA, player.data, testing.data) {


  fv.lm <- lm(force ~ vel, data = testing.data)

  athlete.results <- data.frame(r2 = summary(fv.lm)$r.squared,
                                f0 = as.numeric(coef(fv.lm)[1]),
                                sfv = as.numeric(coef(fv.lm)[2]))

  athlete.results$r <- sqrt(athlete.results$r2)
  athlete.results$f0.normalized = athlete.results$f0 / player.data$body.mass
  athlete.results$sfv.normalized = athlete.results$sfv / player.data$body.mass
  athlete.results$v0 = -1 * athlete.results$f0 / athlete.results$sfv
  athlete.results$pmax = (athlete.results$f0 * athlete.results$v0)/ 4
  athlete.results$pmax.normalized = athlete.results$pmax / player.data$body.mass
  athlete.results$sfv.optimal = fv.slope(hpo = player.data$push.off.distance,
                                               push.off.angle = player.data$push.off.angle,
                                               pmax = athlete.results$pmax.normalized)

  athlete.results$f0.optimal.30 = 2 * sqrt(-1 * athlete.results$pmax.normalized * (fv.slope(hpo = player.data$push.off.distance,
                                                                                            push.off.angle = 30,
                                                                                            pmax = athlete.results$pmax.normalized)))
  athlete.results$f0.optimal.90 = 2 * sqrt(-1 * athlete.results$pmax.normalized * (fv.slope(hpo = player.data$push.off.distance,
                                                                                            push.off.angle = 90,
                                                                                            pmax = athlete.results$pmax.normalized)))

  athlete.results$v0.optimal.30 = 4 * athlete.results$pmax.normalized/ athlete.results$f0.optimal.30
  athlete.results$v0.optimal.90 = 4 * athlete.results$pmax.normalized/ athlete.results$f0.optimal.90

  athlete.results$perc.optimal.30 = athlete.results$sfv.normalized / fv.slope(hpo = player.data$push.off.distance,
                                                                              push.off.angle = 30,
                                                                              pmax = athlete.results$pmax.normalized) * 100
  athlete.results$perc.optimal.90 = athlete.results$sfv.normalized / fv.slope(hpo = player.data$push.off.distance,
                                                                              push.off.angle = 90,
                                                                              pmax = athlete.results$pmax.normalized) * 100

  athlete.results$training.focus.30 = ifelse(athlete.results$perc.optimal.30 < 0, "force", "velocity")
  athlete.results$training.focus.90 = ifelse(athlete.results$perc.optimal.90 < 0, "force", "velocity")

  athlete.results <- cbind(player.data, athlete.results)

  athlete.profile <- cbind(data.frame(player.name = player.name),
                           player.data)

  athlete.profile <- cbind(athlete.profile,

                           athlete.results[, c("f0", "f0.normalized", "f0.optimal.30", "f0.optimal.90",
                                               "perc.optimal.30", "perc.optimal.90",
                                               "pmax", "pmax.normalized",
                                               "r", "r2",
                                               "sfv", "sfv.normalized", "sfv.optimal",
                                               "training.focus.30", "training.focus.90",
                                               "v0", "v0.optimal.30", "v0.optimal.90")])

  athlete.profile

}


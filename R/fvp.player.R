#' FVP Player Profile
#'
#' This function returns a player's force-velocity-power profile
#'
#' This function returns an athlete's complete force-velocity-power profile which includes:
#' - The player's modeled max force and max normalized force
#' - The player's modeled slope
#' - The player's modeled max power and max normalized power
#' - Ratio of forces
#' - The player's modeled max velocity
#'
#' @note The \code{player.profile} argument takes on player profiles created using the \code{_.player.profile()} functions
#'
#' @param player.name The player's name
#' @param player.data The player's anthropometric data and testing day weather
#' @param player.profile The player's previously created profile
#'
#' @return
#' @export
#'
fvp.player.profile <- function(player.name = NA,
                               player.data,
                               player.profile) {

  fvp.data <- fvp.results.model(player.data = player.data,
                                player.profile = player.profile,
                                sprint.duration = 5)

  fv.lm <- lm(horizontal.net.force ~ speed, data = fvp.data)
  fv.norm.lm <- lm(horizontal.net.force.normalized ~ speed, data = fvp.data)
  rf.lm <- lm(ratio.of.forces ~ speed, data = fvp.data)


  athlete.results <- data.frame(f0 = as.numeric(coef(fv.lm)[1]),
                                fv.slope = as.numeric(coef(fv.norm.lm)[2]))

  athlete.results$f0.normalized = athlete.results$f0 / player.data$body.mass
  athlete.results$v0 = -1 * athlete.results$f0.normalized / athlete.results$fv.slope
  athlete.results$pmax = (athlete.results$f0 * athlete.results$v0)/ 4
  athlete.results$pmax.normalized = athlete.results$pmax / player.data$body.mass
  athlete.results$rf.max = max(fvp.data$ratio.of.forces, na.rm = TRUE)
  athlete.results$rf.d = as.numeric(coef(rf.lm)[2])

  athlete.info <- cbind(data.frame(player.name = player.name),
                        player.data)

  athlete.info <- cbind(athlete.info,
                        player.profile[, -1])

  athlete.results <- cbind(athlete.info,
                           athlete.results[, c("f0", "f0.normalized", "fv.slope",
                                               "pmax", "pmax.normalized",
                                               "rf.d", "rf.max",
                                               "v0")])

  athlete.results



}

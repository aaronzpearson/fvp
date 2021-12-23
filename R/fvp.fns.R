#' Force-Velocity-Power Functions
#'
#' These are helper functions that are used when modeling a player's force-velocity-power profile.
#'
#' @return Values used when modeling a player's force-velocity-power profile
#' @export
#'
fvp.fns <- function() {

  NULL

}

#' @describeIn fvp.fns Force of air against the player
#' @export
air.force <- function(air.friction.coefficient, player.speed) {

  air.f <- air.friction.coefficient * player.speed^2
  air.f

}

#' @describeIn fvp.fns Friction from air applied to the player
#' @export
air.friction <- function(player.height, player.mass) {

  af <- 0.266 * (0.2025 * player.height^0.75 * player.mass^0.425)
  af

}

#' @describeIn fvp.fns Air friction coefficient
#' @export
air.friction.coef <- function(player.data) {

  afc <- (air.rho(player.data$ambient.temp, player.data$air.pressure) *
            air.friction(player.data$player.height, player.data$body.mass) * 0.9) / 2
  afc

}

#' @describeIn fvp.fns Air functions RHO constant
#' @export
air.rho <- function(ambient.temp, air.pressure) {

  rho <- 1.293 * (air.pressure/ 760) * (273/ (273 + ambient.temp))
  rho

}

#' @describeIn fvp.fns Horizontal force applied by the athlete
#' @export
force.horizontal <- function(player.data, player.accel) {

  horiz.f <- player.data$body.mass * player.accel
  horiz.f

}

#' @describeIn fvp.fns Horizontal power produced by the athlete
#' @export
power.horizontal <- function(player.speed, horiz.net.force) {

  horiz.p <- player.speed * horiz.net.force

}

#' @describeIn fvp.fns Total force vector
#' @export
force.resultant <- function(horiz.force, body.mass) {

  resultant.f <- sqrt(horiz.force^2 + (body.mass * 9.81)^2)
  resultant.f

}

#' @describeIn fvp.fns Ratio of horizontal to total force
#' @export
ratio.of.force <- function(horiz.force, result.force) {

  ratio.of.forces <- (horiz.force/ result.force) * 100

}


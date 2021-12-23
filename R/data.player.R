#' Player Data
#'
#' This is a generic function that supports a player's anthropometric data and testing day weather. The data
#' in this data set can be subsequently utilized in the functions \code{fv.results.model} and \code{fvp.results.model}.
#'
#' @note To properly implement jump-based force-velocity and sprint-based force-velocity-power profiling models,
#' measurements must be in metric.
#'
#' @param player.height The player's height (meters)
#' @param body.mass The player's mass (kg)
#' @param lower.limb.length The player's lower-limb length (meters)
#' @param initial.height The player's height (meters)
#' @param push.off.angle The angle at which the player pushes off (degrees)
#' @param ambient.temp The air temperature at the time of testing (Celsius)
#' @param air.pressure The air pressure at the time of testing (mmHg)
#'
#' @return A data.frame that contains a player's anthropometric data and testing day weather
#'
#' @export
data.player <- function(player.height,
                        body.mass,
                        lower.limb.length,
                        initial.height,
                        push.off.angle = 90,
                        ambient.temp = 25,
                        air.pressure = 760) {

  player <- data.frame(player.height = player.height,
                       body.mass = body.mass,
                       lower.limb.length = lower.limb.length,
                       initial.height = initial.height,
                       push.off.angle = push.off.angle,
                       ambient.temp = ambient.temp,
                       air.pressure = air.pressure)

  player

}

#' @describeIn data.player A data.frame required for jump-based force-velocity analyses
#' @export
fv.data.player <- function(body.mass,
                           lower.limb.length,
                           initial.height,
                           push.off.angle = 90) {

  athlete <- data.frame(body.mass = body.mass,
                        lower.limb.length = lower.limb.length,
                        initial.height = initial.height,
                        push.off.distance = lower.limb.length - initial.height,
                        push.off.angle = push.off.angle)

}

#' @describeIn data.player Returns a data.frame required for sprint-based force-velocity-power analyses
#' @export
fvp.data.player <- function(player.height,
                            body.mass,
                            ambient.temp = 25,
                            air.pressure = 760) {

  athlete <- data.frame(player.height = player.height,
                        body.mass = body.mass,
                        ambient.temp = ambient.temp,
                        air.pressure = air.pressure)
  athlete

}

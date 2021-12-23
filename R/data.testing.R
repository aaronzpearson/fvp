#' Player Testing Data
#'
#' This is a generic function that builds a data.frame containing the data necessary for subsequent force-velocity
#' analyses. This function is to be used in conjunction with \code{_.data.testing()} when returning force-velocity
#' player profiles and force-velocity model results.
#'
#' @note The model requires at least two trials. A third trial is encouraged and should be input for the argument
#' \code{trial.three = NA}.
#' @note To properly implement force-velocity models, measurements must be in metric.
#
#'
#' @param condition The percentage of body weight
#' @param additional.mass Mass added to the player
#' @param trial.one A vector containing results for trial 1 of all conditions
#' @param trial.two A vector containing results for trial 2 of all conditions
#' @param trial.three A vector containing results for trial 3 of all conditions
#'
#' @return A data.frame containing pertinent information for further force-velocity analyses
#'
#'
#' @export
data.testing <- function(condition = percent.bw,
                         additional.mass = add.mass,
                         trial.one,
                         trial.two,
                         trial.three = NA) {

  athlete <- data.frame(condition = percent.bw,
                        additional.mass = add.mass,
                        trial.one = trial.one,
                        trial.two = trial.two,
                        trial.three = trial.three)

  athlete$hmax = apply(athlete[, -1:-2], 1, max, na.rm = TRUE)
  athlete$total.mass = player.data$body.mass + athlete$additional.mass
  athlete$force = athlete$total.mass * 9.81 * (1 + (athlete$hmax/player.data$push.off.distance) )
  athlete$vel = sqrt((9.81* athlete$hmax)/2)
  athlete$power = athlete$force * athlete$vel

  return(athlete)

}

#' @describeIn data.testing A data.frame for subsequent jump-based force-velocity models
#' @export
fv.data.testing <- function(player.data,
                            percent.bw,
                            add.mass,
                            trial.one,
                            trial.two,
                            trial.three = NA) {

  athlete <- data.frame(condition = percent.bw,
                        additional.mass = add.mass,
                        trial.one = trial.one,
                        trial.two = trial.two,
                        trial.three = trial.three)

  athlete$hmax = apply(athlete[, -1:-2], 1, max, na.rm = TRUE)
  athlete$total.mass = player.data$body.mass + athlete$additional.mass
  athlete$force = athlete$total.mass * 9.81 * (1 + (athlete$hmax/player.data$push.off.distance) )
  athlete$vel = sqrt((9.81* athlete$hmax)/2)
  athlete$power = athlete$force * athlete$vel

  return(athlete)

}

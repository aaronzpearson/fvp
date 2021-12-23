#' Force-Velocity Profiling Functions
#'
#' These functions are helper functions when building a player's force-velocity profile. Many of the values
#' returned from these functions are returned when using the \code{fv.player.profile()} function.
#'
#' @return Modeled force-velocity max force
#' @export
fv.f0 <- function(player.data, player.profile, push.off.angle = 90) {

  optim.slope <- fv.slope(hpo = player.data$push.off.distance,
                          push.off.angle = push.off.angle,
                          pmax = player.profile$pmax.normalized)

  f0.optim = 2 * sqrt(-1 * player.profile$pmax.normalized * optim.slope)
  f0.optim

}


#' @describeIn fv.f0 The suggested training focus
#' @export
fv.focus <- function(player.data, player.profile, push.off.angle = 90, return.focus = FALSE) {

  optim.slope.alpha = fv.slope(hpo = player.data$push.off.distance,
                               push.off.angle = push.off.angle,
                               pmax = player.profile$pmax.normalized)

  fv.alpha = 100 * (player.profile$sfv.normalized / optim.slope.alpha)

  training.focus <- ifelse(fv.alpha < 0, "force",
                           "velocity")

  if(return.focus == FALSE) {

    fv.alpha

  } else {

    data.frame(fv.alpha = fv.alpha,
               training.focus = training.focus)

  }

}

#' @describeIn fv.f0 Slope from the resulting force-velocity model
#' @export
fv.slope = function(hpo, push.off.angle, pmax) {

  alpha = 9.81 * sin((push.off.angle * pi)/180)
  x = fv.x(hpo, push.off.angle, pmax)

  optim.slope = -(alpha^2/ (3 * pmax)) -
    ((-alpha^4 * hpo^4 -
        12 * alpha * hpo^3 * pmax^2) /
       (3 * hpo^2 * pmax * x)) +
    x/ (3 * hpo^2 * pmax)

  return(optim.slope)

}

#' @describeIn fv.f0 Modeled force-velocity max velocity
#' @export
fv.v0 <- function(player.data, player.profile, push.off.angle = 90) {

  f0.optim <- fv.f0(player.data, player.profile, push.off.angle)

  v0.optim = 4 * player.profile$pmax.normalized/ f0.optim
  v0.optim

}

#' @describeIn fv.f0 Modeled x-intercept helper function
#' @export
fv.x <- function(hpo, push.off.angle, pmax) {

  alpha = 9.81 * sin((push.off.angle * pi)/180)

  x = (-(alpha^6) * hpo^6 -
         18 * alpha^3 * hpo^5 * pmax^2 -
         54 * hpo^4 * pmax^4 +
         6 * sqrt(3) * sqrt(2 * alpha^3 * hpo^9 * pmax^6 + 27 * hpo^8 * pmax^8))
  x = -abs(x)^(1/3)

  return(x)

}

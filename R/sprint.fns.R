#' Mechanical Sprint Abilities
#'
#' A set of functions that return speed, acceleration, distance, and time values as functions of each other. These are
#' helper function when building \code{_.results._()} data.frames.
#'
#' Thank you to Dani Chu for his contributions to many of these functions.
#'
#'
#' @return Speed, acceleration, distance, or time values corresponding to an athlete's sprint abilities
#' @export
#'
sprint.fns <- function() {

  NULL

}


#' @rdname sprint.fns
#' @export
accel.time <- function(player.profile, split.time) {

  accel.dur <- (player.profile$max.speed/ player.profile$player.tau) *
    exp(-split.time/ player.profile$player.tau)

  accel.dur

}


#' @rdname sprint.fns
#' @export
distance.speed <- function(player.profile, current.speed) {


  split.time <- time.speed(player.profile, current.speed)

  position.vel <- player.profile$max.speed *
    (split.time + player.profile$player.tau * exp(-split.time/player.profile$player.tau)) -
    (player.profile$max.speed * player.profile$player.tau)

  position.vel

}


#' @rdname sprint.fns
#' @export
distance.time <- function(player.profile, split.time) {

  pos.dur <- player.profile$max.speed *
    (split.time + player.profile$player.tau * exp(-split.time/ player.profile$player.tau)) -
    player.profile$max.speed * player.profile$player.tau

  pos.dur

}


#' @rdname sprint.fns
#' @export
midsprint <- function(player.profile, current.speed, distance) {

  dur.to.position <- time.to.dist.given.speed(player.profile, current.speed, distance)

  dur.to.position

}


#' @rdname sprint.fns
#' @export
speed.time <- function(player.profile, split.time) {

  vel.dur <- player.profile$max.speed *
    (1 - exp(-split.time/ player.profile$player.tau))

  vel.dur

}


#' @rdname sprint.fns
#' @export
time.distance <- function(player.profile, distance) {

  dur.position <- player.profile$player.tau *
    lamW::lambertW0(-exp(-1 - distance/ (player.profile$player.tau * player.profile$max.speed))) +
    distance/player.profile$max.speed +
    player.profile$player.tau

  dur.position

}


#' @rdname sprint.fns
#' @export
time.speed <- function(player.profile, current.speed) {

  dur.vel <- -log(-(current.speed/ player.profile$max.speed -1)) *
    player.profile$player.tau

  dur.vel

}


#' @rdname sprint.fns
#' @export
time.to.dist.given.speed <- function(player.profile, current.speed, distance) {

  position.0 <- distance.speed(player.profile, current.speed)

  dur.to.pos.given.vel <- time.distance(player.profile, distance + position.0) -
    time.distance(player.profile, position.0)

  dur.to.pos.given.vel

}



#' @rdname sprint.fns
#' @export
time.to.position <- function(player.profile, current.speed, distance) {

  dur.to.position <- time.to.dist.given.speed(player.profile, current.speed, distance)

  dur.to.position

}



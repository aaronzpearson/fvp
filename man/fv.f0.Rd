% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fv.fns.R
\name{fv.f0}
\alias{fv.f0}
\alias{fv.focus}
\alias{fv.slope}
\alias{fv.v0}
\alias{fv.x}
\title{Force-Velocity Profiling Functions}
\usage{
fv.f0(player.data, player.profile, push.off.angle = 90)

fv.focus(
  player.data,
  player.profile,
  push.off.angle = 90,
  return.focus = FALSE
)

fv.slope(hpo, push.off.angle, pmax)

fv.v0(player.data, player.profile, push.off.angle = 90)

fv.x(hpo, push.off.angle, pmax)
}
\value{
Modeled force-velocity max force
}
\description{
These functions are helper functions when building a player's force-velocity profile. Many of the values
returned from these functions are returned when using the \code{fv.player.profile()} function.
}
\section{Functions}{
\itemize{
\item \code{fv.focus}: The suggested training focus

\item \code{fv.slope}: Slope from the resulting force-velocity model

\item \code{fv.v0}: Modeled force-velocity max velocity

\item \code{fv.x}: Modeled x-intercept helper function
}}


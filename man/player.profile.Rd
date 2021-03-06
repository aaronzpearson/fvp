% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/player.profile.R
\name{player.profile}
\alias{player.profile}
\alias{build.player.profile}
\alias{gps.player.profile}
\alias{gps.player.profile.game}
\alias{sa.player.profile}
\alias{scout.player.profile}
\title{Player Profiling}
\usage{
player.profile(player.name = NA, game.data, r2 = 0.95)

build.player.profile(player.name = NA, max.speed, max.accel)

gps.player.profile(player.name = NA, game.data)

gps.player.profile.game(
  player.name = NA,
  game.data,
  min.speed = 0.3,
  max.speed.threshold = 95,
  sample.rate = 10
)

sa.player.profile(player.name = NA, game.data, r2 = 0.95)

scout.player.profile(player.name = NA, sprint.data)
}
\arguments{
\item{player.name}{The player's name}

\item{game.data}{The player's cleaned game data}

\item{r2}{The minimum r^2 when fitting the model}
}
\value{
An athlete's player profile which includes the player's name, their maximal attainable speed,
maximal attainable acceleration, and their acceleration constant.
}
\description{
This is a generic function that builds player profiles from game data much like the functions \code{gps.player.profile()},
\code{gps.game.profile()}, \code{sa.player.profile()}, and \code{scout.player.profile}. Depending on the function used,
player profiles will return either observed or modeled values.
}
\details{
The intention of \code{player.profile()}, and similar functions, is to provide sports practitioners with
pertinent information on the given athlete's mechanical sprint abilities. Practitioners can apply this
information in \code{_.results.model()} functions to build data sets that represent the athlete's
mechanical speeds, accelerations, and distances covered as a function of time.

It is suggested that the \code{player.name} argument be filled to be effectively applied to
functions like \code{compare.player.profiles()}.

Thank you to Patrick Ward for the first iteration of \code{scout.player.profile()}.
}
\section{Functions}{
\itemize{
\item \code{build.player.profile}: Manually create a player's profile

\item \code{gps.player.profile}: GPS player profile from a player's potential abilities

\item \code{gps.player.profile.game}: GPS player profile from a player's observed abilities

\item \code{sa.player.profile}: Speed-acceleration player profile from a player's GPS data

\item \code{scout.player.profile}: Sprint test player profile from a player's split times and distances
}}

\note{
The functions \code{sa.player.profile()} and \code{scout.player.profile()} must be called explicitly because their
helper functions differ significantly.

The functions \code{fv.player.profile()} and \code{fvp.player.profile()} are unique functions and are not assessed
in the same manner. Therefore, the generic function is not applicable.

The input game data is assumed to be in metric. Speed must be in m/s and acceleration in m/s/s. If it is not,
values that are returned are not representative of a player's true sprint potential. If your data needs to be converted,
use the \code{convert.to.metric()} function. See the \code{convert.to.metric()} documentation for examples on automating
the process.
}

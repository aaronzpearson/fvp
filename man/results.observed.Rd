% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.observed.R
\name{results.observed}
\alias{results.observed}
\alias{sa.results.observed}
\alias{gps.results.observed}
\alias{fv.results.observed}
\title{Observed Results}
\usage{
results.observed()

sa.results.observed(game.data)

gps.results.observed(
  game.data,
  min.speed = 0.3,
  max.speed.threshold = 95,
  sample.rate = 10
)

fv.results.observed(player.data, testing.data)
}
\value{
The observations in the data set that are used to model their respective outcomes
}
\description{
The \code{_.results.observed()} functions return observations that are found in the original game.data data set.
These are data that were used in subsequent analyses within their respective family of functions.
}
\details{
\code{sa.results.observed()} returns the observations that were maintained within the data set that fit
inclusion criteria to build the model. The speed-acceleration model performs five tasks: 1. speed values below
3 m/s are removed from the data set, 2. speed bins are created every 0.2 m/s, 3. the player's acceleration is
grouped by speed bin, 4. the player's acceleration is ordered in descending fashion, 5. the greatest two
acceleration rates are kept in the data set and are returned to the user. These data are then applied in
the \code{sa.results.fitted()} function to return the final model observations that are required to build the
speed-acceleration plot.

\code{gps.results.observed{}} returns the observations that make-up the player's best sprint.

\code{fv.results.observed()} returns the observations that build the force-velocity model.

The \code{_.results.game()}, \code{_.results.fitted()}, and \code{_.results.observed()} return similar data.frames but
behave differently on the back-end. Therefore, it is suggested that these functions be used for their intended purposes.
}
\section{Functions}{
\itemize{
\item \code{sa.results.observed}: Returns observations that fit speed-acceleration model inclusion criteria

\item \code{gps.results.observed}: Returns observations of a player's best in-game sprint

\item \code{fv.results.observed}: Returns force-velocity observations
}}


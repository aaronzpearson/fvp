#' Convert Metric
#'
#' The functions in this package are optimized when values are in metric. This function provides an easy way
#' of converting values to m/s or m/s/s.
#'
#' @param value A number
#' @param units The units corresponding to the number
#'
#' @return
#' @export
convert.to.metric <- function(value, units = "m/s") {

  # converts values to metric
  # in m/s

  # functions create objects (to_metric & from_metric)
  # from_metric = 1/to_metric
  distance <- convert.to.meters(value, strsplit(units, "/")[[1]][1])
  duration <- convert.to.seconds(strsplit(units, "/")[[1]][2])

  distance/ duration

}

#' @describeIn convert.to.metric Conversion to meters
#' @export
convert.to.meters <- function(value, units = c("ft", "km", "m", "mi", "yd")) {

  # converts ft, km, mi, yd to meters
  # provides some leeway on different spelling of each input

  if(units %in% c("meters", "m", "meter")) {
    return(value)

  } else if(units %in% c("feet", "ft", "fts", "f")) {
    return(value * 0.3048)

  } else if(units %in% c("kilometers", "kilometer", "k", "km")) {
    return(value * 1000)

  } else if(units %in% c("miles", "mile", "mi")) {
    return(value * 1609.344)

  } else if(units %in% c("yard", "yards", "yd", "yds")) {
    return(value * 0.9144)

  }

}

#' @describeIn convert.to.metric Conversion to seconds
#' @export
convert.to.seconds <- function(units = c("second, minute, hour")) {

  # converts min and hours to seconds

  if(units %in% c("seconds", "second", "s", "sec")) {
    1

  } else if(units %in% c("min", "m", "minute", "minutes")) {
    1*60

  } else if(units %in% c("hour", "h", "hr", "hours")) {
    1*60*60

  }

}


CurrentYear <- function() {
  if (as.numeric(format(Sys.Date(), "%m")) > 9) {
    return(as.numeric(format(Sys.Date(), "%Y")) + 1)
  } else {
    return(as.numeric(format(Sys.Date(), "%Y")))
  }
}

YearToSeason <- function(x) {
  if (is.numeric(x)) {
    paste0(x - 1, '-', substring(x, 3, 4))
  } else {
    return(x)
  }
}

SeasonToYear <- function(x) {
  as.numeric(substring(x, 1, 4)) + 1
}

TimeToSeconds <- function(x) {
  return(sapply(strsplit(x, ":"), function(x) as.numeric(x[1]) * 60 + as.numeric(x[2])))
}

CurrentYear <- function() {
  if (as.numeric(format(Sys.Date(), "%m")) > 9) {
    return(as.numeric(format(Sys.Date(), "%Y")) + 1)
  } else {
    return(as.numeric(format(Sys.Date(), "%Y")))
  }
}

YearToSeason <- function(x) {
  paste0(x - 1, '-', substring(x, 3, 4))
}

SeasonToYear <- function(x) {
  as.numeric(substring(x, 1, 4)) + 1
}
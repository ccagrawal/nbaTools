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

kToday <- Sys.Date()
kYear <- CurrentYear()
kSeason <- YearToSeason(kYear)

NameConvert <- function(name) {
  name <- gsub(' Jr\\.', '', name)
  name <- gsub(' III', '', name)
  name <- sapply(name, function(x) paste0(substr(x, 1, 1), '.', substr(x, tail(gregexpr(' ', x)[[1]], 1), nchar(x))))
  return(as.character(name))
}

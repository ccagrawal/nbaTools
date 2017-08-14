#' Game Schedule
#'
#' @return data frame with list of games on a certain day
#' @keywords games
#' @export
#' @examples
#' GetGames(Date = '2017-02-02')

GetGames <- function(...) {

  endpoint <- 'scoreboardV2'
  referer <- 'scores'
  ix <- 1

  param.keys <- c('DayOffset', 'LeagueID', 'gameDate')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

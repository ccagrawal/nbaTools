#' Player list
#'
#' @return data frame with all players
#' @keywords player
#' @export
#' @examples
#' # GetPlayerIDs()

GetPlayerIDs <- function(...) {

  endpoint <- 'commonallplayers'
  referer <- 'player'
  ix <- 1

  param.keys <- c('IsOnlyCurrentSeason', 'LeagueID', 'Season')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Player game logs
#'
#' @return data frame with all games for a player
#' @keywords player game log
#' @export
#' @examples
#' # GetPlayerGameLogs()

GetPlayerGameLogs <- function(...) {

  endpoint <- 'playergamelog'
  referer <- 'player'
  ix <- 1

  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID', 'Location', 'MeasureType',
                  'Month', 'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerID', 'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'VsConference', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

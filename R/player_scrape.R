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

  endpoint <- 'playergamelogs'
  referer <- 'player'
  ix <- 1

  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID', 'Location', 'MeasureType',
                  'Month', 'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerID', 'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'VsConference', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Player year over year stats
#'
#' @return data frame with players yearly stats
#' @keywords player game log
#' @export
#' @examples
#' # GetPlayerYearByYearStats(PlayerID = '2544')

GetPlayerYearByYearStats <- function(split = 'Advanced', ...) {

  endpoint <- 'playerdashboardbyyearoveryear'
  referer <- 'player'

  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID', 'Location', 'MeasureType',
                  'Month', 'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerID', 'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'Split', 'VsConference', 'VsDivision')

  if (split == 'Traditional') {
    ix <- 1
  } else if (split == 'Advanced') {
    ix <- 2
  } else if (split == 'Misc') {
    ix <- 3
  } else if (split == 'Scoring') {
    ix <- 4
  } else if (split == 'Usage') {
    ix <- 5
  }

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

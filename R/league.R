#' Team Stats
#'
#' @return data frame with stats for all teams
#' @keywords team
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamStats(SeasonType = 'Playoffs')

GetTeamStats <- function(...) {

  endpoint <- 'leaguedashteamstats'
  referer <- 'teams/traditional'
  ix <- 1

  param.keys <- c('Conference', 'DateFrom', 'DateTo', 'Division', 'GameScope',
                  'GameSegment', 'LastNGames', 'LeagueID', 'Location',
                  'MeasureType', 'Month', 'OpponentTeamID', 'Outcome', 'PORound',
                  'PaceAdjust', 'PerMode', 'Period', 'PlayerExperience',
                  'PlayerPosition', 'PlusMinus', 'Rank', 'Season', 'SeasonSegment',
                  'SeasonType', 'ShotClockRange', 'StarterBench', 'TeamID',
                  'VsConference', 'VsDivision')

  return(GetNBAData(endpoint, referer, ix, param.keys, ...))
}

#' Player Stats
#'
#' @return data frame with stats for all players
#' @keywords player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerStats(SeasonType = 'Playoffs')

GetPlayerStats <- function(...) {

  endpoint <- 'leaguedashplayerstats'
  referer <- 'players/traditional'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'GameScope', 'GameSegment', 'Height', 'LastNGames',
                  'LeagueID', 'Location', 'MeasureType', 'Month', 'OpponentTeamID',
                  'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerExperience', 'PlayerPosition', 'PlusMinus', 'Rank', 'Season',
                  'SeasonSegment', 'SeasonType', 'ShotClockRange', 'StarterBench',
                  'TeamID', 'VsConference', 'VsDivision', 'Weight')

  return(GetNBAData(endpoint, referer, ix, param.keys, ...))
}


#' Tracking Stats
#'
#' @return data frame with tracking stats for all players or all teams
#' @keywords player team tracking
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTrackingStats(PlayerOrTeam = 'Team')

GetTrackingStats <- function(...) {

  endpoint <- 'leaguedashptstats'
  referer <- 'players/catch-shoot/'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'GameScope', 'Height', 'LastNGames',
                  'LeagueID', 'Location', 'Month', 'OpponentTeamID',
                  'Outcome', 'PORound', 'PerMode', 'Period', 'PlayerExperience',
                  'PlayerOrTeam', 'PlayerPosition', 'PtMeasureType', 'Season',
                  'SeasonSegment', 'SeasonType', 'StarterBench',
                  'TeamID', 'VsConference', 'VsDivision', 'Weight')

  return(GetNBAData(endpoint, referer, ix, param.keys, ...))
}

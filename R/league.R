#' Team Stats.
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

#' Player Stats.
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
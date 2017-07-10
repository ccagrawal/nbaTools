#' Team Stats
#'
#' @return data frame with stats for all teams
#' @keywords team
#' @export
#' @examples
#' GetTeamStats(SeasonType = 'Playoffs')

GetTeamStats <- function(source = 'NBA', ...) {

  if (source == 'NBA') {
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

  } else if (source == 'BRef') {
    endpoint <- 'leagues/NBA_Season.html'
    referer <- NA
    ix <- 1

    param.keys <- c('Season')
  }

  return(GetData(endpoint, referer, ix, param.keys, source, ...))
}

#' Player Stats
#'
#' @return data frame with stats for all players
#' @keywords player
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

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}


#' Tracking Stats
#'
#' @return data frame with tracking stats for all players or all teams
#' @keywords player team tracking
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

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Defense Stats
#'
#' @return data frame with defensive stats for all players
#' @keywords player
#' @export
#' @examples
#' GetPlayerDefenseStats(SeasonType = 'Playoffs')

GetPlayerDefenseStats <- function(...) {

  endpoint <- 'leaguedashptdefend'
  referer <- 'players/defense-dash-3pt'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo',
                  'DefenseCategory', 'Division', 'DraftPick', 'DraftYear', 'GameSegment',
                  'Height', 'LastNGames', 'LeagueID', 'Location', 'Month', 'OpponentTeamID',
                  'Outcome', 'PORound', 'PerMode', 'Period', 'PlayerExperience',
                  'PlayerPosition', 'Season', 'SeasonSegment', 'SeasonType', 'StarterBench',
                  'TeamID', 'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

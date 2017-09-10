#' Team Stats
#'
#' @return data frame with stats for all teams
#' @keywords team
#' @export
#' @examples
#' # GetTeamStats(SeasonType = 'Playoffs')

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
#' # GetPlayerStats(SeasonType = 'Playoffs')

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

#' Player Clutch Stats
#'
#' @return data frame with stats for all players in the clutch
#' @keywords player clutch
#' @export
#' @examples
#' # GetPlayerClutchStats(SeasonType = 'Playoffs')

GetPlayerClutchStats <- function(...) {

  endpoint <- 'leaguedashplayerclutch'
  referer <- 'players/clutch-traditional'
  ix <- 1

  param.keys <- c('AheadBehind', 'ClutchTime', 'College', 'Conference', 'Country',
                  'DateFrom', 'DateTo', 'Division', 'DraftPick', 'DraftYear',
                  'GameScope', 'GameSegment', 'Height', 'LastNGames',
                  'LeagueID', 'Location', 'MeasureType', 'Month', 'OpponentTeamID',
                  'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerExperience', 'PlayerPosition', 'PlusMinus', 'PointDiff',
                  'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'StarterBench', 'TeamID', 'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Lineup Stats
#'
#' @return data frame with stats for all lineups
#' @keywords lineup
#' @export
#' @examples
#' # GetLineupStats(SeasonType = 'Playoffs')

GetLineupStats <- function(...) {

  endpoint <- 'leaguedashlineups'
  referer <- 'lineups/advanced'
  ix <- 1

  param.keys <- c('Conference', 'DateFrom', 'DateTo', 'Division', 'GameID',
                  'GameSegment', 'GroupQuantity', 'LastNGames', 'LeagueID',
                  'Location', 'MeasureType', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'Period', 'PlusMinus',
                  'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'TeamID', 'VsConference', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Tracking Stats
#'
#' @return data frame with tracking stats for all players or all teams
#' @keywords player team tracking
#' @export
#' @examples
#' # GetTrackingStats(PlayerOrTeam = 'Team')

GetTrackingStats <- function(...) {

  endpoint <- 'leaguedashptstats'
  referer <- 'players/catch-shoot'
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

#' Play Type Stats
#'
#' @return data frame with play type stats for all players or all teams
#' @keywords player team play type
#' @export
#' @examples
#' # GetPlayTypeStats(category = 'Isolation')

GetPlayTypeStats <- function(type = 'player', ...) {

  if (type == 'team') {
    endpoint <- 'team/'
    referer <- 'teams/transition'
  } else {
    endpoint <- 'player/'
    referer <- 'players/transition'
  }

  ix <- 1
  param.keys <- c('category', 'limit', 'names', 'q', 'season', 'seasonType')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA.Synergy', ...))
}

#' Defense Stats
#'
#' @return data frame with defensive stats for all players
#' @keywords player
#' @export
#' @examples
#' # GetPlayerDefenseStats(SeasonType = 'Playoffs')

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

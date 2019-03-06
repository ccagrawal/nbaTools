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

#' Team Hustle Stats
#'
#' @return data frame with hustle stats for all teams
#' @keywords team hustle
#' @export
#' @examples
#' # GetTeamHustleStats(SeasonType = 'Playoffs')

GetTeamHustleStats <- function(...) {

  endpoint <- 'leaguehustlestatsteams'
  referer <- 'teams/hustle'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'DraftYear', 'GameScope',
                  'GameSegment', 'Height', 'LastNGames', 'LeagueID', 'Location',
                  'Month', 'OpponentTeamID', 'Outcome', 'PORound',
                  'PaceAdjust', 'PerMode', 'PlayerExperience',
                  'PlayerPosition', 'PlusMinus', 'Rank', 'Season', 'SeasonSegment',
                  'SeasonType', 'ShotClockRange', 'TeamID',
                  'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Player Stats
#'
#' @return data frame with stats for all players
#' @keywords player
#' @export
#' @examples
#' # GetPlayerStats(SeasonType = 'Playoffs')

GetPlayerStats <- function(source = 'NBA', ...) {

  if (source == 'NBA') {
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

  } else if (source == 'BRef') {
    endpoint <- 'leagues/NBA_<Season>_<MeasureType>.html'
    referer <- ''
    ix <- 1

    param.keys <- c('Season', 'MeasureType')
  }

  return(GetData(endpoint, referer, ix, param.keys, source, ...))
}

#' Player Bio Stats
#'
#' @return data frame with bio stats for all players
#' @keywords player bio
#' @export
#' @examples
#' # GetPlayerBioStats(SeasonType = 'Playoffs')

GetPlayerBioStats <- function(...) {

  endpoint <- 'leaguedashplayerbiostats'
  referer <- 'players/bio'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'DraftYear', 'GameScope', 'GameSegment', 'Height', 'LastNGames',
                  'LeagueID', 'Location', 'Month', 'OpponentTeamID', 'Outcome', 'PORound',
                  'PerMode', 'Period', 'PlayerExperience', 'PlayerPosition', 'Season',
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

#' Player shooting
#'
#' @return data frame with shooting stats for all players
#' @keywords player shooting
#' @export
#' @examples
#' # GetPlayerShootingStats(SeasonType = 'Playoffs')

GetPlayerShootingStats <- function(...) {

  endpoint <- 'leaguedashplayerptshot'
  referer <- 'players/shots-touch-time'
  ix <- 1

  param.keys <- c('CloseDefDistRange', 'College', 'Conference', 'Country', 'DateFrom', 'DateTo',
                  'Division', 'DraftPick', 'DraftYear', 'DribbleRange', 'GameScope', 'GameSegment',
                  'GeneralRange', 'Height', 'LastNGames', 'LeagueID', 'Location', 'Month',
                  'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerExperience', 'PlayerPosition', 'PlusMinus', 'Rank', 'Season',
                  'SeasonSegment', 'SeasonType', 'ShotClockRange', 'ShotDistRange', 'StarterBench',
                  'TeamID', 'TouchTimeRange', 'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Team shooting
#'
#' @return data frame with shooting stats for all teams
#' @keywords team shooting
#' @export
#' @examples
#' # GetTeamShootingStats(SeasonType = 'Playoffs')

GetTeamShootingStats <- function(...) {

  endpoint <- 'leaguedashteamptshot'
  referer <- 'teams/shots-closest-defender'
  ix <- 1

  param.keys <- c('CloseDefDistRange', 'College', 'Conference', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'DraftYear', 'DribbleRange', 'GameScope', 'GameSegment', 'GeneralRange',
                  'Height', 'LastNGames', 'LeagueID', 'Location', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'Period', 'PlayerExperience', 'PlayerPosition',
                  'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange', 'ShotDistRange',
                  'StarterBench', 'TeamID', 'TouchTimeRange', 'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Team shot location
#'
#' @return data frame with shot location stats for all teams
#' @keywords team shot location
#' @export
#' @examples
#' # GetTeamShotLocationStats(SeasonType = 'Playoffs')

GetTeamShotLocationStats <- function(...) {

  endpoint <- 'leaguedashteamshotlocations'
  referer <- 'teams/shooting'
  ix <- 1

  param.keys <- c('Conference', 'DateFrom', 'DateTo', 'DistanceRange', 'Division', 'GameScope',
                  'GameSegment', 'LastNGames', 'LeagueID', 'Location', 'MeasureType', 'Month',
                  'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust', 'PerMode', 'Period',
                  'PlayerExperience', 'PlayerPosition', 'PlusMinus', 'Rank', 'Season',
                  'SeasonSegment', 'SeasonType', 'ShotClockRange', 'StarterBench',
                  'TeamID', 'VsConference', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Player Hustle Stats
#'
#' @return data frame with hustle stats for all players
#' @keywords player hustle
#' @export
#' @examples
#' # GetPlayerHustleStats(SeasonType = 'Playoffs')

GetPlayerHustleStats <- function(...) {

  endpoint <- 'leaguehustlestatsplayer'
  referer <- 'players/hustle'
  ix <- 1

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'Division',
                  'DraftPick', 'DraftYear', 'GameScope', 'Height', 'LastNGames',
                  'LeagueID', 'Location', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'PlayerExperience', 'PlayerPosition',
                  'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType',
                  'TeamID', 'VsConference', 'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Player Shot Location Stats
#'
#' @return data frame with shot location stats for all players
#' @keywords player shot location
#' @export
#' @examples
#' # GetPlayerShotLocationStats(SeasonType = 'Playoffs')

GetPlayerShotLocationStats <- function(...) {

  endpoint <- 'leaguedashplayershotlocations'
  referer <- 'players/shooting'
  ix <- NULL

  param.keys <- c('College', 'Conference', 'Country', 'DateFrom', 'DateTo', 'DistanceRange',
                  'Division', 'DraftPick', 'DraftYear', 'GameScope', 'GameSegment',
                  'Height', 'LastNGames', 'LeagueID', 'Location', 'MeasureType',
                  'Month', 'OpponentTeamID', 'Outcome', 'PORound', 'PaceAdjust',
                  'PerMode', 'Period', 'PlayerExperience', 'PlayerPosition',
                  'PlusMinus', 'Rank', 'Season', 'SeasonSegment', 'SeasonType',
                  'ShotClockRange', 'StarterBench', 'TeamID', 'VsConference',
                  'VsDivision', 'Weight')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' Game Logs
#'
#' @return data frame with game logs for all players
#' @keywords player gamelog
#' @export
#' @examples
#' # GetGameLogs(SeasonType = 'Playoffs')

GetGameLogs <- function(...) {

  endpoint <- 'playergamelogs'
  referer <- 'players/boxscores-traditional'
  ix <- 1

  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID',
                  'Location', 'MeasureType', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'Period', 'PlusMinus', 'Rank',
                  'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'VsConference', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

#' RPM
#'
#' @return data frame with player RPMs
#' @keywords player rpm
#' @export
#' @examples
#' # GetRPM(year = 2017)

GetRPM <- function(year = kYear) {

  base.url <- paste0('http://espn.go.com/nba/statistics/rpm/_/year/', year, '/page/PPPP/sort/RPM')

  continue <- TRUE
  i <- 1
  df <- data.frame()

  # Loop through pages until we get to an empty page
  while(continue) {
    url <- gsub('PPPP', i, base.url)
    table <- readHTMLTable(url)[[1]]

    if (is.null(table)) {
      continue <- FALSE
    } else {
      df <- rbind(df, table)
      i <- i + 1
    }
  }

  # Split up Name into Name and Position
  df$POS <- gsub('.*, (.*)', '\\1', df$NAME)
  df$NAME <- gsub('(.*),.*', '\\1', df$NAME)
  df <- df[, c(2, 3, 10, 4:9)]

  # Fix the column types
  df[, -c(1:3)] <- lapply(df[, -c(1:3)], as.numeric)

  return(df)
}

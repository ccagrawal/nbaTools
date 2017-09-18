#' Team game log
#'
#' @return data frame with game box scores for a team
#' @keywords player
#' @export
#' @examples
#' # GetTeamGameLog(SeasonType = 'Playoffs', 'TeamID' = '1610612745')

GetTeamGameLog <- function(...) {

  endpoint <- 'teamgamelogs'
  referer <- 'team'
  ix <- 1

  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID',
                  'Location', 'MeasureType', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'Period', 'PlusMinus',
                  'Rank', 'Season', 'SeasonSegment', 'SeasonType', 'ShotClockRange',
                  'TeamID', 'VsConfererence', 'VsDivision')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

GetTeamDashboard <- function(source = 'NBA', endpoint = '', ix = 1, ...) {

  if (source == 'NBA') {
    referer <- 'team/'

    param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID',
                    'Location', 'MeasureType', 'Month', 'OpponentTeamID', 'Outcome',
                    'PaceAdjust', 'PerMode', 'Period', 'PlusMinus', 'Rank', 'Season',
                    'SeasonSegment', 'SeasonType', 'TeamID', 'VsConference', 'VsDivision')
  }

  return(GetData(endpoint, referer, ix, param.keys, source, ...))
}

#' Team On Off Details
#'
#' @return data frame with on-off stats for players on a team
#' @keywords team on-off
#' @export
#' @examples
#' # GetTeamPlayerOnOffDetails(TeamID = '1610612756')

GetTeamPlayerOnOffDetails <- function(...) {
  endpoint <- 'teamplayeronoffdetails'

  on <- GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = 2, ...)
  off <- GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = 3, ...)

  return(rbind(on, off))
}

#' Team Dashboard by General Splits
#'
#' @return data frame with team information by a variety of splits
#' @keywords team dashboard splits
#' @export
#' @examples
#' # GetTeamGeneralSplits(split = 'location', TeamID = '1610612756')

GetTeamGeneralSplits <- function(split = 'location', ...) {
  endpoint <- 'teamdashboardbygeneralsplits'

  if (split == 'location') {
    ix <- 2
  } else if (split == 'wins/losses') {
    ix <- 3
  } else if (split == 'monthly') {
    ix <- 4
  } else if (split == 'pre/post all-star') {
    ix <- 5
  } else if (split == 'days rest') {
    ix <- 6
  }

  return(GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = ix, ...))
}

#' Team Shot Dashboard
#'
#' @return data frame with team information by a variety of splits
#' @keywords team shot dashboard
#' @export
#' @examples
#' # GetTeamShotDashboard(split = 'shot clock', TeamID = '1610612756')

GetTeamShotDashboard <- function(split = 'shot clock', ...) {
  endpoint <- 'teamdashptshots'

  if (split == 'shot clock') {
    ix <- 2
  } else if (split == 'dribble') {
    ix <- 3
  } else if (split == 'closest defender') {
    ix <- 4
  } else if (split == 'closest defender long distance') {
    ix <- 5
  } else if (split == 'touch time') {
    ix <- 6
  } else {
    return(NA)
  }

  return(GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = ix, ...))
}

#' Get Team Logo
#'
#' @param team.id Team ID from ESPN (e.g. 'hou')
#' @return team's logo
#' @keywords picture team logo
#' @importFrom utils download.file
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @export
#' @examples
#' # GetTeamLogo('hou')

GetTeamLogo <- function(team.id) {

  url <- gsub('###', team.id, 'http://a.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/###.png')
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  pic <- readPNG(temp)
  file.remove(temp)

  return(rasterGrob(pic, interpolate = TRUE))
}

#' Team player stats
#'
#' @return data frame with stats for players on a team
#' @keywords player team
#' @export
#' @examples
#' # GetTeamPlayerDashboard(SeasonType = 'Playoffs', 'TeamID' = '1610612745')

GetTeamPlayerDashboard <- function(...) {
  
  endpoint <- 'teamplayerdashboard'
  referer <- 'team'
  ix <- 1
  
  param.keys <- c('DateFrom', 'DateTo', 'GameSegment', 'LastNGames', 'LeagueID',
                  'Location', 'MeasureType', 'Month', 'OpponentTeamID', 'Outcome',
                  'PORound', 'PaceAdjust', 'PerMode', 'Period', 'PlusMinus',
                  'Rank', 'Season', 'SeasonSegment', 'SeasonType',
                  'TeamID', 'VsConfererence', 'VsDivision')
  
  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

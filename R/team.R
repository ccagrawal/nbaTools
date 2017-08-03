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
#' GetTeamPlayerOnOffDetails(TeamID = '1610612756')

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
#' GetTeamGeneralSplits(split = 'location', TeamID = '1610612756')

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
#' GetTeamShotDashboard(split = 'location', TeamID = '1610612756')

GetTeamGeneralSplits <- function(split = 'location', ...) {
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
  }

  return(GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = ix, ...))
}

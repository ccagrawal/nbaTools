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
#' GetTeamPlayerOnOffDetails(SeasonType = 'Playoffs')

GetTeamPlayerOnOffDetails <- function(...) {
  endpoint <- 'teamplayeronoffdetails'

  on <- GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = 2, ...)
  off <- GetTeamDashboard(source = 'NBA', endpoint = endpoint, ix = 3, ...)

  return(rbind(on, off))
}

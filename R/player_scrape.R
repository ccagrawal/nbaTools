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

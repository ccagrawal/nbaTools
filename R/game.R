#' Play by play
#'
#' @return data frame with list of plays in a game
#' @keywords plays
#' @export
#' @examples
#' GetPlayByPlay(GameID = '0021600794')

GetPlayByPlay <- function(...) {

  endpoint <- 'playbyplayv2'
  referer <- 'game'
  ix <- 1

  param.keys <- c('GameID', 'Season', 'SeasonType', 'StartPeriod',
                  'EndPeriod', 'StartRange', 'EndRange', 'RangeType')

  return(GetData(endpoint, referer, ix, param.keys, source = 'NBA', ...))
}

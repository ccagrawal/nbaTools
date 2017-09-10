#' Play by play
#'
#' @return data frame with list of plays in a game
#' @keywords plays
#' @export
#' @examples
#' # GetPlayByPlay(GameID = '0021600794')

GetPlayByPlay <- function(...) {

  endpoint <- 'playbyplayv2'
  referer <- 'game'
  ix <- 1

  param.keys <- c('GameID', 'Season', 'SeasonType', 'StartPeriod',
                  'EndPeriod', 'StartRange', 'EndRange', 'RangeType')

  kwargs <- list(...)
  kwargs[['Season']] <- ''
  kwargs[['SeasonType']] <- ''

  arg.list <- list(endpoint = endpoint, referer = referer, ix = ix,
                   param.keys = param.keys, source = 'NBA')

  return(do.call(GetData, c(arg.list, kwargs)))
}

#' Box Score
#'
#' @return data frame with game's box score
#' @keywords boxscore
#' @export
#' @examples
#' # GetBoxScore(GameID = '0021600794')

GetBoxScore <- function(...) {

  endpoint <- 'boxscoretraditionalv2'
  referer <- 'game'
  ix <- 1

  param.keys <- c('GameID', 'Season', 'SeasonType', 'StartPeriod',
                  'EndPeriod', 'StartRange', 'EndRange', 'RangeType')

  kwargs <- list(...)
  kwargs[['Season']] <- ''
  kwargs[['SeasonType']] <- ''

  arg.list <- list(endpoint = endpoint, referer = referer, ix = ix,
                   param.keys = param.keys, source = 'NBA')

  return(do.call(GetData, c(arg.list, kwargs)))
}

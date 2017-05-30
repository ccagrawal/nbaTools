#' @include helper.R

kBaseURL <- 'http://stats.nba.com/stats/%endpoint%'
kHeaders <- list(
  'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
  'Referer' = 'http://stats.nba.com/%referer%/',
  'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5)',
                       'AppleWebKit/537.36 (KHTML, like Gecko)',
                       'Chrome/57.0.2987.133 Safari/537.36')
)

kDefaultParams = list(
  AheadBehind = '',
  ClutchTime = '',
  College = '',
  Conference = '',
  ContextMeasure = 'FGM',
  Counter = 1000,
  Country = '',
  DateFrom = '',
  DateTo = '',
  Direction = 'DESC',
  Division = '',
  DraftPick = '',
  DraftYear = '',
  EndPeriod = 0,
  EndRange = 0,
  Game_Scope = '',
  GameID = '',
  GameScope = '',
  GameSegment = '',
  GroupQuantity = 5,
  Height = '',
  LastNGames = 0,
  League = '00',
  Location = '',
  MeasureType = 'Base',
  Month = 0,
  OpponentTeamID = 0,
  Outcome = '',
  PORound = 0,
  PaceAdjust = 'N',
  PerMode = 'PerGame',
  Period = 0,
  Player_or_Team = 'P',
  PlayerExperience = '',
  PlayerOrTeam = 'Player',
  PlayerPosition = '',
  PlayerScope = 'All Players',
  PlayoffRound = 0,
  PlusMinus = 'N',
  PtMeasureType = 'SpeedDistance',
  RangeType = 0,
  Rank = 'N',
  RookieYear = '',
  Scope = 'S',
  ShotClockRange = '',
  Season = kSeason,
  SeasonSegment = '',
  SeasonType = 'Regular Season',
  ShotClockRange = '',
  Sorter = 'PTS',
  StarterBench = '',
  StartPeriod = 0,
  StartRange = 0,
  StatCategory = 'PTS',
  TeamID = 0,
  VsConference = '',
  VsDivision = '',
  Weight = ''
)

GenerateParams <- function(param.keys, ...) {
  params <- list()
  kwargs <- list(...)

  for (k in param.keys) {
    params[[k]] <- kDefaultParams[[k]]
  }

  for (k in names(kwargs)) {
    params[[k]] <- kwargs[[k]]
  }

  return(params)
}

ScrapeContent <- function(endpoint, params, referer) {
  headers <- kHeaders
  headers['Referer'] <- gsub('%referer%', referer, headers['Referer'])

  request <- GET(
    url = gsub('%endpoint%', endpoint, kBaseURL),
    query = params,
    do.call(add_headers, headers)
  )

  return(content(request, 'parsed'))
}

#' NBA Content to DataFrame
#'
#' @return data frame with info from stats.nba.com
#' @importFrom utils type.convert

ContentToDataFrame <- function(content, ix) {
  options(stringsAsFactors = FALSE)

  if ('resultSets' %in% names(content)) {
    content <- content$resultSets
  } else if ('resultSet' %in% names(content)) {
    content <- content$resultSet
  } else {
    stop('Invalid stats.nba.com content provided.')
  }

  if (!missing(ix)) {
    content <- content[[ix]]
  }

  data <- content$rowSet
  data <- lapply(data, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE)) # Turn list to data frame
  colnames(data) <- content$headers
  data[] <- lapply(data, type.convert, as.is = TRUE)

  return(data)
}

GetNBAData <- function(endpoint, referer, ix, param.keys, ...) {
  params <- GenerateParams(param.keys, ...)
  content <- ScrapeContent(endpoint, params, referer)
  df <- ContentToDataFrame(content, ix)

  Sys.sleep(1)
  return(df)
}

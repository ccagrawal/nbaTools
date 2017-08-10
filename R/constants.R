#' @include helper.R

kToday <- Sys.Date()
kYear <- CurrentYear()
kSeason <- YearToSeason(kYear)

kBaseURL <- list(
  'NBA' = 'http://stats.nba.com/stats/%endpoint%',
  'NBA.Synergy' = 'http://stats-prod.nba.com/wp-json/statscms/v1/synergy/%endpoint%',
  'BRef' = 'http://www.basketball-reference.com/%endpoint%'
)

kHeaders <- list(
  'NBA' = list(
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'http://stats.nba.com/%referer%/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/57.0.2987.133 Safari/537.36')
  ),
  'NBA.Synergy' = list(
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'http://stats.nba.com/%referer%/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/57.0.2987.133 Safari/537.36')
  ),
  'BRef' = list(
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'http://www.basketball-reference.com/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/57.0.2987.133 Safari/537.36')
  )
)

kDefaultParams = list(
  'NBA' = list(
    AheadBehind = '',
    ClutchTime = '',
    College = '',
    Conference = '',
    ContextMeasure = 'FGM',
    Counter = 1000,
    Country = '',
    DateFrom = '',
    DateTo = '',
    DefenseCategory = '3 Pointers',
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
    LeagueID = '00',
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
  ),
  'NBA.Synergy' = list(
    category = 'Transition',
    limit = 500,
    names = 'offensive',
    q = 2501745,
    season = kYear - 1,
    seasonType = 'Reg'
  ),
  'BRef' = list(
    TeamID = 'HOU',
    Season = kYear
  )
)

GenerateParams <- function(param.keys, source = 'NBA', ...) {
  params <- list()
  kwargs <- list(...)

  for (k in param.keys) {
    params[[k]] <- kDefaultParams[[source]][[k]]
  }

  for (k in names(kwargs)) {
    if (k == 'Season') {
      params[[k]] <- YearToSeason(kwargs[[k]])
    } else if (k == 'season') {
      params[[k]] <- kwargs[[k]] - 1
    } else {
      params[[k]] <- kwargs[[k]]
    }
  }

  return(params)
}

#' @importFrom httr GET content add_headers

ScrapeContent <- function(endpoint, params, referer, source = 'NBA') {
  headers <- kHeaders[[source]]

  if (source %in% c('NBA', 'NBA.Synergy')) {
    headers['Referer'] <- gsub('%referer%', referer, headers['Referer'])

    request <- GET(
      url = gsub('%endpoint%', endpoint, kBaseURL[[source]]),
      query = params,
      do.call(add_headers, headers)
    )

    return(content(request, 'parsed'))

  } else if (source == 'BRef') {
    url <- gsub('%endpoint%', endpoint, kBaseURL[[source]])

    for (k in names(params)) {
      url <- gsub(k, params[[k]], url)
    }

    # content <- read_html(url)
    # content <- content %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
    #   html_text() %>%             # extract comment text
    #   paste(collapse = '') %>%    # collapse to a single string
    #   read_html()
    #
    # content <- rawToChar(request$content)
    # content <- gsub('<!--(.*)-->', '\\1', content)
    # return(content)
  }
}

#' @importFrom utils type.convert

ContentToDataFrame <- function(content, ix, source = 'NBA') {
  options(stringsAsFactors = FALSE)

  if (source == 'NBA') {
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

  } else if (source == 'NBA.Synergy') {
    if ('results' %in% names(content)) {
      data <- do.call(rbind, lapply(content$results, data.frame))
      
      # Fix shifted play type tables
      ix <- is.na(as.numeric(data$PlayerIDSID))
      if (sum(ix) > 0) {
        data[ix, 1:18] <- data[ix, c(13:18, 1:12)]
      }
      
    } else {
      stop('Invalid stats.nba.com content provided.')
    }
  } else if (source == 'BRef') {
    # data <- content %>%
    #   html_node(ix) %>%
    #   html_table()

    # data <- readHTMLTable(content, header = FALSE)[[ix]]
  }

  return(data)
}

GetData <- function(endpoint, referer, ix, param.keys, source = 'NBA', ...) {
  params <- GenerateParams(param.keys, source, ...)
  content <- ScrapeContent(endpoint, params, referer, source)
  df <- ContentToDataFrame(content, ix, source)

  Sys.sleep(1)
  return(df)
}

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
    AheadBehind = 'Ahead or Behind',
    ClutchTime = 'Last 5 Minutes',
    College = '',
    Conference = '',
    ContextMeasure = 'FGM',
    Counter = 1000,
    Country = '',
    DateFrom = '',
    DateTo = '',
    DayOffset = 0,
    DefenseCategory = '3 Pointers',
    Direction = 'DESC',
    Division = '',
    DraftPick = '',
    DraftYear = '',
    EndPeriod = 10,
    EndRange = 55800,
    gameDate = format(kToday, "%m/%d/%Y"),
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
    PointDiff = 5,
    PtMeasureType = 'SpeedDistance',
    RangeType = 2,
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
    StartPeriod = 1,
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

CHAR.COLS <- c('Team_ID', 'TEAM_ID', 'Game_ID', 'GAME_ID', 'PLAYER1_ID',
               'PLAYER1_TEAM_ID', 'PLAYER2_ID', 'PLAYER2_TEAM_ID',
               'PLAYER3_ID', 'PLAYER3_TEAM_ID')

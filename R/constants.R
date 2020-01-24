kToday <- Sys.Date()
kYear <- CurrentYear()
kSeason <- YearToSeason(kYear)

kBaseURL <- list(
  'NBA' = 'http://stats.nba.com/stats/%endpoint%',
  'BRef' = 'http://www.basketball-reference.com/%endpoint%',
  'PBP' = 'https://api.pbpstats.com/%endpoint%'
)

kHeaders <- list(
  'NBA' = list(
    'Accept' = 'application/json, text/plain, */*',
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'http://stats.nba.com/%referer%/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/67.0.3396.99 Safari/537.36'),
    'x-nba-stats-origin' = 'stats',
    'x-nba-stats-token' = 'true'
  ),
  'BRef' = list(
    'Accept' = 'application/json, text/plain, */*',
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'http://stats.nba.com/%referer%/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/67.0.3396.99 Safari/537.36')
  ),
  'PBP' = list(
    'Accept' = 'application/json, text/plain, */*',
    'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
    'Referer' = 'https://www.pbpstats.com/%referer%/',
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6)',
                         'AppleWebKit/537.36 (KHTML, like Gecko)',
                         'Chrome/72.0.3626.121 Safari/537.36')
  )
)

kDefaultParams = list(
  'NBA' = list(
    AheadBehind = 'Ahead or Behind',
    CloseDefDistRange = '',
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
    DistanceRange = 'By Zone',
    Division = '',
    DraftPick = '',
    DraftYear = '',
    DribbleRange = '',
    EndPeriod = 10,
    EndRange = 55800,
    gameDate = format(kToday, "%m/%d/%Y"),
    Game_Scope = '',
    GameID = '',
    GameScope = '',
    GameSegment = '',
    GeneralRange = '',
    GroupQuantity = 5,
    Height = '',
    IsOnlyCurrentSeason = 0,
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
    PlayType = 'Isolation',
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
    SeasonYear = kSeason,
    ShotClockRange = '',
    ShotDistRange = '',
    Sorter = 'DATE',
    StarterBench = '',
    StartPeriod = 1,
    StartRange = 0,
    StatCategory = 'PTS',
    TeamID = 0,
    TouchTimeRange = '',
    TypeGrouping = 'offensive',
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
    Season = kYear,
    MeasureType = 'totals',
    PlayerID = 'j/jamesle01'
  ),
  'PBP' = list(
    OffDef = 'Offense',
    Season = kSeason,
    SeasonType = 'Regular Season',
    StarterState = '5v5',
    StartType = 'All',
    TeamID = '1610612745',
    Type = 'Player'
  )
)

CHAR.COLS <- c('Team_ID', 'TEAM_ID', 'TeamIDSID', 'PLAYER1_TEAM_ID', 'PLAYER2_TEAM_ID', 'PLAYER3_TEAM_ID',
               'Player_ID', 'PLAYER_ID', 'PERSON_ID', 'PLAYER1_ID',  'PLAYER2_ID', 'PLAYER3_ID',
               'Game_ID', 'GAME_ID',
               'Player', 'Pos', 'Tm')

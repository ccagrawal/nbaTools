#' Lineups
#'
#' @return Data frame with 5 man lineups with play-by-play from NBA.com
#' @keywords lineups playbyplay
#' @export
#' @examples
#' # GetLineups(GameID = '0021300359')

GetLineups <- function(GameID) {

  # Get play by play first
  pbp <- GetPlayByPlay(GameID = GameID)

  # Add markers for when the 10 man lineups can change
  mark.periods <- c(1, which(pbp$EVENTMSGTYPE == 13 & pbp$EVENTMSGACTIONTYPE == 0))   # End of period
  markers <- which(pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0)  # Subs

  # Group subs based on consecutive actions
  ranges <- list(markers[1])
  last <- markers[1]
  last.time <- pbp[markers[1], 'PCTIMESTRING']
  for (i in 2:length(markers)) {
    if ((markers[i] == (last + 1)) & (pbp[markers[i], 'PCTIMESTRING'] == last.time)) {
      ranges[[length(ranges)]] <- c(ranges[[length(ranges)]], markers[i])
    } else {
      ranges[[length(ranges) + 1]] <- markers[i]
    }

    last <- markers[i]
    last.time <- pbp[markers[i], 'PCTIMESTRING']
  }

  # Remove consecutive markers at the same timestamp
  markers <- sapply(ranges, function(x) x[1])

  # Add periods into markers
  markers <- c(markers, mark.periods)
  markers <- markers[order(markers)]

  # Get team IDs
  home.id <- pbp[pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0 & !is.na(pbp$HOMEDESCRIPTION), 'PLAYER1_TEAM_ID'][1]
  away.id <- pbp[pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0 & !is.na(pbp$VISITORDESCRIPTION), 'PLAYER1_TEAM_ID'][1]

  # Go through substitutions and get preliminary lineups
  home.players <- .ProcessSubs(pbp, markers, mark.periods, ranges, 'Home')
  away.players <- .ProcessSubs(pbp, markers, mark.periods, ranges, 'Away')

  # Get # players in each range; if not 10, then use the box score
  i <- length(mark.periods)
  num.players <- sapply(home.players, function(x) length(x)) + sapply(away.players, function(x) length(x))

  while ((length(num.players[num.players != 10]) > 0) & (i > 1)) {

    # Decrement the period
    i <- i - 1

    # Get box score for period and split into home and away
    box.score <- GetBoxScore(GameID = GameID, RangeType = 1, StartPeriod = i, EndPeriod = i)

    if (i <= 4) {
      box.score <- box.score[box.score$MIN == '12:00', ]
    } else {
      box.score <- box.score[box.score$MIN == '5:00', ]
    }

    home.add <- box.score[box.score$TEAM_ID == home.id, 'PLAYER_ID']
    away.add <- box.score[box.score$TEAM_ID == away.id, 'PLAYER_ID']

    # Get range corresponding to period in players list
    group.first <- length(markers[markers <= mark.periods[i]])
    group.last <- length(markers[markers <= mark.periods[i + 1]]) - 1

    # If home players haven't played in the quarter, add them to the whole thing
    for (player in home.add) {
      if (!(player %in% .GetPlayers(home.players, group.first, group.last))) {
        home.players <- .AddPlayers(player, home.players, group.first, group.last)
      }
    }

    # If away players haven't played in the quarter, add them to the whole thing
    for (player in away.add) {
      if (!(player %in% .GetPlayers(away.players, group.first, group.last))) {
        away.players <- .AddPlayers(player, away.players, group.first, group.last)
      }
    }

    # Update the player counts
    num.players <- sapply(home.players, function(x) length(x)) + sapply(away.players, function(x) length(x))
  }

  pbp[, c('H1', 'H2', 'H3', 'H4', 'H5', 'A1', 'A2', 'A3', 'A4', 'A5')] <- NA

  # Add player ids into pbp
  for (i in 1:length(home.players)) {
    length <- markers[i + 1] - markers[i] + 1
    pbp[markers[i]:markers[i + 1], c('H1', 'H2', 'H3', 'H4', 'H5')] <- rep(home.players[[i]], each = length)
    pbp[markers[i]:markers[i + 1], c('A1', 'A2', 'A3', 'A4', 'A5')] <- rep(away.players[[i]], each = length)
  }

  return(pbp)
}

# Input:    pbp - NBA play by play data frame
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           ranges - list of consecutive subs
#           team - either 'Home' or 'Away'
# Output:   List of arrays of players at each time
.ProcessSubs <- function(pbp, markers, mark.periods, ranges, team) {

  # Keep track of players that are in the lineup between each marker
  players <- lapply(markers[-1], function(x) NULL)

  # Create act column corresponding to home or away team
  if (team == 'Home') {
    actions <- pbp$HOMEDESCRIPTION
  } else {
    actions <- pbp$VISITORDESCRIPTION
  }

  subs <- grep('SUB:', actions)                               # Find plays with subs
  for (j in 1:length(ranges)) {

    # Get subs in that range
    current.subs <- subs[subs %in% ranges[[j]]]

    # Only proceed if there was at least 1 sub
    if (length(current.subs) > 0) {

      # Get one sub
      i <- current.subs[1]

      # Compute the group this sub happened between, and the first and last group of the period
      group <- length(markers[markers <= i]) - 1
      i.last <- min(mark.periods[mark.periods > i])
      group.last <- length(markers[markers <= i.last]) - 1
      i.first <- max(mark.periods[mark.periods <= i])
      group.first <- length(markers[markers <= i.first])

      # Get the players subbed in and the players subbed out
      players.in <- pbp[current.subs, 'PLAYER2_ID']
      players.out <- pbp[current.subs, 'PLAYER1_ID']

      # Remove players subbed in and out
      overlap <- intersect(players.in, players.out)
      for (player in overlap) {
        players.in <- players.in[-match(player, players.in)]
        players.out <- players.out[-match(player, players.out)]
      }

      # Loop through subs
      if (length(players.in) > 0) {
        for (k in 1:length(players.in)) {

          player.in <- players.in[k]
          player.out <- players.out[k]

          # If we don't have the player that subbed out at all, add him in the whole quarter up to now
          if (!(player.out %in% .GetPlayers(players, group.first, group))) {
            players <- .AddPlayers(player.out, players, group.first, group)
          }

          # Remove the player subbed out for the remainder of the quarter, and add the guy subbed in
          players <- .RemovePlayer(player.out, players, group + 1, group.last)
          players <- .AddPlayers(player.in, players, group + 1, group.last)
        }
      }
    }
  }

  return(players)
}

# Get all the players in the lineups between the first and last indices
.GetPlayers <- function(lineups, first, last) {
  players <- c()
  for (i in first:last) {
    players <- union(players, lineups[[i]])
  }
  return(players)
}

# Add all the players to the lineups between the first and last indices
.AddPlayers <- function(players, lineups, first, last) {
  for (i in first:last) {
    lineups[[i]] <- union(lineups[[i]], players)
  }
  return(lineups)
}

# Remove player from the lineups between the first and last indices
.RemovePlayer <- function(player, lineups, first, last) {
  for (i in first:last) {
    if (player %in% lineups[[i]]) {
      lineups[[i]] <- lineups[[i]][lineups[[i]] != player]
    }
  }
  return(lineups)
}

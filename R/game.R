#' Lineups
#'
#' @return Data frame with 5 man lineups with play-by-play from NBA.com
#' @keywords lineups playbyplay
#' @export
#' @examples
#' # GetLineups(game.id = '0021300359')

GetLineups <- function(game.id) {

  # Get play by play first
  pbp <- GetPlayByPlay(GameID = game.id)
  pbp <- pbp[-which(pbp$EVENTMSGTYPE == 18), ]

  # Add markers for when the 10 man lineups can change
  mark.periods <- c(1, which(pbp$EVENTMSGTYPE == 13 & pbp$EVENTMSGACTIONTYPE == 0))   # End of period
  
  # Check if duplicate "end of periods"
  period.diffs <- diff(mark.periods)
  while (sum(period.diffs == 1) > 0) {
    bad.marker <- mark.periods[which(period.diffs == 1) + 1][1]
    pbp <- pbp[-bad.marker, ]
    row.names(pbp) <- 1:nrow(pbp)

    mark.periods <- c(1, which(pbp$EVENTMSGTYPE == 13 & pbp$EVENTMSGACTIONTYPE == 0))   # End of period
    period.diffs <- diff(mark.periods)
  }
  
  markers <- which(pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0)  # Subs

  # For some reason last play sometimes marked wrong
  if ((max(mark.periods) + 20) < nrow(pbp)) {
    mark.periods <- c(mark.periods, max(which(pbp$EVENTMSGTYPE == 18 & pbp$EVENTMSGACTIONTYPE == 0)))
  }
  
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
  
  # Add players who played the entire quarter
  home.players <- .AddFullQuarterPlayers(home.players, game.id, markers, mark.periods, home.id)
  away.players <- .AddFullQuarterPlayers(away.players, game.id, markers, mark.periods, away.id)

  # Sometimes the full quarter isn't the right length. Add those players back in
  home.players <- .AddFullQuarterPlayersWrongTime(home.players, game.id, markers, mark.periods, home.id)
  away.players <- .AddFullQuarterPlayersWrongTime(away.players, game.id, markers, mark.periods, away.id)
  
  # Sometimes the player isn't in the box score. Add players who had an action back in
  home.players <- .AddPlayersNotInBoxScore(home.players, pbp, markers, mark.periods, home.id)
  away.players <- .AddPlayersNotInBoxScore(away.players, pbp, markers, mark.periods, away.id)
  
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

# Input:    players - list of vectors of players in
#           game.id - game ID to get box score
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           team.id - id of team to match with box scores
# Output:   List of arrays of players at each time
.AddFullQuarterPlayers <- function(players, game.id, markers, mark.periods, team.id) {
  
  # Get # players in each range; if not 10, then use the box score
  i <- length(mark.periods)
  num.players <- sapply(players, function(x) length(x))
  
  # Add players who were in the entire quarter
  while ((length(num.players[num.players != 5]) > 0) & (i > 1)) {
    
    # Decrement the period
    i <- i - 1
    
    # Get box score for period and split into home and away
    box.score <- GetBoxScore(GameID = game.id, RangeType = 1, StartPeriod = i, EndPeriod = i)
    
    if (i <= 4) {
      box.score <- box.score[box.score$MIN == '12:00', ]
    } else {
      box.score <- box.score[box.score$MIN == '5:00', ]
    }
    
    players.to.add <- box.score[box.score$TEAM_ID == team.id, 'PLAYER_ID']
    
    # Get range corresponding to period in players list
    group.first <- length(markers[markers <= mark.periods[i]])
    group.last <- length(markers[markers <= mark.periods[i + 1]]) - 1
    
    # If home players haven't played in the quarter, add them to the whole thing
    for (player in players.to.add) {
      if (!(player %in% .GetPlayers(players, group.first, group.last))) {
        players <- .AddPlayers(player, players, group.first, group.last)
      }
    }
    
    # Update the player counts
    num.players <- sapply(players, function(x) length(x))
  }
  
  return(players)
}

# Input:    players - list of vectors of players in
#           game.id - game ID to get box score
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           team.id - id of team to match with box scores
# Output:   List of arrays of players at each time
.AddFullQuarterPlayersWrongTime <- function(players, game.id, markers, mark.periods, team.id) {
  
  # Get # players in each range; if not 10, then use the box score
  i <- length(mark.periods)
  num.players <- sapply(players, function(x) length(x))
  
  # Add players who were in the entire quarter
  while ((length(num.players[num.players != 5]) > 0) & (i > 1)) {
    
    # Decrement the period
    i <- i - 1
    
    # Get box score for period and split into home and away
    box.score <- GetBoxScore(GameID = game.id, RangeType = 1, StartPeriod = i, EndPeriod = i)
    
    box.score$MIN <- TimeToSeconds(box.score$MIN)
    max.time <- round(sum(box.score$MIN) / 10)
    box.score <- box.score[box.score$MIN == max.time, ]
    
    players.to.add <- box.score[box.score$TEAM_ID == team.id, 'PLAYER_ID']
    
    # Get range corresponding to period in players list
    group.first <- length(markers[markers <= mark.periods[i]])
    group.last <- length(markers[markers <= mark.periods[i + 1]]) - 1
    
    # If home players haven't played in the quarter, add them to the whole thing
    for (player in players.to.add) {
      if (!(player %in% .GetPlayers(players, group.first, group.last))) {
        players <- .AddPlayers(player, players, group.first, group.last)
      }
    }
    
    # Update the player counts
    num.players <- sapply(players, function(x) length(x))
  }
  
  return(players)
}


# Input:    players - list of vectors of players in
#           pbp - NBA play by play data frame
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           team.id - id of team to match with box scores
# Output:   List of arrays of players at each time
.AddPlayersNotInBoxScore <- function(players, pbp, markers, mark.periods, team.id) {
  
  # Get # players in each range; if not 10, then use the box score
  i <- length(mark.periods)
  num.players <- sapply(players, function(x) length(x))
  
  # Add players who were in the entire quarter
  while ((length(num.players[num.players != 5]) > 0) & (i > 1)) {
    
    # Decrement the period
    i <- i - 1
    
    # Get range corresponding to period in players list
    group.first <- length(markers[markers <= mark.periods[i]])
    group.last <- length(markers[markers <= mark.periods[i + 1]]) - 1
    
    q.players <- players[group.first:group.last]
    q.num <- sapply(q.players, function(x) length(x))
    
    # If less than 5 players, find a missing player
    if (mean(q.num) < 5) {
      
      # Get players we already have
      existing.players <- unique(paste(unlist(q.players)))
      
      # Get plays with presence actions
      actions <- pbp[mark.periods[i]:mark.periods[i + 1], ]
      actions <- actions[!is.na(actions$PLAYER1_TEAM_ID) & (actions$PLAYER1_TEAM_ID == team.id), ]
      actions <- actions[actions$EVENTMSGTYPE %in% c(1, 2, 4), ]
      
      potential.players <- unique(actions$PLAYER1_ID)
      potential.players <- potential.players[!(potential.players %in% existing.players)]
      
      if (length(potential.players) == (5 - mean(q.num))) {
        for (player in potential.players) {
          players <- .AddPlayers(player, players, group.first, group.last)
        }
      }
      
      # Update the player counts
      num.players <- sapply(players, function(x) length(x))
    }
  }
  
  return(players)
}
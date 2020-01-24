GenerateParams <- function(param.keys, source = 'NBA', ...) {
  params <- list()
  kwargs <- list(...)

  for (k in param.keys) {
    params[[k]] <- kDefaultParams[[source]][[k]]
  }

  for (k in names(kwargs)) {

    if ((k %in% c('Season', 'SeasonYear')) & (source %in% c('NBA', 'PBP'))) {
      if (length(kwargs[[k]]) > 1) {
        params[[k]] = paste(YearToSeason(kwargs[[k]]), collapse = ',')
      } else {
        params[[k]] <- YearToSeason(kwargs[[k]])
      }

    } else if (k == 'season') {
      params[[k]] <- kwargs[[k]] - 1

    } else if (k == 'Date') {
      if (class(kwargs[[k]]) == 'character') {
        kwargs[[k]] <- as.Date(kwargs[[k]])
      }

      params[['gameDate']] <- kwargs[[k]]

    } else if ((k == 'SeasonType') & (source == 'BRef')) {
      if (kwargs[[k]] == 'Regular Season') {
        params[[k]] <- 'leagues'
      } else if (kwargs[[k]] == 'Playoffs') {
        params[[k]] <- 'playoffs'
      }

    } else if ((k == 'PlayerIds') & (length(kwargs[[k]]) > 1)) {
      params[[k]] = paste(kwargs[[k]], collapse = ',')

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
      url <- gsub(paste0('<', k, '>'), params[[k]], url)
    }

    request <- GET(url)
    content <- rawToChar(request$content)

    content <- gsub('<!--([(?!-->)]*)', '\\1', content)
    return(content)
  } else if (source == 'PBP') {
    headers['Referer'] <- gsub('%referer%', referer, headers['Referer'])

    request <- GET(
      url = gsub('%endpoint%', endpoint, kBaseURL[[source]]),
      query = params,
      do.call(add_headers, headers)
    )

    return(content(request, 'parsed'))

  }
}

#' @importFrom utils type.convert
#' @importFrom XML readHTMLTable
#' @importFrom dplyr bind_rows

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

    if (!is.null(ix)) {
      content <- content[[ix]]
    }

    data <- content$rowSet
    data <- lapply(data, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs

    if (length(data) == 0) {
      return(NULL)
    }

    data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE))  # Turn list to data frame

    if (class(content$headers[[1]]) == 'list') {
      headers <- unlist(content$headers[[2]]$columnNames)
      temp <- content$headers[[1]]

      ix <- temp$columnsToSkip + 1
      span <- temp$columnSpan

      for (i in 1:length(temp$columnNames)) {
        label <- temp$columnNames[[i]]
        all.ix <- ix:(ix + span - 1)
        headers[all.ix] <- paste0(label, '.', headers[all.ix])
        ix <- ix + span
      }

    } else {
      headers <- content$headers
    }

    colnames(data) <- headers

  } else if (source == 'NBA.Synergy') {
    if ('results' %in% names(content)) {
      data <- do.call(rbind, lapply(content$results, data.frame))

      # Fix shifted play type tables
      ix <- is.na(as.numeric(data$PlayerIDSID))
      if (sum(ix) > 0) {
        data[ix, 1:18] <- data[ix, c(13:18, 1:12)]
      }

      data[] <- lapply(data, as.character)

    } else {
      stop('Invalid stats.nba.com content provided.')
    }

  } else if (source == 'BRef') {
    data <- readHTMLTable(content)[[ix]]
    headers <- colnames(data)
    dup.headers <- apply(data, 1, function(x) x == headers)

    if (sum(dup.headers) > 0) {
      data <- data[-which(colSums(dup.headers) > 1), ]
    }

    data <- data[, colSums(is.na(data)) < nrow(data)]

  } else if (source == 'PBP') {

    if (!is.null(ix)) {
      content <- content[[ix]]
    } else if ('multi_row_table_data' %in% names(content)) {
      content <- content[['multi_row_table_data']]
    } else {
      content <- content[['results']]
    }

    if (class(content[[1]]) == 'list') {
      content <- lapply(content, Filter, f = Negate(is.null))
      data <- bind_rows(lapply(content, as.data.frame.list))
    } else {
      return(content)
    }
  }

  keep.char <- which(colnames(data) %in% CHAR.COLS)
  if (length(keep.char) > 0) {
    data[, -keep.char] <- lapply(data[, -keep.char], type.convert, as.is = TRUE)
  } else {
    data[] <- lapply(data, type.convert, as.is = TRUE)
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

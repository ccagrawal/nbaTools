GenerateParams <- function(param.keys, source = 'NBA', ...) {
  params <- list()
  kwargs <- list(...)

  for (k in param.keys) {
    params[[k]] <- kDefaultParams[[source]][[k]]
  }

  for (k in names(kwargs)) {

    if ((k == 'Season') & (source == 'NBA')) {
      params[[k]] <- YearToSeason(kwargs[[k]])

    } else if (k == 'season') {
      params[[k]] <- kwargs[[k]] - 1

    } else if (k == 'Date') {
      if (class(kwargs[[k]]) == 'character') {
        kwargs[[k]] <- as.Date(kwargs[[k]])
      }

      params[['gameDate']] <- kwargs[[k]]

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

    # content <- read_html(url)
    # content <- content %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
    #   html_text() %>%             # extract comment text
    #   paste(collapse = '') %>%    # collapse to a single string
    #   read_html()

    request <- GET(url)
    content <- rawToChar(request$content)

    content <- gsub('<!--(.*)-->', '\\1', content)
    return(content)
  }
}

#' @importFrom utils type.convert
#' @importFrom XML readHTMLTable

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

    if (length(data) == 0) {
      return(NULL)
    }

    data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE)) # Turn list to data frame
    colnames(data) <- content$headers

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

    data <- readHTMLTable(content)[[ix]]
    headers <- colnames(data)
    dup.headers <- apply(data, 1, function(x) x == headers)
    data <- data[-which(colSums(dup.headers) == ncol(data)), ]
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

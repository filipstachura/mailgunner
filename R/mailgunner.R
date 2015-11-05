# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

mailgun_stats <- function(url, api_key) {
  auth <- httr::authenticate(user = "api", password = api_key)
  r <- httr::GET(url, auth, query = list(event='accepted', event="opened"))
  httr::stop_for_status(r)
  httr::content(r, "parsed", "application/json")
  r
}

mailgun_logs <- function(url, api_key, skip = 0, limit = 100) {
  auth <- httr::authenticate(user = "api", password = api_key)
  r <- httr::GET(url, auth, query = list(skip=skip, limit=limit))
  httr::stop_for_status(r)
  json <- httr::content(r, "parsed", "application/json")
  df_nrow <- length(json$items)
  df <- data.frame(matrix(unlist(json$items), nrow = df_nrow, byrow = T))
  colnames(df) <- c("hap", "message", "type", "created_at", "message_id")
  df
}

mailgun_events <- function(url, api_key, nreq = 0) {
  auth <- httr::authenticate(user = "api", password = api_key)
  r <- httr::GET(url, auth)
  httr::stop_for_status(r)
  json <- httr::content(r, "parsed", "application/json")
  items <- json$items
  success <- TRUE
  nreq <- nreq - 1
  if (nreq == 0) {
    success <- FALSE
  }
  while(success) {
    success <- tryCatch({
      message("Loading next part of events...")
      next_url <- json$paging[["next"]]
      r <- httr::GET(next_url, auth)
      httr::stop_for_status(r)
      json <- httr::content(r, "parsed", "application/json")
      items <- c(items, json$items)
      continue <- (length(json$items) != 0)
      continue
    }, error = function(e) {
      message(e)
      FALSE
    })
    nreq <- nreq - 1
    if (nreq == 0) {
      success <- FALSE
    }
  }
  df <- plyr::ldply(items, function(s){
    t(data.frame(unlist(s)))
  })
  df
}

mailgun_logs_all <- function(url, api_key) {
  index <- 0
  fetch <- 300
  df <- mailgun_logs(url, api_key, skip = index, limit=fetch)
  success <- TRUE
  while(success) {
    success <- tryCatch({
      message("Loading next part of logs...")
      index <- index + fetch
      df_next <- mailgun_logs(url, api_key, skip = index, limit=fetch)
      df <- rbind(df, df_next)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  df
}

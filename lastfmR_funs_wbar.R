api_root <- "http://ws.audioscrobbler.com/2.0/?method="
api_key <- "23fadd845ffb9a4ece7caeaecd74c94e"

# reformat curl reponse
parse_content <- function(response){
  content <- rawToChar(response$content)
  Encoding(content) <- 'UTF-8'
  return(content)
}

# run batch or urls
run_batch <- function(url_list, indices, update_data){
  pool <- new_pool()
  for (i in indices) {
    curl_fetch_multi(url_list[i], pool = pool, done = update_data)
  }
  out <- multi_run(pool = pool)
}

get_scrobbles <- function(user, timezone = 'GMT') {
  withProgress(message = "Getting Listening History", value = 0, {
  #get number of pages
  first_url <- paste0(
    api_root,
    "user.getRecentTracks&user=",
    user,
    "&limit=1000&api_key=",
    api_key
  )
  first_url_conn <- curl(first_url)
  page_check <- try(readLines(first_url_conn), silent = TRUE)
  close(first_url_conn)
  if(class(page_check)[1] == "try-error"){
    # todo connection problems vs invalid username (?)
    stop("Invalid username")
  }
  parsed_xml <- read_xml(paste(page_check, collapse = '\n'))
  pageline <- xml_find_first(parsed_xml, ".//recenttracks")
  pages <- as.integer(xml_attr(pageline, 'totalPages'))
  # total number of scrobbles
  # +20 to prevent out of range error
  # (if the user is scrobbling right now, data grows during downloading)
  total <- as.integer(xml_attr(pageline, 'total')) + 20
  
  #allocate data.table
  scrobbles <- data.table(
    date = as.integer(rep(NA_integer_, total)),
    artist = as.character(rep(NA_character_, total)),
    track = as.character(rep(NA_character_, total)),
    album = as.character(rep(NA_character_, total))
  )
  
  #get XML files
  lastfm_urls <- paste0(
    api_root,
    "user.getRecentTracks&user=",
    user,
    "&limit=1000&page=",
    seq(pages),
    "&api_key=",
    api_key
  )
  
  #pb <- txtProgressBar(min = 0, max = pages, style = 3)
  add_data <- function(response){
    page_index <- which(lastfm_urls == response$url)
    parsed_xml <- read_xml(parse_content(response))
    entries <- xml_find_all(parsed_xml, ".//track")
    if (!is.na(xml_attr(entries[1], 'nowplaying'))) {
      entries <- entries[2:length(entries)]
    }
    dates <- as.integer(xml_attr(xml_find_all(entries, './/date'), 'uts'))
    artists <- xml_text(xml_find_all(entries, './/artist'))
    tracks <- xml_text(xml_find_all(entries, './/name'))
    albums <- xml_text(xml_find_all(entries, './/album'))
    start_index <- as.integer(((page_index - 1) * 1000) + 1)
    end_index <- start_index + length(artists) - 1
    scrobbles[
      start_index:end_index,
      `:=`(date = dates, artist = artists, track = tracks, album = albums)
    ]
    #setTxtProgressBar(pb, getTxtProgressBar(pb) + 1L)
    incProgress(1/pages)
  }
  
  run_batch(url_list = lastfm_urls, indices = seq(pages), update_data = add_data)
  
  #remove empty rows
  empty_rows <- apply(scrobbles, 1, function(x) all(is.na(x)))
  scrobbles <- scrobbles[!empty_rows,]
  
  #handle missing values
  #assumes anything before 2000-01-01 to be error
  scrobbles[date < 946684800, date := NA_integer_]
  scrobbles[grepl("^\\s*$", album), album := NA_character_]
  
  #date formatting
  #last.fm returns GMT, this need to be set first, then optionally convertred
  class(scrobbles$date) <- c("POSIXt", "POSIXct")
  attr(scrobbles$date, "tzone") <- "GMT"
  if (timezone != 'GMT') {
    attr(scrobbles$date, "tzone") <- timezone
  }
  
  })
  #close(pb)
  return(scrobbles)
  }


get_tags <- function(artist_vector) {
  
  withProgress(message = "Getting Artist Tags", value = 0, {
  
  total <- length(artist_vector)
  
  #length of tags not known need to store list of data.tables and rbind later
  dt_list <- replicate(total, NA, simplify = FALSE)
  
  #get XML files
  artists_encoded <- sapply(artist_vector, function(x) URLencode(x, reserved = TRUE))
  lastfm_urls <- paste0(
    api_root,
    "artist.gettoptags&",
    "artist=",
    artists_encoded,
    "&autocorrect=0",
    "&api_key=",
    api_key
  )
  
  #pb <- txtProgressBar(min = 0, max = total, style = 3)
  add_data <- function(response){
    dt_index <- which(lastfm_urls == response$url)
    current_artist = artist_vector[dt_index]
    parsed_xml <- read_xml(parse_content(response))
    entries <- xml_find_all(parsed_xml, ".//tag")
    tags <- xml_text(xml_find_all(entries, './/name'))
    counts <- as.integer(xml_text(xml_find_all(entries, './/count')))
    tags_dt <- data.table(
      artist = rep(current_artist, length(tags)),
      tag = tags,
      tag_freq = counts
    )
    # if(tags_dt[, .N] == 0){
    #   warning(sprintf("Artist %s not found", current_artist))
    # }
    dt_list[[dt_index]] <<- tags_dt
    incProgress(1/length(batches))
    #setTxtProgressBar(pb, getTxtProgressBar(pb) + 1L)
  }
  
  # process data in 100-url batches
  all_indices <- 1:total
  batches <- split(all_indices, ceiling(seq_along(all_indices) / 100))
  for (i in 1:length(batches)) {
    current_batch <- batches[[i]]
    run_batch(url_list = lastfm_urls, indices = current_batch, update_data = add_data)
  }
  })
  #close(pb)
  return(rbindlist(dt_list))
}
  
  
  
  
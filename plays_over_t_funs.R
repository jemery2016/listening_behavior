################################################
################ PLAYS OVER T ##################
################################################

lower_to_na <- function(mat){
  mat[lower.tri(mat)] <- NA
  return(mat)
}

get_dist_plays <- function(dates, t){
  dist <- dates$date |> 
    Dist() |> 
    lower_to_na()
  return(
    apply(X = (dist <= t*86400),
          FUN  = sum,
          na.rm = T,
          MARGIN = 1) |> 
      max()
  )
}

get_plays_over_t_tracks <- function(hist, days){
  
  hist <- hist |>
    select(-album, -date_num)
  
  tracks <- hist |> 
    group_by(track, artist) |> 
    nest(dates = date) |> 
    as_tibble() |> 
    rowwise() |> 
    mutate(plays = nrow(dates)) |> 
    filter(plays > 2) 
  
  tracks$plays_over_t <- sapply(tracks$dates, get_dist_plays, t = days)
  
  tracks <- tracks |> 
    arrange(desc(plays_over_t)) |> 
    select(artist, track, plays_over_t, plays) 
  
  colnames(tracks) <- c("Artist",
                        "Track",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(tracks)
}

get_plays_over_t_albums <- function(hist, days){
  
  hist <- hist |> 
    select(artist, album, date)
  
  albums <- hist |> 
    group_by(album) |> 
    nest(dates = date) |> 
    as_tibble() |> 
    rowwise() |> 
    mutate(plays = nrow(dates)) |> 
    filter(plays > 2)
  
  albums$plays_over_t <- sapply(albums$dates, get_dist_plays, t = days)
  
  albums <- albums |> 
    arrange(desc(plays_over_t)) |> 
    select(artist, album, plays_over_t, plays)
  
  colnames(albums) <- c("Artist",
                        "Album",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(albums)
}

get_plays_over_t_artists <- function(hist, days){
  
  hist <- hist |> 
    select(artist, date)
  
  artists <- hist |> 
    group_by(artist) |> 
    nest(dates = date) |> 
    as_tibble() |> 
    rowwise() |> 
    mutate(plays = nrow(dates)) |> 
    filter(plays > 2)
  
  artists$plays_over_t <- sapply(artists$dates, get_dist_plays, t = days)
  
  artists <- artists |> 
    arrange(desc(plays_over_t)) |> 
    select(artist, plays_over_t, plays)
  
  colnames(artists) <- c("Artist",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(artists)
}


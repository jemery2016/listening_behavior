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

get_plays_over_t_tracks <- function(tracks, days){
  
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

get_plays_over_t_albums <- function(albums, days){
  
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

get_plays_over_t_artists <- function(artists, days){
  
  artists$plays_over_t <- sapply(artists$dates, get_dist_plays, t = days)
  
  artists <- artists |> 
    arrange(desc(plays_over_t)) |> 
    select(artist, plays_over_t, plays)
  
  colnames(artists) <- c("Artist",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(artists)
}


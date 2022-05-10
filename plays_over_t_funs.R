################################################
################ PLAYS OVER T ##################
################################################

lower_to_na <- function(mat){
  mat[lower.tri(mat)] <- NA
  return(mat)
}

get_dist <- function(dates){
  return(
    dates$date |>
      Dist() |> 
      lower_to_na()
  )
}

plays_over_t <- function(track_dist, t){
  return(
    apply(X = (track_dist <= t*86400),
          FUN  = sum,
          na.rm = T,
          MARGIN = 1) |> 
      max()
  )
}


get_plays_over_t_table <- function(hist, days){
  
  hist <- hist |>
    select(-album, -date_num)
  
  tracks <- hist |> 
    group_by(track, artist) |> 
    nest(dates = date) |> 
    as_tibble() |> 
    rowwise() |> 
    mutate(plays = nrow(dates)) |> 
    filter(plays > 1) 
  
  tracks$dist <- sapply(tracks$dates, get_dist)
  
  tracks$plays_over_t <- sapply(tracks$dist, plays_over_t, t = days)
  
  tracks <- tracks |> 
    arrange(desc(plays_over_t)) |> 
    select(artist, track, plays_over_t, plays) 
  
  colnames(tracks) <- c("Artist",
                        "Track",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(tracks)
}



#ref
hist <- lastfmR::get_scrobbles("eniiler", "EST") |> 
  mutate(date_num = ymd_hms(date) |> 
           as_date() |> 
           as.numeric()) |> 
  as_tibble()
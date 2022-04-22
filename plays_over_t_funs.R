################################################
################ PLAYS OVER T ##################
################################################

get_dist <- function(hist, track_name, artist_name){
  return(
    hist %>% 
      filter(track == track_name & artist == artist_name) %>% 
      pull(date) %>% 
      Dist() %>% 
      as.matrix()
  )
}

lower_to_na <- function(mat){
  mat[lower.tri(mat)] <- NA
  return(mat)
}

plays_over_t <- function(track_dist, t){
  return(
    max(
      apply(X = (track_dist <= t*86400),
            FUN  = sum,
            na.rm = T,
            MARGIN = 1)
    )
  )
}

get_plays_over_t_table <- function(hist, days){
  
  tracks <- hist %>% 
    group_by(track, artist) %>% 
    summarize(plays = n(), .groups = "drop") %>% 
    arrange(desc(plays)) %>% 
    utils::head(200) %>% 
    as_tibble() %>% 
    rowwise() %>%  
    mutate(dist = list(get_dist(hist, track, artist))) %>% 
    ungroup()
  
  tracks$dist <- lapply(X = tracks$dist, FUN = lower_to_na)
  
  tracks <- tracks %>% 
    mutate(plays_over_t = sapply(X = tracks$dist, 
                                 FUN = plays_over_t, 
                                 t = days)) %>% 
    select(-dist) %>% 
    arrange(desc(plays_over_t)) %>% 
    relocate(artist, track, plays_over_t, plays)
  
  colnames(tracks) <- c("Artist",
                        "Track",
                        paste("Max Plays Over", days, "Days"),
                        "Total Plays")
  
  return(tracks)
}
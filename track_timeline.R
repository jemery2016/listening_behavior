##################################################
################ TRACK TIMELINE ##################
##################################################

get_track_timeline <- function(hist, input_artist){

  if(input_artist == ""){
    return(NULL)
  }
  else {
    #filtering down to a single artist
    artist_hist <- hist %>% 
      filter(artist == input_artist) %>% 
      as_tibble()
    
    #getting total plays and play ranking
    artist_track_rank <- artist_hist %>% 
      group_by(artist, track) %>% 
      summarize(plays = n()) %>% 
      arrange(desc(plays))
    artist_track_rank$play_rank <- 1:nrow(artist_track_rank)
    
    #filtering to top 15 tracks by plays and getting play number
    artist_hist <- artist_hist %>% 
      left_join(artist_track_rank) %>% 
      filter(play_rank <= 15) %>% 
      mutate(date_utc = as.numeric(date)) %>% 
      group_by(track) %>% 
      mutate(play_num = rank(date))
    
    data <- highlight_key(artist_hist, ~track)
    
    p <- ggplot(data, aes(x = date, y = play_num, color = track)) + 
      geom_point(aes(text = paste(
        "Track:", track,
        "\nPlay Count:", play_num,
        "\nDate:", date))) + 
      geom_line() + 
      labs(x = "Date",
           y = "Play Count",
           title = paste("Timeline of Plays for", input_artist),
           color = "Track") + 
      scale_y_continuous(breaks = pretty_breaks())
    #scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
    
    pp <- ggplotly(p, tooltip = "text")
    
    return(highlight(pp, on = "plotly_click", off = "plotly_relayout"))
  }
}
##################################################
################ ALBUM TIMELINE ##################
##################################################

get_album_timeline <- function(hist, input_artist){
  
  if(input_artist == ""){
    return(NULL)
  } 
  else{
    #filtering down to artist
    artist <- hist %>% 
      filter(artist == input_artist) %>% 
      as_tibble()
    
    #getting album track counts
    album_track_count <- artist %>% 
      group_by(album) %>% 
      summarize(track_count = n_distinct(track))
    
    #getting play number after filtering albums w/less then 5 tracks
    artist_albums <- artist %>% 
      left_join(album_track_count, by = "album") %>% 
      filter(track_count > 4) %>% 
      group_by(album) %>% 
      mutate(play_num = rank(date)) %>% 
      as_tibble()
    
    if(nrow(artist_albums) == 0){
      return(NULL)
    } else {
      data <- highlight_key(artist_albums, ~album)
      
      p <- ggplot(data, aes(x = date, y = play_num, color = album)) + 
        geom_point(aes(text = paste(
          "Album:", album,
          "\nPlay Count:", play_num,
          "\nDate:", date)),
          size = 1) + 
        geom_line() + 
        labs(x = "Date",
             y = "Play Count",
             title = paste("Timeline of Plays for", input_artist),
             color = "Album") + 
        scale_y_continuous(breaks = pretty_breaks())
      
      pp <- ggplotly(p, tooltip = "text")
      
      return(highlight(pp, on = "plotly_click", off = "plotly_relayout"))
    }
  }
}
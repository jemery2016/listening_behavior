##################################################
################ TRACK TIMELINE ##################
##################################################

get_track_timeline <- function(tracks, input_artist){

  if(input_artist == ""){
    return(NULL)
  }
  else {
    #filtering down to a single artist and getting play ranks
    artist_tracks <- tracks |>  
      filter(artist == input_artist) |> 
      as_tibble()
    artist_tracks$play_rank <- artist_tracks$plays |> 
      desc() |> 
      row_number()
    
    #filtering to top 15 tracks by plays and getting play number
    artist_hist <- artist_tracks |> 
      filter(play_rank <= 15) |> 
      unnest_longer(dates) |> 
      mutate(dates = dates$date) |>
      group_by(track) |> 
      mutate(play_num = rank(dates))
    
    data <- highlight_key(artist_hist, ~track)
    
    p <- ggplot(data, aes(x = dates, y = play_num, color = track)) + 
      geom_point(aes(text = paste(
        "Track:", track,
        "\nPlay Count:", play_num,
        "\nDate:", dates))) + 
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
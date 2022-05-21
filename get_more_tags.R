get_more_tags <- function(artist_node_df){
  artist_node_df$tag <- NA
  splits <- split(artist_node_df$artist, 
                  ceiling(seq_along(artist_node_df$artist)/1000))
  
  for(i in seq_along(splits)){
    if(i != 1) Sys.sleep(60)
    tags <- lastfmR::get_tags(splits[[i]]) |> 
      group_by(artist) |> 
      summarize(tag = first(tag)) |> 
      as_tibble()
    
    artist_node_df <- artist_node_df |> 
      left_join(tags, by = "artist") |> 
      mutate(tag = coalesce(tag.x, tag.y)) |> 
      select(-tag.x, -tag.y)
    
    print(sum(is.na(artist_node_df$tag)))
  }
  return(artist_node_df)
}

#reference code  
hist <- lastfmR::get_scrobbles("eniiler", timezone = "EST") |> 
  mutate(date_num = ymd_hms(date) |> 
           as_date() |> 
           as.numeric()) |> 
  as_tibble()

artist_node_df <- hist |>
  group_by(artist) |> 
  summarize(plays = n()) |> 
  as_tibble()


lib_info <- lastfmR::get_library_info("eniiler")

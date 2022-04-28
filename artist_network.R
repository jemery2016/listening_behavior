#PAST 30 DAYS
get_artist_network <- function(hist, seed = 1){

  #getting edges
  artist_edge_df <- hist |> 
    select(date, date_num, artist) |> 
    filter(date_num >= max(date_num) - 30) |> 
    mutate(next_date = lag(date),
           next_date_num = lag(date_num),
           next_artist = lag(artist)) |> 
    filter(artist != next_artist) |> 
    group_by(artist, next_artist) |> 
    summarise(edge_count = n(),
              first_date = first(date),
              .groups = "drop") |> 
    arrange(desc(first_date)) |> 
    as_tibble()
  
  #getting nodes
  artist_node_df <- hist |>
    filter(date_num >= max(date_num) - 30) |> 
    group_by(artist) |> 
    summarize(plays = n()) |> 
    as_tibble()
  
  #getting tags 
  tags <- get_tags(artist_node_df$artist) |>
    group_by(artist) |>
    summarize(tag = first(tag)) |>
    as_tibble()
  
  #adding tags to nodes
  artist_node_df <- artist_node_df |> 
    left_join(tags, by = "artist")
  
  #prepping for visNetwork
  edges <- artist_edge_df |> 
    rename(from = artist,
           to = next_artist,
           width = edge_count) |> 
    mutate(arrows = "to",
           color.opacity = 0.4) |> 
    select(-first_date)
  
  nodes <- artist_node_df |> 
    rename(value = plays,
           label = artist,
           group = tag) |> 
    mutate(id = label,
           title = paste0("<p>", id, 
                          "<br> Tag: ", group, 
                          "<br> Plays:", value,
                          "</p>"))
  
  vis_net <- visNetwork(nodes, edges) |> 
    visIgraphLayout(physics = T) |>
    visNodes(labelHighlightBold = T) |> 
    visOptions(highlightNearest = list(enabled = T, degree = 1)) |> 
    visLayout(randomSeed = seed)
  
  return(vis_net)
}


#REFERENCE CODE
hist <- lastfmR::get_scrobbles("jemery2016", "EST")
hist <- hist |> 
  mutate(date_num = ymd_hms(date) |> 
           as_date() |> 
           as.numeric()) |> 
  as_tibble()

artist_edge_df <- hist |> 
  select(date, date_num, artist) |> 
  #filter(date_num >= max(date_num) - 30) |> 
  mutate(next_date = lag(date),
         next_date_num = lag(date_num),
         next_artist = lag(artist)) |> 
  filter(artist != next_artist) |> 
  group_by(artist, next_artist) |> 
  summarise(edge_count = n(),
            first_date = first(date),
            .groups = "drop") |> 
  arrange(desc(first_date)) |> 
  as_tibble()

artist_node_df <- hist |>
  #filter(date_num >= max(date_num) - 30) |> 
  group_by(artist) |> 
  summarize(plays = n()) |> 
  as_tibble()

get_more_tags <- function(artist_node_df){
  artist_node_df$tag <- NA
  new_count <- 0
  miss_index <- 1:nrow(artist_node_df)
  continue <- TRUE
  while(continue){
    Sys.sleep(10)
    old_count <- new_count
    
    tags <- lastfmR::get_tags(artist_node_df$artist[miss_index]) |>
      group_by(artist) |>
      summarize(tag = first(tag)) |>
      as_tibble()
    
    artist_node_df <- artist_node_df |> 
      left_join(tags, by = "artist") |> 
      mutate(tag = coalesce(tag.x, tag.y)) |> 
      select(-tag.x, -tag.y)
    
    miss_index <- which(is.na(artist_node_df$tag))
    new_count <- length(miss_index)
    continue <- new_count != old_count
    print(old_count); print(new_count)
  }
  return(artist_node_df)
}

test <- get_more_tags(artist_node_df)

#getting tags 
tags <- lastfmR::get_tags(artist_node_df$artist) |>
  group_by(artist) |>
  summarize(tag = first(tag)) |>
  as_tibble()

artist_node_df <- artist_node_df |> 
  left_join(tags, by = "artist")

missing_tags <- artist_node_df |> 
  filter(is.na(tag))

tags <- lastfmR::get_tags(missing_tags$artist) |> 
  group_by(artist) |>
  summarize(tag = first(tag)) |>
  as_tibble()
 

missing_tags |> left_join(tags, by = "artist") |> View()


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
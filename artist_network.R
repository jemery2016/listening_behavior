library(tidyverse)
library(lastfmR)
library(data.table)
library(dtplyr)
library(igraph)
library(visNetwork)


get_artist_network <- function(hist, tags, start_date, end_date, seed = 1){
  
  #getting edge list
  artist_edge_df <- hist |> 
    select(date, date_num, artist) |> 
    mutate(next_date = lag(date),
           next_date_num = lag(date_num),
           next_artist = lag(artist)) |> 
    filter(artist != next_artist) |> 
    filter(date_num >= start_date & date_num <= end_date) |> 
    group_by(artist, next_artist) |> 
    summarise(edge_count = n(),
              first_date = first(date),
              .groups = "drop") |> 
    arrange(desc(first_date)) |> 
    as_tibble()
  
  #getting nodes and adding tags
  artist_node_df <- hist |> 
    filter(date_num >= start_date & date_num <= end_date) |> 
    group_by(artist) |> 
    summarize(plays = n()) |> 
    left_join(tags, by = "artist") |> 
    as_tibble()
  
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
           title = paste0("<p>", id, "<br> Tag: ", group, "<br> Plays:", value, "</p>"))
  
  vis_net <- visNetwork(nodes, edges) |> 
    visIgraphLayout(physics = F) |>
    visNodes(labelHighlightBold = T) |> 
    visOptions(highlightNearest = list(enabled = T, degree = 1)) |> 
    visLayout(randomSeed = seed)

  return(vis_net)
}

#rEFERENCE CODE
# hist <- lastfmR::get_scrobbles("johndorito") |>
#   mutate(date_num = ymd_hms(date) |>
#            as_date() |>
#            as.numeric()) |>
#   as_tibble()
# 
# tags <- lastfmR::get_tags(unique(hist$artist)) |>
#   group_by(artist) |>
#   summarize(tag = first(tag)) |>
#   as_tibble()
# 
# min_date <- min(hist$date_num)
# 
# max_date <- max(hist$date_num) 
# 
# start_date <- min_date
# end_date <- min_date + 30
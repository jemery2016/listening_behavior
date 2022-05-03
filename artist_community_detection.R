
#COMMUNITY DETECTION
library(igraph)
library(ggraph)
library(tidyverse)
library(lastfmR)
library(lubridate)
library(beepr)
library(visNetwork)

#getting listening history
hist <- lastfmR::get_scrobbles("jemery2016", "EST") |> 
  mutate(date_num = ymd_hms(date) |> 
           as_date() |> 
           as.numeric()) |> 
  as_tibble()

#artists and plays
artist <- hist |> 
  group_by(artist) |> 
  summarize(plays = n()) |> 
  as_tibble()

#getting artist edges
artist_edge_df <- hist |> 
  select(date, date_num, artist) |> 
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

#preping for igraph
edges <- artist_edge_df |> 
  rename(weight = edge_count,
         source = artist,
         target = next_artist) |> 
  select(-first_date)

#creating igraph object
G <- graph_from_data_frame(edges)
#running community detection
#com <- cluster_infomap(G)
com <- cluster_walktrap(G)

#creating df of artists, communities, and plays
coms_df <- tibble(artist = com$names, com = com$membership) |> 
  left_join(artist, by = 'artist')

#plays by community
com_plays <- coms_df |> 
  group_by(com) |> 
  summarize(com_plays = sum(plays))

#creating new edge df with just communities
edges_com <- edges |> 
  left_join(coms_df, by = c("source" = "artist")) |> 
  left_join(coms_df, by = c("target" = "artist")) |> 
  select(-source, -target) |> 
  rename(source = com.x,
         target = com.y) |> 
  filter(source != target) |> 
  group_by(source, target) |> 
  summarize(weight = sum(weight), .groups = "drop")

nodes_com <- tibble(id = unique(edges_com$source),
                        in_degree = count(edges_com, target)$n) |> 
  mutate(value = com_plays$com_plays)


visNetwork(nodes_com, rename(edges_com, from = source, to = target)) |> 
  visIgraphLayout()


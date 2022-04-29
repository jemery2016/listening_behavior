library(ggraph)

#COMMUNITY DETECTION

#getting listening history
hist <- lastfmR::get_scrobbles("jemery2016", "EST") |> 
  mutate(date_num = ymd_hms(date) |> 
           as_date() |> 
           as.numeric()) |> 
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
comms <- cluster_infomap(G)

#creating df of artists and their communities
comms_df <- tibble(artist = comms$names, comm = comms$membership)

#creating new edge df with just communities
edges_w_comms <- edges |> 
  left_join(comms_df, by = c("source" = "artist")) |> 
  left_join(comms_df, by = c("target" = "artist")) |> 
  select(-source, -target) |> 
  rename(source = comm.x,
         target = comm.y) |> 
  filter(source != target) |> 
  group_by(source, target) |> 
  summarize(weight = sum(weight), .groups = "drop")

#new igraph from community graph
G_comm <- graph_from_data_frame(edges_w_comms)


lay <- create_layout(G_comm, layout = "kk")
ggraph(lay) |> 
  geom_node_point() |> 
  geom_edge_link()
library(shinyWidgets)
library(shiny)
library(tidyverse)
library(dtplyr)
library(data.table)
library(Rfast)
library(lastfmR)
library(scales)
library(plotly)
library(bslib)
library(shinydashboard)
library(lubridate)
library(shinyjs)
library(visNetwork)
library(igraph)
source("lastfmR_funs_wbar.R")
source("plays_over_t_funs.R")
source("track_timeline.R")
source("album_timeline.R")
source("artist_network.R")

server <- function(input, output, session) {
  
  #reference code
  # hist <- lastfmR::get_scrobbles("eniiler", timezone = "EST") |>
  #   mutate(date_num = ymd_hms(date) |>
  #            as_date() |>
  #            as.numeric()) |>
  #   as_tibble()
  # 
  # tracks <- hist |>
  #   select(artist, track, date) |>
  #   group_by(track, artist) |>
  #   nest(dates = date) |>
  #   as_tibble() |>
  #   rowwise() |>
  #   mutate(plays = nrow(dates))
  # 
  # albums <- hist |>
  #   select(artist, album, date) |>
  #   group_by(album, artist) |>
  #   nest(dates = date) |>
  #   as_tibble() |>
  #   rowwise() |>
  #   mutate(plays = nrow(dates))
  # 
  # artists <- hist |>
  #   select(artist, date) |>
  #   group_by(artist) |>
  #   nest(dates = date) |>
  #   as_tibble() |>
  #   rowwise() |>
  #   mutate(plays = nrow(dates))
  
  
  #getting listening history
  hist <- eventReactive(input$get_hist, {
    ret <- tryCatch(
     get_scrobbles(input$user_id, timezone = input$timezone) |> 
        mutate(date_num = ymd_hms(date) |> 
                 as_date() |> 
                 as.numeric()) |> 
        as_tibble(),
      error = function(e) conditionMessage(e))
    validate(
      need(typeof(ret) != "character", "Invalid Username")
    )
    ret
   })
  
  tracks <- eventReactive(input$get_hist, {
    hist() |> 
      select(artist, track, date) |> 
      group_by(track, artist) |> 
      nest(dates = date) |> 
      as_tibble() |> 
      rowwise() |> 
      mutate(plays = nrow(dates))
  })
  
  albums <- eventReactive(input$get_hist, {
    hist() |> 
      select(artist, album, date) |> 
      group_by(album, artist) |> 
      nest(dates = date) |> 
      as_tibble() |> 
      rowwise() |> 
      mutate(plays = nrow(dates))
  })
  
  artists <- eventReactive(input$get_hist, {
    hist() |> 
      select(artist, date) |> 
      group_by(artist) |> 
      nest(dates = date) |> 
      as_tibble() |> 
      rowwise() |> 
      mutate(plays = nrow(dates))
  })
  
  ################################################
  ################ PLAYS OVER T ##################
  ################################################
  
  output$plays_over_t_tracks <- DT::renderDataTable(
    get_plays_over_t_tracks(tracks(), input$days)
  )
  
  output$plays_over_t_albums <- DT::renderDataTable(
    get_plays_over_t_albums(albums(), input$days)
  )
  
  output$plays_over_t_artists <- DT::renderDataTable(
    get_plays_over_t_artists(artists(), input$days)
  )

  ##################################################
  ################ TRACK TIMELINE ##################
  ##################################################
  artist_list <- reactive(unique(hist()$artist))
  
  observe({
    updateSelectInput(inputId = "artist", choices = artist_list())
  })
  
  output$plot_tracks <- renderPlotly(
    get_track_timeline(tracks(), input$artist)
  )
  
  ##################################################
  ################ ALBUM TIMELINE ##################
  ##################################################
  
  observe({
    updateSelectInput(inputId = "artist1", choices = artist_list())
  })
  
  output$plot_albums <- renderPlotly(
    get_album_timeline(hist(), input$artist1)
  )
  
  ##################################################
  ################ ARTIST NETWORK ##################
  ##################################################

  output$plot_artist_network <- renderVisNetwork(get_artist_network(hist()))
}

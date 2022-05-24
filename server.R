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
library(promises)
library(future)
plan(multisession)
source("lastfmR_funs_wbar.R")
source("plays_over_t_funs.R")
source("track_timeline.R")
source("album_timeline.R")
source("artist_network.R")

#reference code
# hist <- future_promise(
#   lastfmR::get_scrobbles("eniiler", "EST"),
#   packages = "lastfmR"
#   )
# 
# p_over_t <- hist %...>%
#   get_plays_over_t_tracks(1)

server <- function(input, output, session) {
  
  #getting listening history async
  hist <- eventReactive(input$get_hist, {
    user_id <- input$user_id
    timezone <- input$timezone
    future_promise({
      lastfmR::get_scrobbles(user_id, timezone) %...>%
        as_tibble()},
      packages = "lastfmR"
    )
  })
  
  #getting listening history
  # hist <- eventReactive(input$get_hist, {
  #   ret <- tryCatch(
  #    get_scrobbles(input$user_id, timezone = input$timezone) |> 
  #       mutate(date_num = ymd_hms(date) |> 
  #                as_date() |> 
  #                as.numeric()) |> 
  #       as_tibble(),
  #     error = function(e) conditionMessage(e))
  #   validate(
  #     need(typeof(ret) != "character", "Invalid Username or API Crash")
  #   )
  #   ret
  #  })
  
  ################################################
  ################ PLAYS OVER T ##################
  ################################################
  
  output$plays_over_t_tracks <- DT::renderDataTable({
    days <- input$days
    promise_all(hist = hist()) %...>% 
      with(get_plays_over_t_tracks(hist, days))
  })

  # output$plays_over_t_tracks <- DT::renderDataTable(
  #   get_plays_over_t_tracks(hist(), input$days)
  # )
  
  output$plays_over_t_albums <- DT::renderDataTable(
    get_plays_over_t_albums(hist(), input$days)
  )
  
  output$plays_over_t_artists <- DT::renderDataTable(
    get_plays_over_t_artists(hist(), input$days)
  )

  ##################################################
  ################ TRACK TIMELINE ##################
  ##################################################
  artist_list <- reactive(unique(hist()$artist))
  
  observe({
    updateSelectInput(inputId = "artist", choices = artist_list())
  })
  
  output$plot_tracks <- renderPlotly(
    get_track_timeline(hist(), input$artist)
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

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


server <- function(input, output, session) {
  
  #getting listening history async
  hist <- eventReactive(input$get_hist, {
    user_id <- input$user_id
    timezone <- input$timezone
    p <- Progress$new()
    #p$set(value = NULL, message = "Getting Listening History...")
    future_promise(
      lastfmR::get_scrobbles(user_id, timezone),
      packages = "lastfmR"
    ) %...>%
      mutate(date_num = ymd_hms(date) |> 
               as_date() |> 
               as.numeric()) %...>%
      as_tibble()
    #finally(~p$close())
  })
  
  ################################################
  ################ PLAYS OVER T ##################
  ################################################
  
  output$plays_over_t_tracks <- DT::renderDataTable(
    hist() %...>%
      get_plays_over_t_tracks(input$days)
  )
  
  output$plays_over_t_albums <- DT::renderDataTable(
    hist() %...>%
      get_plays_over_t_albums(input$days)
  )
  
  output$plays_over_t_artists <- DT::renderDataTable(
    hist() %...>%
      get_plays_over_t_artists(input$days)
  )

  ##################################################
  ################ TRACK TIMELINE ##################
  ##################################################
  observe({
    hist() %...>% {
      hist <- .
      hist$artist %>%
        unique() %>%
        updateSelectInput(inputId = "artist", choices = .)
    }
  })
  
  output$plot_tracks <- renderPlotly(
    hist() %...>%
      get_track_timeline(input$artist)
  )
  
  ##################################################
  ################ ALBUM TIMELINE ##################
  ##################################################
  observe({
    hist() %...>% {
      hist <- .
      hist$artist %>%
        unique() %>%
        updateSelectInput(inputId = "artist1", choices = .)
    }
  })
  
  output$plot_albums <- renderPlotly(
    hist() %...>%
      get_album_timeline(input$artist1)
  )
  
  ##################################################
  ################ ARTIST NETWORK ##################
  ##################################################

  output$plot_artist_network <- renderVisNetwork(
    hist() %...>%
      get_artist_network()
    )
}

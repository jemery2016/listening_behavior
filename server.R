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
source("lastfmR_funs_wbar.R")
source("plays_over_t_funs.R")
source("track_timeline.R")
source("album_timeline.R")
source("artist_network.R")

server <- function(input, output, session) {
  
  #getting listening history
  hist <- eventReactive(
    input$get_hist, 
    get_scrobbles(input$user_id, timezone = input$timezone) |> 
      mutate(date_num = ymd_hms(date) |> 
               as_date() |> 
               as.numeric()) |> 
      as_tibble()
  )
  
  ################################################
  ################ PLAYS OVER T ##################
  ################################################
  
  output$plays_over_t_table <- DT::renderDataTable(
    get_plays_over_t_table(hist(), input$days)
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

  min_date <- min(hist()$date_num) |> 
    reactive()
  
  max_date <- max(hist()$date_num) |> 
    reactive()
  
  output$artist_slider <- renderUI({
    noUiSliderInput("artist_slider",
                    label = character(0),
                    min = min_date(),
                    max = max_date(),
                    value = c(min_date(), min_date()+100),
                    behaviour = "drag",
                    #limit = 30,
                    color = "green",
                    width = "100%")
  })
  
  tags <- reactive({
    get_tags(artist_list()) |>
      group_by(artist) |>
      summarize(tag = first(tag)) |>
      as_tibble()
  })
  
  artist_slider <- reactive(input$artist_slider) |> 
    debounce(1000)

  output$plot_artist_network <- renderVisNetwork({
    get_artist_network(hist(),
                       tags(),
                       artist_slider()[1],
                       artist_slider()[2])
  })
}

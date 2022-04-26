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

#header with the buttons on the right side
header <- dashboardHeader(title = "Listening Behavior",
                          dropdownMenu(
                            type = "notifications",
                            icon = icon("heart", lib = "glyphicon"),
                            badgeStatus = NULL,
                            headerText = "Thanks to:",
                            notificationItem(
                              text = "Piotr Patrzyk",
                              href = "https://github.com/ppatrzyk/lastfmR",
                              icon = icon("book", lib = "glyphicon")
                            )))

#sidebar buttons and list hist inputs
sidebar <- dashboardSidebar(
  textInput(inputId = "user_id", label = "Last.fm Username"),
  selectInput(inputId = "timezone", label = "Timezone",
              choices = OlsonNames(),
              selected = "EST"),
  actionButton(inputId = "get_hist", label = "Get Listening History"),
  sidebarMenu(
    menuItem("Max Plays Over Time", tabName = "plays_over_t_tab"),
    menuItem("Track Timeline", tabName = "track_timeline"),
    menuItem("Album Timeline", tabName = "album_timeline"),
    menuItem("Artist Network", tabName = "artist_network")
  )
)

#body with outputs
body <- dashboardBody(tabItems(
  tabItem(tabName = "plays_over_t_tab",
          fluidRow(box(
            shiny::sliderInput(
              inputId = "days",
              label = "Time Range in Days",
              min = 1,
              max = 30,
              value = 1),
            width = 6,
            status = "warning"
            )),
          fluidRow(box(
            DT::dataTableOutput("plays_over_t_table"),
            width = 6,
            status = "primary"
          ))),
  
  tabItem(tabName = "track_timeline",
          fluidRow(box(
            selectInput("artist", label = "Artist", ""),
            status = "warning"
          ),
          box(title = "Plots are Interactive!",
              width = 3,
            "Click a point to highlight track timeline", br(), 
            "Double click anywhere to reset", br(),
            "Other tools shown in options bar"
          )),
          fluidRow(box(
            plotlyOutput(
              outputId = "plot_tracks",
              height = "700px"),
            width = 12,
            status = "primary"
          ))),
  
  tabItem(tabName = "album_timeline",
          fluidRow(box(
            selectInput("artist1", label = "Artist", ""),
            status = "warning"
          ),
          box(title = "Plots are Interactive!",
              width = 3,
              "Click a point to highlight album timeline", br(), 
              "Double click anywhere to reset", br(),
              "Other tools shown in options bar"
          )),
          fluidRow(box(
            plotlyOutput(
              outputId = "plot_albums",
              height = "700px"),
              width = NULL,
              status = "primary"
            ))),
  
  tabItem(tabName = "artist_network",
          fluidRow(
            box(
              title = "Artist Network Date Range",
              uiOutput("artist_slider"),
              status = "warning",
              width = 12,
              height = "150px",
            )),
          fluidRow(
            box(
              visNetworkOutput(outputId = "plot_artist_network",
                               height = "700px"),
              width = 12,
              status = "primary"
            )
          ))
            #trying vertical slider
            # box(
            #   visNetworkOutput(outputId = "plot_artist_network",
            #                    height = "700px"),
            #   width = 9,
            #   status = "primary"
            # )
      
))
  
# Put them together into a dashboardPage
dashboardPage(
  skin = "green",
  header,
  sidebar,
  body
)


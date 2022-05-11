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
          fluidRow(
            column(
              width = 3,
              box(title = "Welcome!",
              "Enter your last.fm username and click \"Get Listening History\" to begin. \
              If your listening history is large give it some time to gather the data. \
              If you get disconnected from server please try again. If it continues to be \
              disconnected, try pausing music while data is being collected.",
              width = NULL)
              ),
            column(
              width = 6,
              box(title = "Time Range in Days",
                shiny::sliderInput(
                  inputId = "days",
                  label = NULL,
                  min = 1,
                  max = 30,
                  value = 1),
                status = "warning",
                width = NULL),
              tabBox(title = "Top Played in Time Range",
                     id = "tabset1",
                     width = NULL,
                     #status = "primary",
                     tabPanel("Tracks",
                              DT::dataTableOutput("plays_over_t_tracks")),
                     tabPanel("Albums",
                              DT::dataTableOutput("plays_over_t_albums")),
                     tabPanel("Artists",
                              DT::dataTableOutput("plays_over_t_artists")))
          ))), 
  
  tabItem(tabName = "track_timeline",
          fluidRow(box(title = "Select Artist",
            selectInput("artist", label = NULL, ""),
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
          fluidRow(box(title = "Select Artist",
            selectInput("artist1", label = NULL, ""),
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
            box(title = "Plots are Interactive!",
                width = 4,
                "Hover over nodes to see plays and tags. Click a node to higlight its neighbors."),
            box(title = "Artist Network from Past 30 Days",
              visNetworkOutput(outputId = "plot_artist_network",
                               height = "800px"),
              width = 12,
              status = "primary"
            )
          ))
))
  
# Put them together into a dashboardPage
dashboardPage(
  skin = "green",
  header,
  sidebar,
  body
)


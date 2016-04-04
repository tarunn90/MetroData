library(shiny)
library(shinyapps)
library(ggplot2)
library(dplyr)

shinyUI(fluidPage(
  
  titlePanel("Metro ridership by line, station, and time of day"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "line_chosen", 
                  label = "Choose line:",
                  choices = c("Red", "Blue", "Orange", "Green", "Yellow", "Silver")
      ),
      sliderInput(inputId = "time_of_day", 
                  label = "Time of day:", 
                  min = Sys.time() - 120,
                  max = Sys.time() + 120,
                  step = 30,
                  value = Sys.time(),
                  animate = animationOptions(interval = 300, loop = T)
      )
    ),
    mainPanel(
    )
  )
  
))
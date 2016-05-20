
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("title panel"), 
  
  fluidRow(
    
    
    column(5,
           fileInput("files", "File input", multiple=TRUE,accept=c('text/csv', 
                                                                   'text/comma-separated-values,
                                                                   text/plain', 
                                                                   '.csv'))
    ),
    
    column(3, 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets."))
    
  ),
  
  fluidRow(
    uiOutput("text1")
  ),
  
  

  
  selectInput(inputId = "n_breaks",
              label = "Number of bins in histogram (approximate):",
              choices = c(10, 20, 35, 50),
              selected = 20),
  
  checkboxInput(inputId = "individual_obs",
                label = strong("Show individual observations"),
                value = FALSE),
  
  checkboxInput(inputId = "density",
                label = strong("Show density estimate"),
                value = FALSE),
  
  plotOutput(outputId = "main_plot", height = "300px"),
  
  # Display this only if the density is shown
  conditionalPanel(condition = "input.density == true",
                   sliderInput(inputId = "bw_adjust",
                               label = "Bandwidth adjustment:",
                               min = 0.2, max = 2, value = 1, step = 0.2)
  )
  
))


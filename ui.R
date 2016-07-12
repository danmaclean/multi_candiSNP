
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("CandiSNP Jar"), 
  
  fluidRow(
    
    
    column(5,
           fileInput("files", "File input", multiple=TRUE,accept=c('text/csv', 
                                                                   'text/comma-separated-values,
                                                                   text/plain', 
                                                                   '.csv'))
    ),
    
    column(3, 
           helpText("Plots and filters SNPs from multiple experiments",
                    "annotated on the",
                    "Arabidopsis TAIR 10 genome with CandiSNP."),
           
           helpText("To use:"),
           helpText("1. Select as many CandiSNP input",
                    " or output files to load as you like."
                    ),
           helpText("2. Give each file a unique tag to identify them in the outputs."
           ),
           helpText("3. Select the view options as appropriate.")
           )
    
  ),
  
  fluidRow(
    uiOutput("text1")
  ),
  
  

  

  

sliderInput("range", "Allele Frequency Range:",
            min = 0, max = 1, value = c(0.75,0.9)),

  checkboxInput(inputId = "individual_obs",
                label = strong("Show individual observations"),
                value = TRUE),
 
  checkboxInput(inputId = "remove_centromere",
                label = strong("Remove centromere associated SNPs"),
                value = TRUE),

  
  

                   sliderInput(inputId = "bw_adjust",
                               label = "Bin width adjustment:",
                               min = 100000, max = 1000000, value = 2000000, step = 100000),


uiOutput("colour_choice"),

sliderInput(inputId = "spot_alpha",
            label = "spot opacity",
            min = 0.1, max = 1, value = 0.8, step = 0.1),


 plotOutput(outputId = "main_plot", height = "300px"),
 

 conditionalPanel(condition = "input.individual_obs == true",
                  plotOutput(outputId = "snp_plot", height="300px")
 
                )

))


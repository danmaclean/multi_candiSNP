
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
 

    output$text1 <- renderUI({
      file_count <- 0
      #cat(file=stderr(), "input obj files", length(input$files))
      if ( length(input$files) > 0) { file_count <- length(input$files$name) 
        lapply(1:file_count, function(i){
          tag <- paste0(input$files$name[i], "_tag")
          textInput(tag, paste0("Enter tag for file ", input$files$name[i]), value="Enter Tag..." )
        })
      }
    })
  

  filedata <- reactive({
    
    dataframe_list <- NULL
    if (length(input$files) > 0){
      dataframe_list <- lapply(1:length(input$files$name), function(i){
        df <- read.csv(input$files$datapath[i], header=TRUE, sep=",")
        tag <- paste0(input$files$name[i], "_tag")
        df$tag <- input[[tag]]
        df$tag <- as.factor(df$tag)
        df$Chr <- as.factor(df$Chr)
        return(df)
      })
    }
    file_data <- dataframe_list[[1]]
    if (length(dataframe_list) > 1){
      for (i in 2:length(dataframe_list) ){
        file_data <- rbind(file_data, dataframe_list[[i]])
      }
    }
    return(file_data)
  }) 
  
 

  output$main_plot <- renderPlot({

    file_data <- filedata()
    if ("Pos" %in% colnames(file_data) ){    
    hist(file_data$Pos,
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)",
         main = "Geyser eruption duration")
    
    if (input$individual_obs) {
      rug(file_data$Pos)
    }
    
    if (input$density) {
      dens <- density(file_data$Pos,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }
    }  
  })
  

  
})

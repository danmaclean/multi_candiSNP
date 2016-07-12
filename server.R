
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# TODO:
# Allele Freq Range Slider - DONE
# Colour by: Change, Effect, Is_CTGA, Is_synonymous - if present in input. 
# Shape by: Tag
# only call filedata if values entered in file selector
# filter out centromeres - DONE
# theme selector

library(shiny)
library(ggplot2)
library(jsonlite)
library(dplyr)


filterCentromeres <- function(df){
#  "1" : 15086545,
#  "2" : 3608429,
#  "3" : 14209452,
#  "4" : 3956521,
#  "5" : 11725524
  
  df <- df %>% filter( (Chr == "1" & (Pos < (15086545 - 500000) |  Pos > (15086545 + 500000) ) ) |  
                       (Chr == "2" & (Pos < (3608429 - 500000) |  Pos > (3608429 + 500000) ) ) |
                       (Chr == "3" & (Pos < (14209452 - 500000) |  Pos > (14209452 + 500000) ) ) |
                       (Chr == "4" & (Pos < (3956521 - 500000) |  Pos > (3956521 + 500000) ) ) |
                       (Chr == "5" & (Pos < (11725524 - 500000) |  Pos > (11725524 + 500000) ) )
                      )
  return(df)  
}



shinyServer(function(input, output) {
 

    output$text1 <- renderUI({
      file_count <- 0
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
        df <- read.csv(input$files$datapath[i], header=TRUE, sep=",",stringsAsFactors=TRUE, quote="\"")
        tag <- paste0(input$files$name[i], "_tag")
        df$tag <- input[[tag]]
        df$tag <- as.factor(df$tag)
        df$Chr <- as.factor(df$Chr)
        if("Is_CTGA" %in% colnames(df)){
          df$Is_CTGA <- as.factor(df$Is_CTGA)
        }
        if("Is_Synonymous" %in% colnames(df)){
          df$Is_Synonymous <- as.factor(df$Is_Synonymous)
        }
        if("Effect" %in% colnames(df)){
          df$Effect <- as.factor(df$Effect)
        }
        if("In_CDS" %in% colnames(df)){
          df$In_CDS <- as.factor(df$In_CDS)
        }
        return(df)
      })
    }
    file_data <- dataframe_list[[1]]
    if (length(dataframe_list) > 1){
      for (i in 2:length(dataframe_list) ){
        file_data <- rbind(file_data, dataframe_list[[i]])
      }
    }
    file_data <-  filter(file_data, Allele_Freq >= input$range[1], Allele_Freq <= input$range[2])
    if (input$remove_centromere){
      file_data <- filterCentromeres(file_data) 
    }
    
    
    
    return(file_data)
  }) 
  
  
  output$colour_choice <- renderUI({
    file_data <- filedata()
    categories <- character()
    headers = c('Is_CTGA', 'Is_synonymous', 'In_CDS', 'Effect')
    for(i in 1:length(colnames(file_data))){
      if (colnames(file_data)[i] %in% headers){
        categories <-  c(categories, colnames(file_data)[i] )
      }
    }
    radioButtons("colour_choice", "Choose Colouring Factor", categories)
  })
  


  output$main_plot <- renderPlot({

    file_data <- filedata() # only call if file is selected otherwise throws errors
    if ("Pos" %in% colnames(file_data) ){    

      p <- ggplot(file_data, aes(Pos, colour=tag, fill=tag)) + geom_freqpoly(binwidth=input$bw_adjust)  
      p <- p + facet_grid(.~Chr, space="free", scales="free")#, ncol=1)
      p <- p + scale_x_continuous(breaks = seq(0,30000000,10000000) ) #labels=c("5Mb","10Mb","15Mb","20Mb","25Mb","30Mb") )
      print(p)

    }  
  })
  
output$snp_plot <- renderPlot({
  file_data <- filedata()
  if ("Pos" %in% colnames(file_data) ){    
    cat(file=stderr(), "drawing histogram with", str(input$colour_choice), "bins\n")
    cat(file=stderr(), "drawing histogram with", str(input$spot_alpha), "bins\n")
    s <- ggplot(file_data, aes_string(x="Pos", y="Allele_Freq",colour=input$colour_choice, fill=input$colour_choice, shape="tag")) + geom_point( alpha = input$spot_alpha) + facet_grid(. ~ Chr, scales="free_x",space="free_x")
    s <- s + scale_x_continuous(breaks = seq(0,30000000,10000000) ) #labels=c("5Mb","10Mb","15Mb","20Mb","25Mb","30Mb") )
    print(s)
  }
})
  
})

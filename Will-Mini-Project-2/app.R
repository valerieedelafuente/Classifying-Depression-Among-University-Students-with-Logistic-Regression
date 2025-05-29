#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(bslib)
library(shinyBS)
data("mtcars")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mini Project 2"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
         
          selectInput(
            "plotType",
            "Select Plot Type:",
            choices = c("Scatter Plot", "Bar Graph",
                        "Box Plot", "Correlation Plot"),
            selected = 1
            ),
          
          bsTooltip("plotType", "Choose the type of plot to display",
                    placement = "right", trigger = "hover"),
          
          selectInput(
            "variable1",
            "Select X-axis Variable:",
            choices = names(mtcars),
            selected = 1
          ),
          
          selectInput(
            "variable2",
            "Select Optional Group/Y-axis Variable:",
            choices = names(mtcars),
            selected = 1
          ),
          
          selectInput(
            "color",
            "Select Plot Color",
            choices = c(Red = "red", Blue = "blue", Green = "green",
                        Orange = "orange", Black = "black", White = "white"),
            selected = 1
          ),
          
          sliderInput(
            "barWidth",
            "Set Bar Graph width:",
            min = 0,
            max = 1,
            value = 0.1
          ),
          
          checkboxInput(
            "namesCheckbox",
            "Show Car Names on Scatterplot",
            value = TRUE
          ),
          
          checkboxInput(
            "sumStatsCheckbox",
            "Show Summary Statistics for Current Variables",
            value = TRUE
          ),
          
       bsTooltip("sumStatsCheckbox", 
                  "Click to display the summary function output",
                  placement = "right", trigger = "hover")
          
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("summaryStats")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectedData <- mtcars
  selectedData$cyl <- as.factor(selectedData$cyl)
  selectedData$vs <- as.factor(selectedData$vs)
  selectedData$am <- as.factor(selectedData$am)
  selectedData$gear <- as.factor(selectedData$gear)
  selectedData$carb <- as.factor(selectedData$carb)

    output$distPlot <- renderPlot({
        
        if (input$plotType == "Scatter Plot") {
          
          if (input$namesCheckbox == TRUE) { 
            
            ggplot(selectedData, aes_string(x = input$variable1, y = input$variable2)) +
              geom_point(color = input$color, size = 1.5) +
              labs(title = paste("Scatter Plot of", input$variable1, "and", input$variable2),
                   x = input$variable1,
                   y = input$variable2) + 
              geom_text(aes(label = rownames(selectedData)), vjust = 1.5, check_overlap = TRUE, size = 3)
          } 
          
          else {
            ggplot(selectedData, aes_string(x = input$variable1, y = input$variable2)) +
              geom_point(color = input$color, size = 1.5) +
              labs(title = paste("Scatter Plot of", input$variable1, "and", input$variable2),
                   x = input$variable1,
                   y = input$variable2)
          }
        } 
        
        else if (input$plotType == "Bar Graph") {
          ggplot(selectedData, aes_string(x = input$variable1)) +
            geom_bar(stat = 'count', fill = input$color, color = "black",
                     width = input$barWidth) +
            labs(title = paste("Bar Graph of", input$variable1),
              x = input$variable1,
              y = "Count")
        }
        
        else if (input$plotType == "Box Plot") {
          ggplot(selectedData, aes_string(x = input$variable1, group = input$variable2)) +
            geom_boxplot(fill = input$color, color = "black") +
            labs(title = paste("Box Plot of", input$variable2, "by", input$variable1),
              x = input$variable1,
              y = input$variable2) 
        } 
        
        else if (input$plotType == "Correlation Plot") {
          corrplot(cor(selectedData[sapply(selectedData, is.numeric)]), diag = FALSE, type = "lower", method = "square",
                   addCoef.col = input$color)
        }
    })
    
    output$summaryStats <- renderPrint({
      if(input$sumStatsCheckbox){
        summary(mtcars[c(input$variable1,input$variable2)])
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

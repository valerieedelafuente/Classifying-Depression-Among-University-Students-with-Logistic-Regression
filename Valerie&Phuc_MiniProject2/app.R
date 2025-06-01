# app.R

# Load libraries
library(shiny)
library(tidyverse)
library(stringr)

# Load and clean dataset
data(infert)
infert_ds <- infert %>%
  rename(
    Education = education,
    Age = age,
    Parity = parity,
    InducedAbortions = induced,
    SpontaneousAbortions = spontaneous,
    CaseStatus = case,
    StratumID = stratum,
    PooledStratumID = pooled.stratum
  ) %>%
  mutate(
    Education = factor(str_remove(Education, "yrs"), levels = c("0-5", "6-11", "12+")),
    CaseStatus = factor(CaseStatus, levels = c(0, 1), labels = c("control", "case"))
  )

# Define UI
ui <- fluidPage(
  titlePanel("Infertility Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:",
                  choices = names(infert_ds)),
      
      uiOutput("plotTypeUI"),
      
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 5),
        checkboxInput("showDensity", "Overlay Density Curve", value = FALSE)
      ),
      
      conditionalPanel(
        condition = "input.plotType == 'Boxplot'",
        checkboxInput("showJitter", "Add Jittered Data Points", value = FALSE)
      ),
      
      checkboxInput("includeNA", "Include NA in Grouped Summary", value = FALSE),
      
      h4("Numeric Filters"),
      sliderInput("ageFilter", "Age Range:",
                  min(infert_ds$Age), max(infert_ds$Age),
                  value = c(min(infert_ds$Age), max(infert_ds$Age))),
      sliderInput("parityFilter", "Parity Range:",
                  min(infert_ds$Parity), max(infert_ds$Parity),
                  value = c(min(infert_ds$Parity), max(infert_ds$Parity))),
      sliderInput("inducedFilter", "Induced Abortions:",
                  min(infert_ds$InducedAbortions), max(infert_ds$InducedAbortions),
                  value = c(min(infert_ds$InducedAbortions), max(infert_ds$InducedAbortions))),
      sliderInput("spontaneousFilter", "Spontaneous Abortions:",
                  min(infert_ds$SpontaneousAbortions), max(infert_ds$SpontaneousAbortions),
                  value = c(min(infert_ds$SpontaneousAbortions), max(infert_ds$SpontaneousAbortions))),
      sliderInput("stratumFilter", "StratumID:",
                  min(infert_ds$StratumID), max(infert_ds$StratumID),
                  value = c(min(infert_ds$StratumID), max(infert_ds$StratumID))),
      sliderInput("pooledStratumFilter", "Pooled StratumID:",
                  min(infert_ds$PooledStratumID), max(infert_ds$PooledStratumID),
                  value = c(min(infert_ds$PooledStratumID), max(infert_ds$PooledStratumID))),
      
      selectInput("groupBy", "Group Summary By:",
                  choices = c("CaseStatus", "Education"))
    ),
    
    mainPanel(
      plotOutput("mainPlot", hover = "plot_hover"),
      verbatimTextOutput("hoverInfo"),
      h4("Summary"),
      verbatimTextOutput("summaryOutput"),
      h4("Grouped Summary"),
      tableOutput("groupedSummary")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically adjust plot options
  output$plotTypeUI <- renderUI({
    req(input$variable)
    var <- input$variable
    
    if (var %in% c("StratumID", "PooledStratumID")) {
      selectInput("plotType", "Select Plot Type:", choices = c("Histogram"))
    } else if (is.numeric(infert_ds[[var]])) {
      selectInput("plotType", "Select Plot Type:", choices = c("Histogram", "Boxplot"))
    } else {
      selectInput("plotType", "Select Plot Type:", choices = c("Bar Chart"))
    }
  })
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    infert_ds %>%
      filter(
        Age >= input$ageFilter[1], Age <= input$ageFilter[2],
        Parity >= input$parityFilter[1], Parity <= input$parityFilter[2],
        InducedAbortions >= input$inducedFilter[1], InducedAbortions <= input$inducedFilter[2],
        SpontaneousAbortions >= input$spontaneousFilter[1], SpontaneousAbortions <= input$spontaneousFilter[2],
        StratumID >= input$stratumFilter[1], StratumID <= input$stratumFilter[2],
        PooledStratumID >= input$pooledStratumFilter[1], PooledStratumID <= input$pooledStratumFilter[2]
      )
  })
  
  # Generate plots
  output$mainPlot <- renderPlot({
    req(input$variable, input$plotType)
    df <- filtered_data()
    var <- input$variable
    
    if (input$plotType == "Histogram") {
      p <- ggplot(df, aes_string(x = var)) +
        geom_histogram(bins = input$bins, fill = "lightpink", color = "black") +
        labs(title = paste("Histogram of", var), x = var, y = "Count") +
        theme_minimal()
      
      if (input$showDensity) {
        p <- p + geom_density(aes(y = ..count..), color = "blue", size = 1)
      }
      
      p
      
    } else if (input$plotType == "Boxplot") {
      p <- ggplot(df, aes_string(y = var)) +
        geom_boxplot(fill = "royalblue") +
        labs(title = paste("Boxplot of", var), y = var) +
        theme_minimal()
      
      if (input$showJitter) {
        p <- p + geom_jitter(width = 0.2, alpha = 0.4)
      }
      
      p
      
    } else if (input$plotType == "Bar Chart") {
      ggplot(df, aes_string(x = var)) +
        geom_bar(fill = "lavender", color = "black") +
        labs(title = paste("Bar Chart of", var), x = var, y = "Count") +
        theme_minimal()
    }
  })
  
  # Show hover info
  output$hoverInfo <- renderPrint({
    hover <- input$plot_hover
    if (!is.null(hover)) {
      cat("Hovered at:\n")
      cat("x:", round(hover$x, 2), "\n")
      cat("y:", round(hover$y, 2), "\n")
    } else {
      cat("Hover over the plot to see coordinates.")
    }
  })
  
  # Show summary statistics
  output$summaryOutput <- renderPrint({
    req(input$variable)
    summary(filtered_data()[[input$variable]])
  })
  
  # Grouped summary
  output$groupedSummary <- renderTable({
    df <- filtered_data()
    
    if (!input$includeNA) {
      df <- df %>% drop_na(CaseStatus, Education)
    }
    
    if (input$groupBy == "CaseStatus") {
      df %>%
        group_by(CaseStatus) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
    } else {
      df %>%
        group_by(Education) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
    }
  }, rownames = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)











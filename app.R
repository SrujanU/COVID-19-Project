
#The three extra elements I included: Picture, colors, and action button
# ui.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

covidData <- read.csv('US_AllStates_COVID.csv')
covidData <- covidData %>%
  mutate(Date = mdy(Date),  # Convert the date to a date object
         month_category = month(Date, label = TRUE))  # Extract the month and use labels


allowed_variables <- c("death", "total_tested", "daily_tested","daily_positive", "positive")

month_options <- c("Jan", "Feb", "Mar", "Apr", "May")

fill_color_options <- c("Red", "Blue", "Orange")

ui <- fluidPage(
  titlePanel("Shiny App with Univariate Plot"),
  sidebarLayout(
    sidebarPanel(
      img(src = "https://www.tdi.texas.gov/blog/artwork/covid19-home-test-homepage.jpg", width = "100%", height = "auto"),
      selectInput("variable", "Select Variable:", choices = allowed_variables),
      radioButtons("filter_type", "Filter by:", choices = c("Range", "Month"), selected = "Range"),
      uiOutput("range_ui"),  # Dynamic range slider
      helpText("You can filter the range for any selected variable with the slider. This will adjust the frequency in the graph."),
      selectInput("month_filter", "Select Month:", choices = month_options, selected = month_options[1]),
      helpText("Instead of using the range filter, you can sort by month. Make sure to switch to Month filter above before selecting a month."),
      selectInput("fill_color", "Select Fill Color:", choices = fill_color_options, selected = fill_color_options[1])
    ),
    mainPanel(
      verbatimTextOutput("app_explanation"),
      plotOutput("univariate_plot"),
      verbatimTextOutput("summary_text"),
      htmlOutput("image_citation")
    )
  )
)

# server.R

server <- function(input, output, session) {
  
  output$range_ui <- renderUI({
    variable_range <- range(covidData[[input$variable]], na.rm = TRUE)
    sliderInput("range", "Select Range:", min = variable_range[1], max = variable_range[2], value = variable_range)
  })
  
  binwidth_reactive <- reactive({
    selected_variable <- input$variable
    selected_filter <- input$filter_type
    
    if (selected_filter == "Range") {
      selected_range <- input$range
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData[[selected_variable]] >= selected_range[1] &
                          covidData[[selected_variable]] <= selected_range[2], ]
    } else {
      selected_month <- input$month_filter
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData$month_category == selected_month, ]
    }
    
    if (length(na.omit(data[[selected_variable]])) > 0) {
      suggested_binwidth <- (max(data[[selected_variable]], na.rm = TRUE) - 
                               min(data[[selected_variable]], na.rm = TRUE)) / 30
      
      return(max(suggested_binwidth, 0.1))
    } else {
      
      return(1)
    }
  })
  
  output$univariate_plot <- renderPlot({
    selected_variable <- input$variable
    selected_filter <- input$filter_type
    selected_fill_color <- tolower(input$fill_color)
    
    if (selected_filter == "Range") {
      selected_range <- input$range
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData[[selected_variable]] >= selected_range[1] &
                          covidData[[selected_variable]] <= selected_range[2], ]
    } else {
      selected_month <- input$month_filter
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData$month_category == selected_month, ]
    }
    
    ggplot(data, aes(x = .data[[selected_variable]])) +
      geom_histogram(binwidth = binwidth_reactive(), fill = selected_fill_color, color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", selected_variable),
           x = selected_variable,
           y = "Frequency")
  })
  
  # Calculate and display summary statistics
  output$summary_text <- renderText({
    selected_variable <- input$variable
    selected_filter <- input$filter_type
    selected_fill_color <- tolower(input$fill_color)
    
    if (selected_filter == "Range") {
      selected_range <- input$range
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData[[selected_variable]] >= selected_range[1] &
                          covidData[[selected_variable]] <= selected_range[2], ]
    } else {
      selected_month <- input$month_filter
      data <- covidData[complete.cases(covidData[[selected_variable]]) & 
                          covidData$month_category == selected_month, ]
    }
    
    summary_stats <- summary(data[[selected_variable]])
    mean_val <- mean(data[[selected_variable]], na.rm = TRUE)
    sd_val <- sd(data[[selected_variable]], na.rm = TRUE)
    
    result <- paste("Mean: ", mean_val, "\n")
    result <- paste(result, "Standard Deviation: ", sd_val, "\n")
    result <- paste(result, "Median: ", summary_stats[3], "\n")
    result <- paste(result, "Five-Number Summary: ", paste(summary_stats[c(1, 2, 3, 5, 6)], collapse = ", "), "\n")
    
    return(result)
  })
  output$app_explanation <- renderText({
    "This dataset, containing information on the quantity of tests administered over\n time, plays a vital role in interpreting daily reported cases and gaining insight into\n the actual dynamics of COVID-19 spread in each country.\n
    The graphs displayed will show the frequency of each selected variable over the period of 01/22/2020 to 04/07/2020."
  })
  output$image_citation <- renderText({
    "<p style='text-align:center; font-size:14px;'>Image source: Texas Department of Insurance</p>"
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





















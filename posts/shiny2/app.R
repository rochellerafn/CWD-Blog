library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)

mens_100m_nation_medals <- read.csv("https://raw.githubusercontent.com/rochellerafn/CWD-Blog/main/posts/shiny2/mens_100m_nation_medals.csv")
mens_100m_nation_medals$Rank <- factor(mens_100m_nation_medals$Rank, levels = c("Gold", "Silver", "Bronze"))
mens_100m_nation_medals$Year <- as.Date(as.character(mens_100m_nation_medals$Year), format = "%Y")
medal_color <- colorFactor(c("#EBD739", "#B5BCC2", "#FFB48C"), mens_100m_nation_medals$Rank)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Who Has All the Medals for the Men's 100m Dash?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year", "Choose a year range:", timeFormat = "%Y",
                  min = min(mens_100m_nation_medals$Year), max = max(mens_100m_nation_medals$Year), 
                  value = c(min(mens_100m_nation_medals$Year), max(mens_100m_nation_medals$Year)), step = 4),
      selectInput("Nation", "Choose a Country:",
                  choices = unique(mens_100m_nation_medals$Nation), multiple = TRUE, selected = unique(mens_100m_nation_medals$Nation)),
      checkboxGroupInput("Rank", "Choose Medal Type",
                         choices = unique(mens_100m_nation_medals$Rank), selected = unique(mens_100m_nation_medals$Rank)),
      sliderInput("Time", "Choose a Time:", 
                  min = min(mens_100m_nation_medals$Time), max = max(mens_100m_nation_medals$Time), 
                  value = c(min(mens_100m_nation_medals$Time), max(mens_100m_nation_medals$Time)), step = 0.01)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("plot"),
      DTOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_0_data <- reactive({
    mens_100m_nation_medals %>%
      select(-X) %>%
      filter(Rank %in% input$Rank,
             Nation %in% input$Nation,
             Year >= input$Year[1] & Year <= input$Year[2],
             Time >= input$Time[1] & Time <= input$Time[2])
  })
  
  output$plot <- renderLeaflet({
    
    # draw the plot with the specified filters
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = filtered_0_data(),
        color = ~medal_color(Rank),
        stroke = FALSE, fillOpacity = 0.9,
        clusterOptions = markerClusterOptions(),
        label = ~paste(
          "Nation: ", Nation,
          "Year:", Year,
          "Rank: ", Rank,
          "Time: ", Time
        ),
        labelOptions = labelOptions(
          noHide = FALSE,
          direction = "auto",
          textOnly = FALSE,
          className = "marker-label"
        )
      )
  })
  
  output$table <- renderDT({
    # Show a data table of the filtered data
    datatable(filtered_0_data() %>%
                select(-lon, -lat) %>%
                mutate(Year = format(Year, "%Y")))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

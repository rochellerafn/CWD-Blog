library(shiny)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)

mens_100m_nation_medals <- read.csv("/Users/rochellerafn/RStudio_Files/Data_Communication/CWD-Blog/posts/shiny2/mens_100m_nation_medals.csv")
medal_color <- colorFactor(c("#EBD739", "#B5BCC2", "#FFB48C"), mens_100m_nation_medals$Rank)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("100 Years of Men's 100m Dash Medals"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year", "Choose a year range:", 
                  min = min(mens_100m_nation_medals$Year), max = max(mens_100m_nation_medals$Year), 
                  value = c(min(mens_100m_nation_medals$Year), max(mens_100m_nation_medals$Year)), step = 1),
      selectInput("Nation", "Choose a Country:",
                  choices = unique(mens_100m_nation_medals$Nation), multiple = TRUE, selected = unique(mens_100m_nation_medals$Nation)),
      selectInput("Rank", "Choose Medal Type",
                  choices = unique(mens_100m_nation_medals$Rank), multiple = TRUE, selected = unique(mens_100m_nation_medals$Rank)),
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
    datatable(filtered_0_data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
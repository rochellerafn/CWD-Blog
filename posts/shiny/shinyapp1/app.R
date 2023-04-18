library(shiny)
library(DT)
library(dplyr)
library(ggplot2)


data <- data %>%
  mutate(Category = recode(Category, "MEZCAL"="MEZCAL / CACHACA", 
                           "CACHACA"="MEZCAL / CACHACA", 
                           "COCKTAILS"="COCKTAILS / VERMOUTH", 
                           "VERMOUTH"="COCKTAILS / VERMOUTH"))

or_liquor_i <- data %>%
  filter(Year < "2023") %>%
  group_by(Category, County, Year) %>%
  select(County, Year, Category, Sales) %>%
  summarize(Sales = sum(Sales)) %>%
  ungroup()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Oregon Liquor Sales"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year", "Choose a year range:", 
                  min = min(or_liquor_i$Year), max = max(or_liquor_i$Year), 
                  value = c(min(or_liquor_i$Year), max(or_liquor_i$Year)), step = 1),
      selectInput("County", "Choose a County:",
                  choices = unique(or_liquor_i$County), multiple = TRUE, selected = ""),
      selectInput("Category", "Choose Liquor Type",
                  choices = unique(or_liquor_i$Category), multiple = TRUE, selected = "")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      DTOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    or_liquor_i %>% 
      filter(Category %in% input$Category,
             County %in% input$County,
             Year >= input$Year[1] & Year <= input$Year[2])
  })
  
  output$plot <- renderPlot({
    
    # draw the plot with the specified filters
    ggplot(filtered_data(), aes(x=Year, y=Sales, shape=County, color=Category)) +
      geom_jitter(alpha=.6, size=7) +
      labs(y = "Sales", color = "Category") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.background = element_blank(),
            panel.background = element_blank()) +
      scale_y_continuous(limits = c(0,50000000), labels = scales::comma)
  })
  
  output$table <- renderDT({
    # Show a data table of the filtered data
    datatable(filtered_data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

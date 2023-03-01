# Initial app prototype for Kelly's graphs.

library(shiny)
library(tidyverse)

streaming_services <- c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Ratings by Streaming Service"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "service",
        "Streaming service:",
        streaming_services,
        selected = "Netflix"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  streaming <- read_delim("../data/streaming-platform-data.csv")
  
  streaming$modified_ratings <- as.numeric(
      str_remove(streaming$`Rotten Tomatoes`, "/100"))
  
  output$distPlot <- renderPlot({
    
    # generate streaming service name based on input$service from ui.R
    service <- input$service
    
    # draw the histogram for the specified streaming service
    streaming %>% 
      filter(!is.na(modified_ratings)) %>% 
      filter(!!as.symbol(service) == 1) %>% 
      ggplot(aes(x = modified_ratings))+
      geom_bar(stat = "count", aes(fill = ..x..))+
      scale_fill_gradient(low="red",high="green")+
      labs(title=service,
           x = "Rotten Tomatoes Rating",
           y = 'Movie count')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Prototype for Kelly's graphs.

# Loading libraries
library(shiny)
library(tidyverse)

streaming_services <- c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Movie Ratings by Streaming Service"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      # Widget description
      p("Select a streaming service to view the movie ratings."),
      
      # Widget
      selectInput(
        "service",
        "Streaming service:",
        streaming_services,
        selected = "Netflix"
      ),
      
      # Analysis of the histograms
      h3("Analysis:"),
      p("All the streaming services have a large range of ratings and somewhat follow a normal distribution curve.
        All histograms have some outlier data points where there are a handful of very low rated movies in their catalogs.
          This is one area where start up companies making a streaming service that is competitive with the current market could improve on."),
      p("Netflix seems to have the widest range of ratings in their movie catalog, while Hulu seem to have the most narrow.
        An explanation for this could be that the size of Netflix's catalog is much larger than Hulu, almost having 1500 more movies.
        Hulu appears to have the median with the highest rating as it's center is around the 60/100 Rotten Tomatoes rating. Disney+ also has a higher median than Netflix and Prime Video.
        The service with the lowest median rating is Prime Video, with it's histogram looking to be skewed to the right. 
        Prime Video also seems to have the lowest percentage of movies in its catalog that are highly rated as it's bins are the least green shaded and lean more towards red/yellow.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # Plot
      plotOutput("distPlot"),
      
      # Text related to plot
      h3("Histogram Description:"),
      p("These histograms contain data of each streaming service's movie catalog in 2021 to early 2022. 
        The histogram's bins on the x axis represent the Rotten Tomatoes rating given to each movie. Ratings are points from 0 to 100.
        The histogram's bins on the x axis represent the ",
        a("Rotten Tomatoes rating", href= 'https://www.rottentomatoes.com/about#:~:text=When%20at%20least%2060%25%20of,to%20indicate%20its%20Fresh%20status.&text=When%20less%20than%2060%25%20of,to%20indicate%20its%20Rotten%20status.'),
        "given to each movie. Ratings are points from 0 to 100.
        The height of each bin corresponds to how many movies have gotten that rating.
        The color scale on the bins are another way to depict the ratings. Green corresponds to higher ratings while red are lower ratings."),
      
      # Table about total movies on each service
      tableOutput("table"),
      
      # Text related to table
      h3("Table Description:"),
      p("This table shows how many movies were available on each streaming service in 2021 to early 2022.")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  streaming <- read_delim("../data/streaming-platform-data.csv")
  
  streaming$modified_ratings <- as.numeric(
      str_remove(streaming$`Rotten Tomatoes`, "/100"))
  
  # Creating histogram plot
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
           y = 'Movie count',
           fill = "Ratings")
  })
  
  # Creating table
  output$table <- renderTable({
    
    # Vector with total number of movies on each streaming service
    total_Movies <- c(sum(streaming[,7] == 1), 
                      sum(streaming[,8] == 1), 
                      sum(streaming[,9] == 1), 
                      sum(streaming[,10] == 1))
    
    # Creating data table from names of services and total number of movies
    streaming_total_movies <- data.frame(streaming_services,total_Movies)
    streaming_total_movies
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

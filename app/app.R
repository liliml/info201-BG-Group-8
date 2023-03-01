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
    mainPanel(plotOutput("distPlot"),
  
        # Histogram description
        p("Histogram Description:"),
        p("These histograms contain data of each streaming service's movie catalog in 2021 to early 2022. 
        The histogram's bins on the x axis represent the ",
        a("Rotten Tomatoes rating", href= 'https://www.rottentomatoes.com/about#:~:text=When%20at%20least%2060%25%20of,to%20indicate%20its%20Fresh%20status.&text=When%20less%20than%2060%25%20of,to%20indicate%20its%20Rotten%20status.'),
        "given to each movie. Ratings are points from 0 to 100.
        The height of each bin corresponds to how many movies have gotten that rating.
        The color scale on the bins are another way to depict the ratings. Green corresponds to higher ratings while red are lower ratings."),
        
        # Table
        tableOutput("table"),
        
        # Table Description
        p("Table Description:"),
        p("This table shows how many movies were available on each streaming service in 2021 to early 2022."),
        
        # Analysis
        p("Analysis:"),
        p("All streaming services have a variety of ratings and mostly follow a normal distribution shape.
        All histograms have some outlier data points where there are a handful of very low rated movies in their catalogs.
        This is one area where start up companies making a streaming service that is competitive with the current market could improve on."),
        p("Netflix seems to have the widest range of ratings in their movie catalog, while Hulu seem to have the most narrow.
        An explanation for this could be that the size of Netflix's catalog is much larger than Hulu, almost having 1500 more movies.
        Hulu appears to have the median with the highest rating as it's center is around the 60/100 Rotten Tomatoes rating. Disney+ also has a higher median than Netflix and Prime Video.
        The service with the lowest median rating is Prime Video, with it's histogram looking to be skewed to the right. 
        Prime Video also seems to have the lowest percentage of movies in its catalog that are highly rated as it's bins are the least green shaded and lean more towards red/yellow."),
        p("There is a trend with larger movie catalogs and greater diversity of ratings, but moreso leaning towards the lower ratings spectrum while the smaller catalogs lean more towards the higher spectrum even with a more narrow ratings range.
        It is possible that Hulu and Disney+ are more selective with what movies are on their catalog, hence why there are less but are of higher Rotten Tomatoes ratings. 
        For start up companies, it's important to recognize the balance between having a large and diverse catalog of movies and a smaller scope that are of higher quality.")
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
           y = 'Movie count',
           fill = "Ratings")
  })
  
  #Creating table
  output$table <- renderTable({
    total_Movies <- c(sum(streaming[,7] == 1), sum(streaming[,8] == 1), sum(streaming[,9] == 1), sum(streaming[,10] == 1))
    
    streaming_total_movies <- data.frame(streaming_services,total_Movies)
    streaming_total_movies
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

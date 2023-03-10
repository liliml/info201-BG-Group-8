library(tidyverse)
library(shiny)

stream <- read_delim("streaming-platform-data.csv")
serviceType <- c("Netflix", "Hulu", "Prime Video", "Disney+")

ui <- fluidPage(
  titlePanel("Streaming Service Insights"),
  tabsetPanel(
    tabPanel("General",
             titlePanel("General Introduction"),
             sidebarLayout(
               sidebarPanel(
               p("This is an app showing the movie data of different streaming 
               platforms based on their age rating, Rotten Tomatoes rating, and 
               year, starting from the earliest year of", strong("1914"), "to 
               the latest year of", strong("2021."), "There are ", nrow(stream), 
               "rows and", ncol(stream), "columns. The dataset was made and 
               posted on Kaggle.com about three years ago, and was most recently 
               updated about a year ago. It was mainly inspired by the questions 
               of", em('Which streaming platform(s) can I find this movie on?'),
               "and", em('What are the target age group movies available on each 
               streaming platform?'), "While there is no original author, there 
               was an available collaborator under the name of Ruchi Bhatia who 
               seems to own the dataset, so I believe they are the author.")
               ),
             mainPanel(
               dataTableOutput("smolTable")
             )
             )
             ),
    tabPanel("Plot",
             titlePanel("Histogram: Movies per Platform (By Age Rating)"),
             sidebarLayout(
               sidebarPanel(
                 p("This chart attempts to understand what the differences in 
                 each amount of movies for a single age rating can cause in 
                 different streaming services. Based on the statistics gathered, 
                 inferences can be made about the likelihood that a certain 
                 movie will be approved for a streaming service's curation 
                 based on the age rating it was given. This can also show trends 
                 in what streaming services may typically prefer in terms of the 
                 age ratings for a movie."),
                 selectInput(
                 "service",
                 "Streaming service type:",
                 serviceType,
                 selected = "Netflix"
                 ),
                 radioButtons("color", 
                              "Want to change the color?",
                              choiceNames = c("Hello darkness my old friend",
                                              "Looks like Netflix", 
                                              "Looks like Amazon Prime", 
                                              "Looks like Hulu", 
                                              "Looks like Disney+"),
                              choiceValues = c("black", "red", "lightblue", "lightgreen", 
                                                    "darkblue")
                 )
               ), 
               mainPanel(
                 plotOutput("hist"),
                 textOutput("plotText")
               ),
             )
    ),
    tabPanel("Table",
             titlePanel("Table: Selectable Data for Streaming"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "filter",
                   "What would you like to view data for?",
                   choiceNames = list("Movie Number", "Release Year", 
                                      "Age Rating", "Critic Reception", 
                                      "Netflix Availability", "Hulu Availability", 
                                      "Prime Video Availability", "Disney+ 
                                      Availability"),
                   choiceValues = list("ID", "Year", "Age", 
                                       "Rotten Tomatoes", "Netflix", "Hulu", 
                                       "Prime Video", "Disney+")
                 )
               ), 
               mainPanel(
                 dataTableOutput("table"),
                 textOutput("tableText"),
                 textOutput("tableText2")
               )
             )
    )
  ),
)

server <- function(input, output) {
  
  output$smolTable <- renderDataTable({
    
    stream %>% 
      select(ID, Title, Year, Age, `Rotten Tomatoes`) %>% 
      head(5)
    
  })
  
  ##plot outputs
  output$hist <- renderPlot({
    
    ##hist inputs
    service <- input$service
    
    ageDemo <- stream %>%
      filter(Age != "NA") %>% 
      arrange(Age) %>% 
      filter(!!rlang::sym(service) == 1)
    
    ggplot(ageDemo, aes(fill = as.factor(Age))) +
      geom_bar(mapping = aes(x = ageDemo$Age), stat = "count", fill = input$color) +
        labs(title = paste("Movies for:", input$service),
             x = "Age Rating",
             y = "Movie count",
             fill = "Age")
  })
  
  output$plotText <- renderText({
    ##hist inputs
    service <- input$service
    
    ageDemo <- stream %>%
      filter(Age != "NA") %>% 
      arrange(Age) %>% 
      filter(!!rlang::sym(service) == 1)
    
    paste("This is a plot output of the number of movies in each age demographic
          for the selected streaming service. There are a total of", nrow(ageDemo), 
          "movies for this streaming platform,", input$service, ". This data 
          shows that there are large discrepancies in what kind of movies are 
          available for each demographic. Using a table like this, you can find 
          many things, such as where a streaming service is lacking material for
          a certain age demographic. If you are an aspiring film maker and want 
          to get the big bucks, this is the data description for you!")
  })
  
  output$table <- renderDataTable({
    
    filter <- input$filter
    
    stream %>% 
      select(Title, input$filter)
    
  })
  
  output$tableText<- renderText({
    
    filter <- input$filter
    
    stream %>% 
      select(Title, input$filter)
    
    paste("This is a table showing you information for an index of", nrow(stream), 
          "movies. Looking for a specific movie? You can select different kinds 
          of filters for our data to try and find it using the available checkbox 
          widget. For each streaming platform's availability rating, a 1 
          indicates that the movie is available on a streaming platform, 
          while a 0 indicates that it is not. Here are the filters you've 
          selected:")
  })
  
  output$tableText2<- renderText({
    
    filter <- input$filter
    
    stream %>% 
      select(Title, input$filter)
    
    paste("+", input$filter)
  })
}

shinyApp(ui = ui, server = server)
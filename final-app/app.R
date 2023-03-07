#
# Bella Lee, Zerelda Mauricio, Lilian Law, and Kelly Le
# INFO 201 Section BG
# TA: Rona Guo
#

library(shiny)
library(tidyverse)

## Load data set
streaming <- read_delim("data/streaming-platform-data.csv")

## Streaming service names
streaming_services <- c("Netflix", "Hulu", "Prime Video", "Disney+")

ui <- fluidPage(
    navbarPage(
      
      ## Title
      "Movies on Streaming Platforms",
      
      ## Overview
      tabPanel(
        "Overview",
        h1("Purpose and Audience"),
        p("With the rise of online streaming, many different services have 
          flooded the market to fulfill people’s demand for convenient entertainment. 
          Our project looks at various qualities of streaming services’ movie 
          catalogs to identify if there are any discernible relationships or 
          trends that could give start up companies or entrepreneurs an idea of 
          how to create a service that would be competitive in the market."),
        h1("Main Questions"),
        tags$ul(
          tags$li("What is the distribution of movie ratings on each streaming service?"),
          tags$li("What is the distribution of age ratings on each streaming service?"),
          tags$li("How many movies per service per year were there?")
        ),
        h1("Data Source"),
        p("Our dataset covers four notable streaming services; Netflix, Hulu, 
          Prime Video, and Disney+. The dataset was collected through data 
          scraping from these four streaming services in 2021 through early 2022 
          and published on",
          a("Kaggle", href="https://www.kaggle.com/datasets/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
          ". It contains the entire movie catalog of the four streaming services, 
          with information of the movie name, what year it was from, the age rating, 
          Rotten tomatoes rating, and whether it was available on that particular 
          service (represented with 1 or 0)."),
        img(src="amazon.png", alt="Amazon logo", width="auto", height="100px"),
        img(src="disney.png", alt="Disney+ logo", width="auto", height="100px"),
        img(src="hulu.png", alt="Hulu logo", width="auto", height="100px"),
        img(src="netflix.png", alt="Netflix logo", width="auto", height="100px")
      ),
      ## END Overview
      
      ## Movie Ratings by Streaming Service (Kelly)
      tabPanel(
        "Typical Ratings",
        titlePanel("Movie Ratings by Streaming Service"),
        sidebarLayout(
          sidebarPanel(
            p("These histograms contain data of each streaming service's movie 
              catalog in 2021 to early 2022. The histogram's bins on the x axis 
              represent the Rotten Tomatoes rating given to each movie. Ratings are 
              points from 0 to 100. The histogram's bins on the x axis represent the ",
                a("Rotten Tomatoes rating", 
                  href="https://www.rottentomatoes.com/about#:~:text=When%20at%20least%2060%25%20of,to%20indicate%20its%20Fresh%20status.&text=When%20less%20than%2060%25%20of,to%20indicate%20its%20Rotten%20status."),
                "given to each movie. Ratings are points from 0 to 100. The height of 
              each bin corresponds to how many movies have gotten that rating. The 
              color scale on the bins are another way to depict the ratings. Green 
              corresponds to higher ratings while red are lower ratings."),
            selectInput(
              "rating_service",
              "Streaming service:",
              streaming_services,
              selected = "Netflix"
            )
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("ratings_plot")
          )
        )
      ),
    ## END Movie Ratings by Streaming Service
    
    ## Movies per Platform by Age Rating (Z)
    tabPanel(
      "Typical Age Ratings",
      titlePanel("Movies per Platform by Age Rating"),
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
            "platform_service",
            "Streaming service:",
            streaming_services
          ),
          radioButtons("platform_color", 
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
          plotOutput("platform_plot"),
          textOutput("platform_text")
        )
      )
    ),
    ## END Movies per Platform by Age Rating
    
    ## Movies Per Streaming Service per Year (Lilian)
    tabPanel(
      "Compare Services",
      titlePanel("Movies Per Streaming Service Per Year"), 
      sidebarLayout(
         sidebarPanel(
           p("The bar plot below shows the amount of movies per year that are 
           availiable on each streaming service. The slider will allow you to 
           select a particular year to look at. The years shown on the slider 
           range from 1914 to 2021 which allows for a broad selection of movies. 
           The checkboxes displayed will also let you select which services to 
           show for the selected year. This bar graph is helpful for understanding 
           what years had the most movies, and which streaming platforms carry 
           the most movies. These statistics  could be helpful for finding which 
           streaming service has the most options. This data could also help a 
           startup company decide which years to focus on in terms of years 
           movies were releaseed as more movies that year accross more platforms 
           could indicate more popularity for movies from that year."), 
            sliderInput("year", 
                         "Year:", 
                         min = min(streaming$Year), 
                         max = max(streaming$Year), 
                         2004,
                         sep = ""),
            checkboxGroupInput("checkGroup", 
                              "Streaming services:",
                              choices = streaming_services,
                              selected = streaming_services),
        ), 
        mainPanel(
          plotOutput("year_plot"), 
          textOutput("choosenYearandServices"))
      )
    )
    ## END Movies Per Streaming Service per Year
  )
)

server <- function(input, output) {
  ## Movie Ratings by Streaming Service
  output$ratings_plot <- renderPlot({
    streaming$modified_ratings <- as.numeric(
      str_remove(streaming$`Rotten Tomatoes`, "/100"))
    
    # Draw the histogram for the specified streaming service
    streaming %>% 
      filter(!is.na(modified_ratings)) %>% 
      filter(!!as.symbol(input$rating_service) == 1) %>% 
      ggplot(aes(x = modified_ratings))+
      geom_bar(stat = "count", aes(fill = after_stat(x)))+
      scale_fill_gradient(low="red",high="green")+
      labs(title=input$rating_service,
           x = "Rotten Tomatoes Rating",
           y = 'Movie count',
           fill = "Ratings")
  })
  ## END Movie Ratings by Streaming Service
  
  ## Movies per Platform by Age Rating
  output$platform_plot <- renderPlot({
    ageDemo <- streaming %>%
      filter(Age != "NA") %>% 
      filter(!!rlang::sym(input$platform_service) == 1)
    
    ggplot(ageDemo, aes(fill = as.factor(Age))) +
      geom_bar(mapping = aes(x = ageDemo$Age), stat = "count", fill = input$platform_color) +
      labs(title = paste("Movies for:", input$platform_service),
           x = "Age Rating",
           y = "Movie count",
           fill = "Age")
  })
  
  output$platform_text <- renderText({
    ageDemo <- streaming %>%
      filter(Age != "NA") %>% 
      filter(!!rlang::sym(input$platform_service) == 1)
    
    paste("There are a total of", nrow(ageDemo), 
          "movies for this streaming platform,", paste0(input$platform_service, "."), 
          "This data shows that there are large discrepancies in what kind of 
          movies are available for each demographic.")
  })
  ## END Movies per Platform by Age Rating
  
  ## Movies Per Streaming Service per Year
  output$year_plot <- renderPlot({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- streaming %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(streaming_services, number) %>% 
      filter(streaming_services %in% selected)
    ggplot(by_service) +
      geom_col(mapping = aes(x = streaming_services, y = number, fill = factor(streaming_services))) +
      labs(title="Movies by Year",
           x = "Streaming Service",
           y = "Movie count")
  })
  
  output$year_description <- renderText({
    filtered <- data %>%
      filter(Year >= input$plot_year[1] & Year <= input$plot_year[2]) %>%
      filter(!!rlang::sym(input$year_service) == 1)
    
    paste("There are", nrow(filtered), 
          "movies that were released between", input$plot_year[1], "and", 
          input$plot_year[2], "on", paste0(input$year_service, "."))
  })
  
  output$choosenYearandServices <-  renderText({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- streaming %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(streaming_services, number) %>% 
      filter(streaming_services %in% selected)
    paste("There were ", sum(by_service$number), 
         " movies in ", input$year, "across all streaming platforms.") 
    
  })
  ## END Movies Per Streaming Service per Year
  
  ## Images
  output$amazon_logo <- renderImage({
    list(src = "../imgs/amazon.png",
         alt = "")
  }, deleteFile = FALSE)
  ## END Images
}

# Run the application 
shinyApp(ui = ui, server = server)

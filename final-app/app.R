#
# Final Project Deliverable
# Bella Lee, Kelly Le, Lilian Law, and Zerelda Mauricio
# INFO 201 Section BG
# TA: Rona Guo
#
# This Shiny application includes interactive data visualizations for the Rotten
# Tomatoes ratings, age ratings, and release years for movies on the streaming
# platforms Netflix, Hulu, Disney+, and Amazon
#

library(shiny)
library(shinythemes)
library(tidyverse)

## Load data set
streaming <- read_delim("streaming-platform-data.csv")

## Streaming service names
streaming_services <- c("Netflix", "Hulu", "Prime Video", "Disney+")

ui <- fluidPage(
  theme = shinytheme("simplex"),
  navbarPage(
    ## Title
    "Movies on Streaming Platforms",
    
    ## Overview
    tabPanel(
      "Overview",
      p("Project created by Bella Lee, Kelly Le, Lilian Law, Zerelda Mauricio for
        INFO 201 Winter 2023."),
      h1("Purpose and Audience"),
      p(
        "With the rise of online streaming, many different services have
          flooded the market to fulfill people’s demand for convenient
          entertainment. Our project looks at various qualities of streaming
          services’ movie catalogs to identify if there are any discernible
          relationships or trends that could give start up companies or
          entrepreneurs an idea of how to create a service that would be
        competitive in the market."
      ),
      h1("Main Questions"),
      tags$ul(
        tags$li(
          "What is the distribution of movie ratings on each streaming service?"
        ),
        tags$li("What is the distribution of age ratings on each streaming
                service?"),
        tags$li("How many movies per service per year are there?")
      ),
      h1("Data Source"),
      p(
        "Our dataset covers four notable streaming services; Netflix, Hulu,
          Prime Video, and Disney+. The dataset was collected through data
          scraping from these four streaming services in 2021 through early 2022
          and published on",
        a("Kaggle.", href = "https://www.kaggle.com/datasets/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
        "It contains the entire movie catalog of the four streaming services,
          with information of the movie name, what year it was from, the age rating,
          Rotten tomatoes rating, and whether it was available on that particular
          service (represented with 1 or 0)."
      ),
      
      # Create white space
      headerPanel(""),
      headerPanel(""),
      
      # Center images
      fluidRow(column(
        width = 12,
        align = "center",
        img(
          src = "amazon.png",
          alt = "Amazon logo",
          width = "auto",
          height = "100px"
        ),
        img(
          src = "disney.png",
          alt = "Disney+ logo",
          width = "auto",
          height = "100px"
        ),
        img(
          src = "hulu.png",
          alt = "Hulu logo",
          width = "auto",
          height = "100px"
        ),
        img(
          src = "netflix.png",
          alt = "Netflix logo",
          width = "auto",
          height = "100px"
        )
      ))
    ),
    # close Overview
    
    ## Movie Ratings by Streaming Service (Kelly)
    tabPanel(
      "Rotten Tomato Ratings",
      titlePanel("Rotten Tomato Ratings by Streaming Service"),
      sidebarLayout(sidebarPanel(
        p(
          "These histograms contain data of each streaming service's movie
          catalog in 2021 to early 2022. The histogram's bins on the x axis
          represent the ",
          a("Rotten Tomatoes rating",
            href = "https://www.rottentomatoes.com/about#:~:text=When%20at%20least%2060%25%20of,to%20indicate%20its%20Fresh%20status.&text=When%20less%20than%2060%25%20of,to%20indicate%20its%20Rotten%20status."),
          "given to each movie. Ratings are points from 0 to 100. The height of
          each bin corresponds to how many movies have gotten that rating. The
          color scale on the bins are another way to depict the ratings. Green
          corresponds to higher ratings while red are lower ratings."
        ),
        p("There is a trend with larger movie catalogs and greater diversity of
            ratings. The services with a larger movie catalog have more movies with
            lower ratings (skewed to the right), while the smaller catalogs lean more
            towards the higher end, despite their narrow range of diverse ratings. It
            is possible that Hulu and Disney+ are more selective with what movies
            are on their catalog, hence why there are less movies but have higher
            Rotten Tomatoes ratings, while Netflix and Prime Video’s approach is
            having many movies of all ratings."),
        selectInput(
          "rating_service",
          "Streaming service:",
          streaming_services,
          selected = "Netflix"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("ratings_plot")))
    ),
    # close Movie Ratings by Streaming Service
    
    ## Movies per Platform by Age Rating (Z)
    tabPanel(
      "Age Ratings",
      titlePanel("Movies per Platform by Age Rating"),
      sidebarLayout(
        sidebarPanel(
          p(
            "This chart attempts to understand what the differences in
            each amount of movies for a single age rating can cause in
            different streaming services. Based on the statistics gathered,
            inferences can be made about the likelihood that a certain
            movie will be approved for a streaming service's curation
            based on the age rating it was given. This can also show trends
            in what streaming services may typically prefer in terms of the
            age ratings for a movie."
          ),
          selectInput("platform_service",
                      "Streaming service:",
                      streaming_services),
          radioButtons(
            "platform_color",
            "Want to change the color?",
            choiceNames = c(
              "Hello darkness my old friend",
              "Looks like Netflix",
              "Looks like Amazon Prime",
              "Looks like Hulu",
              "Looks like Disney+"
            ),
            choiceValues = c("black", "red", "lightblue", "lightgreen",
                                    "darkblue")
          )
        ),
        mainPanel(plotOutput("platform_plot"),
                  textOutput("platform_text"))
      )
    ),
    # close Movies per Platform by Age Rating
    
    ## Movies Per Streaming Service per Year (Lilian)
    tabPanel(
      "Release Year",
      titlePanel("Movies Per Streaming Service Per Year"),
      sidebarLayout(
        sidebarPanel(
          p(
            "The bar plot below shows the amount of movies per year
            that are availiable on each streaming service.
            The slider allows you to select a particular year to look at.
            The years shown on the slider range from 1914 to 2021, which allows 
            for a broad selection of movies. The checkboxes allow you
            select which services to show for the selected year. Note that some 
            streaming services may carry the same movie, so the total amount of 
            movies released in a year may differ from the total when the values 
            of Netflix, Hulu, Prime Video, and Disney+ are summed up."
          ),
          sliderInput(
            "year",
            "Year:",
            min = min(streaming$Year),
            max = max(streaming$Year),
            2004,
            sep = ""
          ),
          checkboxGroupInput(
            "checkGroup",
            "Streaming services:",
            choices = streaming_services,
            selected = streaming_services
          ),
          uiOutput("choosenYearandServices")
        ),
        mainPanel(plotOutput("year_plot"))
      )
    ),
    # close Movies Per Streaming Service per Year
    
    ## Conclusion
    tabPanel(
      "Conclusion",
      
      h1("General Findings"),
      # Center table
      fluidRow(
        column(width = 3,
               tableOutput("conclusion_table")),
        column(width = 4,
               plotOutput("conclusion_plot")),
        column(
          width = 5,
          p(
            "Overall, Netflix and Prime Video dominate the movie streaming
             industry, with 3695 and 4113 movies available respectively.
             However, as elaborated upon in the more detailed analysis below,
             each platform has its own strengths regarding typical Rotten
             Tomatoes ratings, age ratings, and release years that allow
             them to appeal to different audiences. Thus, when creating a
             movie streaming platform, the below factors must be considered."
          )
        )
      ),
      
      h1("Metric-Related Insights"),
      fluidRow(
        column(
          width = 4,
          h2("Rotten Tomato Ratings"),
          p(
            "For start up companies, it's important
            to recognize the balance between having a large and diverse catalog of
            movies and a smaller scope that are of higher quality. It is not
            realistically possible to have both approaches so companies should
            consider which method they want to target more."
          )
        ),
        column(
          width = 4,
          h2("Age Ratings"),
          p(
            "Most streaming platforms curate a list of movies mainly geared
            towards adults, with the highest age ratings for 3 of the 4 streaming
            platforms being rated 18+.  The outlier to this trend is Disney+,
            whose highest curation is under the age rating ‘all.’ This implies
            that the main target audience of movie stream providers is adults,
            and that Disney is only an exception due to their entire brand mainly
            advertising towards families and children. This displays a huge disparity
            that startup streaming companies can utilize to their advantage
            through curating media for children. While there is high concentration
            — and by consequence, high competition— in the mature-rated market of
            film, there is an open field for children’s film, which can be quite
            lucrative."
          )
        ),
        column(
          width = 4,
          h2("Release Year"),
          p(
            "In 1914, one movie was released, and this was the earliest year
             for which there was only one movie. The years following 1914
             have the least movies, with an increase starting from 1990. Only
             Prime Video seems to carry these earlier movies. In 2019 there
             were 1014 movies released across all streaming services, which
             is the maximum amount of movies released in a year. Streaming
             services carry more newer movies than old movies. This trend could
             be explained by the fact that most movies watched are newer
             movies, which could be more popular among consumers. These
             statistics are helpful for finding which streaming service has
             the most options. This data can also help a startup company
             decide which years’ movies to focus on, as more availability
             for that year across platforms could indicate increased demand
             for movies released that year."
          )
        )
      ),
      # close fluidRow
      
      h1("Data Quality"),
      p(
        "The quality of the dataset for the four streaming services is reasonable.
        There are factors that could have made it better like including movie
        genres, duration of movie, and origin country. Having these would create
        a better understanding of what kind of movies are hosted on successful
        streaming platforms. The dataset is a bit outdated which may interfere
        with our project goal since the streaming service industry has grown
        rapidly and the dataset has not kept up with this, exponentially adding
        more movie selections and removing certain films from their curation list.
        However, market inferences can still be made from past trends, and since
        it has only been a year, the trends are still very predictive."
      ),
      
      h1("Future Direction"),
      p(
        "To advance the project in the future, we could include more interactive
        elements in the visualization using libraries other than ggplot. Elements
        such as tooltips will help tell a more detailed story about the data at
        a glance. We could also improve formatting and aesthetics (fonts,
        background color, how tabs look, size of layout, etc). Lastly, finding
        another dataset with more streaming services to increase our library
        could expand our insights and takeaways."
      )
    ) # close Conclusion
  ) # close navBarPage
) # close fluidPage

server <- function(input, output) {
  ## Movie Ratings by Streaming Service
  output$ratings_plot <- renderPlot({
    # Clean Rotten Tomatoes ratings
    streaming$modified_ratings <-
      as.numeric(str_remove(streaming$`Rotten Tomatoes`, "/100"))
    
    # Draw histogram for amount of movies for each rating for the inputted platform
    streaming %>%
      filter(!is.na(modified_ratings)) %>%
      filter(!!as.symbol(input$rating_service) == 1) %>%
      ggplot(aes(x = modified_ratings)) +
      geom_bar(stat = "count", aes(fill = after_stat(x))) +
      scale_fill_gradient(low = "red", high = "green") +
      labs(
        title = "Streaming service Movie Ratings Distribution",
        x = "Rotten Tomatoes Rating (Points)",
        y = 'Movie count',
        fill = "Ratings"
      )+
      theme(plot.title = element_text(face="bold"))
  })
  ## END Movie Ratings by Streaming Service
  
  ## Movies per Platform by Age Rating
  output$platform_plot <- renderPlot({
    ageDemo <- streaming %>%
      filter(Age != "NA") %>%
      filter(!!rlang::sym(input$platform_service) == 1)
    
    ggplot(ageDemo, aes(fill = as.factor(Age))) +
      geom_bar(
        mapping = aes(x = ageDemo$Age),
        stat = "count",
        fill = input$platform_color
      ) +
      labs(
        title = paste("Movies for:", input$platform_service),
        x = "Age Rating",
        y = "Movie count",
        fill = "Age"
      ) +
      scale_fill_manual(
        values = c(
          "Netflix" = "red",
          "Hulu" = "seagreen2",
          "Prime Video" = "skyblue",
          "Disney+" = "blue"
        )
      )
  })
  
  output$choosenYearandServices = renderUI({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- streaming %>%
      filter(Year == year)
    number <- c(
      sum(filtered_by_year$Netflix),
      sum(filtered_by_year$Hulu),
      sum(filtered_by_year$`Prime Video`),
      sum(filtered_by_year$`Disney+`)
    )
    by_service <- data.frame(streaming_services, number) %>%
      filter(streaming_services %in% selected)
    
    maxMoviesinaYear <- streaming %>%
      group_by(Year) %>%
      summarise(movieTotal = length(Title))
    
    maxVal <- max(maxMoviesinaYear$movieTotal)
    
    maxMoviesinaYear <- maxMoviesinaYear %>%
      group_by(Year) %>%
      filter(movieTotal == maxVal)
    
    maxYear <- max(maxMoviesinaYear$Year)
    
    minMoviesinaYear <- streaming %>%
      group_by(Year) %>%
      summarise(movieTotal = length(Title))
    minVal <- min(minMoviesinaYear$movieTotal)
    
    minMoviesinaYear <- minMoviesinaYear %>%
      group_by(Year) %>%
      filter(movieTotal == minVal)
    
    minYear <- min(minMoviesinaYear$Year)
    
    showMoviesinYear <- streaming %>%
      filter(Year == year) %>%
      summarize(moviesinyear = length(Title))
    
    HTML(
      "<p>The year when the most movies were released was ",
      paste0("<strong>", maxYear, "</strong>, with <strong>"),
      maxVal,
      "</strong>movies.</p>",
      "<p>The earliest year when the least amount of movies were released was ",
      paste0("<strong>", minYear, "</strong>, with <strong>"),
      minVal,
      "</strong>movies. Multiple years, primarily the earlier
         years, had the least amount of movies.</p>",
      "<p>There were <strong>",
      showMoviesinYear$moviesinyear,
      "</strong>movies in <strong>",
      paste0(input$year, "</strong>."),
      "</p>"
    )
  })
  
  output$platform_text <- renderText({
    ageDemo <- streaming %>%
      filter(Age != "NA") %>%
      filter(!!rlang::sym(input$platform_service) == 1)
    
    paste(
      "There are a total of",
      nrow(ageDemo),
      "movies on",
      paste0(input$platform_service, "."),
      "This data shows that there are large discrepancies in what kind of
          movies are available for each demographic."
    )
  })
  ## END Movies per Platform by Age Rating
  
  ## Movies Per Streaming Service per Year
  output$year_plot <- renderPlot({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- streaming %>%
      filter(Year == year)
    number <- c(
      sum(filtered_by_year$Netflix),
      sum(filtered_by_year$Hulu),
      sum(filtered_by_year$`Prime Video`),
      sum(filtered_by_year$`Disney+`)
    )
    by_service <- data.frame(streaming_services, number) %>%
      filter(streaming_services %in% selected)
    ggplot(by_service) +
      geom_col(
        mapping = aes(
          x = streaming_services,
          y = number,
          fill = factor(streaming_services)
        ),
        show.legend = FALSE
      ) + 
      labs(title = "Movies by Year",
           x = "Streaming Service",
           y = "Movie count") + 
      scale_fill_manual(
        values = c("Netflix" = "red", "Hulu" = "seagreen2", "Prime Video" = "skyblue", "Disney+" = "blue")
      )
  })
  
  output$year_description <- renderText({
    filtered <- data %>%
      filter(Year >= input$plot_year[1] &
               Year <= input$plot_year[2]) %>%
      filter(!!rlang::sym(input$year_service) == 1)
    
    paste(
      "There are",
      nrow(filtered),
      "movies that were released between",
      input$plot_year[1],
      "and",
      input$plot_year[2],
      "on",
      paste0(input$year_service, ".")
    )
  })
  ## END Movies Per Streaming Service per Year
  
  ## Conclusion
  # Table that shows amount of movies for each streaming service
  output$conclusion_table <- renderTable({
    number <- c(
      sum(streaming$Netflix),
      sum(streaming$Hulu),
      sum(streaming$`Prime Video`),
      sum(streaming$`Disney+`)
    )
    movies <- data.frame(streaming_services, number)
    movies %>%
      arrange(-number) %>%
      mutate(number = format(number, digits = 0)) %>%
      rename(`Streaming Service` = streaming_services,
             `Number of Movies` = number)
  })
  
  output$conclusion_plot <- renderPlot({
    number <- c(
      sum(streaming$Netflix),
      sum(streaming$Hulu),
      sum(streaming$`Prime Video`),
      sum(streaming$`Disney+`)
    )
    movies <- data.frame(streaming_services, number)
    ggplot(movies) +
      geom_col(mapping = aes(x = streaming_services, y = number),
               fill = "salmon") +
      labs(title = "Number of Movies per Streaming Service",
           x = "Streaming Service",
           y = "Number of Movies")
  })
  ## END Conclusion
} # close server

# Run the application
shinyApp(ui = ui, server = server)

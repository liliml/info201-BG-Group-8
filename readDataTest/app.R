#group project work
library(shiny)
library(tidyverse)
library(shinyWidgets)

fullData <- read_delim("streaming-platform-data.csv")
fullData <- fullData %>%
  select("ID", "Title", "Year", "Age", "Rotten Tomatoes", "Netflix", "Hulu", "Prime Video", "Disney+")
names <- c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Streaming Services, Movies, and Years"), 
  mainPanel(width = "auto",
              navbarPage(
              "Menu",
              tabPanel("Graph",
                       sidebarPanel(width = 5, titlePanel("Movies Per Streaming Service Per Year"), 
                                    p("The bar plot below shows the amount of movies per year 
                                                that are availiable on each streaming service. 
                                                The slider will allow you to select a particular year to look at.
                                                The years shown on the slider range from 1914 to 2021 which allows for
                                                a broad selection of movies. The checkboxes displayed will also let you 
                                                select which services to show for the selected year. This bar graph is helpful for understanding 
                                                what years had the most movies, and which streaming platforms carry the most movies. These statistics 
                                                could be helpful for finding which streaming service has the most options. This data could also help a 
                                                startup company decide which years to focus on in terms of years movies were releaseed as more movies that
                                                year accross more platforms could indicate more popularity for movies from that year."), 
                                    fluidRow(column(align = "center", width = 6, sliderInput("year", 
                                                                                             h4(strong("Year")), 
                                                                                             min = min(fullData$Year), 
                                                                                             max = max(fullData$Year), 
                                                                                             min(fullData$Year),
                                                                                             sep = "")), 
                                             column(width = 6, align = "center", checkboxGroupInput("checkGroup", 
                                                                                                    label = h4(strong("Select Streaming Services")),
                                                                                                    choices = names,
                                                                                                    selected = names))
                                    ), 
                                    h4(strong(uiOutput("choosenYearandServices"))), 
                       ),
                       mainPanel(width = 7, dataTableOutput("plotTable"), plotOutput("mainplot"))
              )
            )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$selectedRows <- renderDataTable({
    fullData %>% 
      sample_n(input$rows)
  })
  
  output$randRowsText = renderText({
    paste0("You have selected ", input$rows, " rows of random movies to show data about")
  })
  
  output$value <- renderPrint({ 
    input$checkGroup 
  })
  
  output$mainplot <- renderPlot({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- fullData %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number) %>% 
      filter(names %in% selected)
    print(selected)
    ggplot(by_service) +
      geom_col(mapping = aes(x = names, y = number, fill = factor(names))) +
      labs(title="Movies by Year",
           x = "Streaming Service",
           y = "Movie count") + 
      scale_fill_manual(
        values = c("Netflix" = "red", "Hulu" = "seagreen2", "Prime Video" = "skyblue", "Disney+" = "blue")
      )
  })
  
  output$plotTable <- renderDataTable({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- fullData %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number) %>% 
      filter(names %in% selected)
    by_service
  })
  
  output$value <- renderPrint({ 
    input$MovieOptions 
  })
  
  
  output$choosenM = renderText({
    paste0("You have selected the movie: ", input$MovieOptions)
  })
  
  output$choosenYearandServices = renderUI({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- fullData %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number) %>% 
      filter(names %in% selected)
    by_service
    
    #NOTE MAXMOVIES AND MINMOVIES DON'T SHOW THE SAME AMOUNT 
    #OF MOVIES PER YEAR COMPARED TO THE BAR GRAPH, WHY IS THIS 
    #OCCURING???? ALSO NEED TO FIX THE THINGS THAT SAY INCORRECT VALUES 
    #BELOW
    maxMoviesinaYear <- fullData %>% 
      group_by(Year) %>% 
      summarise(movieTotal = length(Title))
    maxVal <- max(maxMoviesinaYear$movieTotal)
    maxVal
    maxYear <- max(maxMoviesinaYear$Year)
    maxYear
    
    minMoviesinaYear <- fullData %>% 
      group_by(Year) %>% 
      summarise(movieTotal = length(Title))
    minVal <- min(minMoviesinaYear$movieTotal)
    minVal
    minYear <- min(minMoviesinaYear$Year)
    minYear
    View(maxMoviesinaYear)
    
    HTML("!!! incorrect value: The year the most amount of movies were released was in ", maxYear, " when ", maxVal, " movies were released", "<br/>", 
         "!!! incorrect value: The year the least amount of movies were released was in ", minYear, " when ", minVal, " movie was released", "<br/>", 
         "You have selected the year: ", input$year, "<br/>", "You have selected these movie services: ", toString(input$checkGroup), "<br/>", 
         "There was ", sum(by_service$number), " movies in ", input$year, "<br/>") 
        
  })
  
  output$table <- renderDataTable({
    selectedMovie <- input$MovieOptions
    fullData %>% 
      filter(Title == selectedMovie)
  })
  
  output$allData <- renderDataTable({
    fullData
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

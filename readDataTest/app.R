#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

fullData <- read_delim("streaming-platform-data.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Age, Movies, and Year"), 
  p("There are", nrow(fullData), "movies"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = min(fullData$Year), max = max(fullData$Year), value = min(fullData$Year)),
      #sliderInput("year", "Year", min = min(moviesPerYear$Year), max = max(moviesPerYear$Year), min(moviesPerYear$Year))
    #   selectInput(inputId = "y", label = "Y axis", 
    #               chocies = c(""))
    #   checkboxInput("services", "select services to see data from", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        #Text, Plot, and Table are the names of the tabs
        tabPanel("Table", dataTableOutput("data1")),
        tabPanel("Movie Data By Year", dataTableOutput("movieDataByYear")),
        tabPanel("Text", textOutput("stuff")), 
        tabPanel("Plot", titlePanel("Movies Per Streaming Service Per Year"), 
                plotOutput("plot1")) 
      )
      # dataTableOutput("data1"),
      # plotOutput("data2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$data1 <- renderDataTable({
    fullData #%>% 
      #sample_n(6)
    
  })
  
  # updateCheckboxGroupInput()
  
  output$plot1 <- renderPlot({
    # #WHY WON'T Y VALUE CHANGE???? X VALUE CHANGES? AND WHAT ARE THE GREY MARKS??? AND HOW DO I GET MY DATA TO ACCURATELY SHOW???? SINCE ON SOME STREAMING PLATFOMRS SOME YEARS SHOULD 
    # #HAVE 14 AS A VALUE?
    # showYear <- input$year
    # ggplot(moviesPerYear, aes(x = showYear, fill = factor(Netflix)), aes(x = showYear, fill = factor(Hulu)), aes(x = showYear, fill = factor(`Prime Video`)), aes(x = showYear, fill = factor(`Disney+`))) + 
    #   geom_histogram(position = "dodge") + 
    #   #scale_fill_discrete(name = "Streaming Platforms", labels = c("Netflix", "Hulu", "Prime Video", "Disney+","Total Movies Per Year")) +
    #   scale_fill_manual(values = c("1" = "red", "2" = "seagreen2", "3" = "skyblue", "4" = "blue")) #+ 
    #   #rename("1" = "Netflix")
    # 

    showYear <- input$year
    # ggplot(temp, aes(x = `num of movies`, fill = factor(company))) +
    #   geom_bar()
    
    ggplot(temp, mapping = aes(x = `num of movies`, fill = factor(company))) + 
      geom_bar()
    # showYear <- input$year
    # ggplot(temp, aes(xlab = showYear, ylab = `num of movies`, fill = factor(company))) +
    #   geom_bar() 
    
    # ggplot(temp, aes(y = company, fill = factor(`num of movies`))) + 
    #   geom_bar()
  })
  
  output$movieDataByYear <- renderDataTable({
    #Why won't stuff show?
    showYear <- input$year
    test3 <- fullData %>% 
      filter(Netflix == 1) %>% 
      group_by(Year) %>%
      summarize(n = n())  
    
    HuluData <- fullData %>% 
      #select(Netflix) %>% 
      #filter(old = TRUE) %>% 
      filter(Hulu == 1) %>% 
      group_by(Year) %>%
      summarize(n = n()) #%>% 
    # mutate(amountOfMovies = n) %>% 
    # select(Year, amountOfMovies)
    
    PrimeVideoData <- fullData %>% 
      #select(Netflix) %>% 
      #filter(old = TRUE) %>% 
      filter(`Prime Video` == 1) %>% 
      group_by(Year) %>%
      summarize(n = n())
    
    DisneyData <- fullData %>% 
      #select(Netflix) %>% 
      #filter(old = TRUE) %>% 
      filter(`Disney+` == 1) %>% 
      group_by(Year) %>%
      summarize(n = n())
    
    notFinished <- full_join(full_join(test3, HuluData, by = 'Year'), PrimeVideoData, by = 'Year')
    moviesPerYear <- full_join(notFinished, DisneyData, by = 'Year')
    moviesPerYear
    moviesPerYear <- replace(moviesPerYear, is.na(moviesPerYear), 0)
    moviesPerYear <- moviesPerYear %>% 
      rename(Netflix = n.x, Hulu = n.y, `Prime Video` = n.x.x, `Disney+` = n.y.y)
    #moviesPerYear
    
    # moviesPerYear <- mutate(moviesPerYear, `Total Movies Per Year` = Netflix + Hulu + `Prime Video` + `Disney+`)
    # moviesPerYear
    
    #below tracks num of movies per year for each platform keep this!!!! CAN'T USE HIST, MUST BE BAR GRAPH
    perYear <- moviesPerYear %>% 
      filter(Year == showYear) %>% 
      select(Netflix, Hulu, `Prime Video`, `Disney+`)
    temp <- cbind(c("Netflix", "Hulu", "Prime Video", "Disney+"), t(perYear))
    temp <- as.data.frame(temp)
    colnames(temp) <- c("company", "num of movies")
    #need to put the names and data vertically and put into a bar graph 
    #NOTE: New data frame is called temp
    
    #company is fill, y is movies
    
  })
  #look into plotly
  #note: ggplot + sliders will work for showing the years and things, 
  #use renderplot after making the plot in the sever
}

# Run the application 
shinyApp(ui = ui, server = server)

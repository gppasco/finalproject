

# Loading in a bunch of libraries!

library(shiny)
library(shinythemes)

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(DT)

#############################
#      READING IN DATA      #
#############################
joined <- read_rds("joined.rds")

############################
#      USER INTERFACE      #
############################

# Define UI for application, including a navigation bar on top

ui <- navbarPage("Crosswords!", theme = shinytheme("simplex"),
                 
tabPanel("Home",
         
         fluidPage(
             
             titlePanel("Analysis of New York Times and Los Angeles Times Crosswords"),
             
             p(paste("Add introduction here!"))
         )),

############################
#       COMMON WORDS       #
############################  

tabPanel("Common Words",
         
         fluidPage(
             
             titlePanel("Common Words Per Year"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Choose an outlet:"),
                 
                 selectInput("comm_outlet", h3("Outlet"),
                             choices = list("NYT",
                                            "LAT",
                                            "Both")),
                 
                 helpText("Choose a year:"),
                 
                 selectInput("comm_year", h3("Year"),
                             choices = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("commonplot")
               )
             )
             
         )),

############################
#       WORD LENGTH        #
############################  

tabPanel("Word Length vs. Difficulty",
         
         fluidPage(
             
             titlePanel("Analysis of Word Length vs. Difficulty"),
             
             # Sidebar with a slider input for number of bins. Will adapt to include 
             # NYT and LAT later on.
             
             sidebarLayout(
               sidebarPanel(
                 helpText("Choose a year:"),
                 selectInput("year", h3("Year"),
                             choices = list("2010",
                                            "2011",
                                            "2012",
                                            "2013",
                                            "2014",
                                            "2015",
                                            "2016"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("plot")
               )
             )
             
         )),

#############################
#      CLUE REFERENCES      #
#############################  

tabPanel("Clue References",
         
         fluidPage(
             
             titlePanel("Geographic/Gender Diversity in Clue References")
             
         )),

#############################
#      EXPLORE ANSWERS      #
#############################  

tabPanel("Explore Answers",
         
         fluidPage(
             
             titlePanel("Explore Answers"),
             
             br(),
             
             p(paste("Explore any answer word, and see its past uses in NYT and LAT crosswords. 
                     You can sort by day of week to get a look at how the same answer is clued for different levels of difficulty.")),
             
             # Word resilience tweets.
             
             h3("Answer Search"),
             
             # DTable Keyword Input - input$keyword
             
             textInput("keyword", "Please enter an answer to search in all caps, e.g. OREO", "OREO"),
             
             DTOutput("answer_table")
             
         )),

tabPanel("About",
         
         fluidPage(
             
             titlePanel("About")
             
         ))

)

# Define server logic required to output an image
server <- function(input, output, session) {
  
  ############################
  #       COMMON WORDS       #
  ############################  
      outlet <- reactive({
        if (input$comm_outlet %in% c("NYT","LAT")) {
          filter(joined, Outlet == input$comm_outlet)
        } else {
          joined
        }
        # ifelse(input$comm_outlet %in% c("NYT", "LAT"),
              #filter(joined, Outlet == input$comm_outlet), joined)
      })
      
      observeEvent(outlet(), {
        choices <- unique(outlet()$Year)
        updateSelectInput(session, "comm_year", choices = choices)
      })
      
      observe({
      
      common_filtered <- 
        if(input$comm_outlet %in% c("NYT", "LAT")) {
          filter(joined, Outlet == input$comm_outlet & Word != "")
        } else {
          joined
        }
      
      
      common = common_filtered %>% 
        filter(Year == input$comm_year & Word != "") %>%
        unnest(Word) %>%
        group_by(Word) %>%
        summarize(times=n()) %>%
        arrange(desc(times)) %>%
        slice(1:10)
      
      output$commonplot <- renderPlot(
        ggplot(common, aes(x=reorder(Word, -times), y=times)) +
          geom_col() +
          xlab("Word") +
          ylab("Number of Appearances") +
          ggtitle("Ten most common words in the New York Times crossword",
                  subtitle="Based on crosswords from 2010-2016") +
          geom_text(aes(y=times + 0.5, label=times))
      )
    })
  
  #############################
  #       EXPLORE WORDS       #
  #############################  
    output$answer_table <- renderDT({
      datatable(joined %>% filter(Word == input$keyword),
                class= 'display',
                rownames = FALSE,
                selection = 'single',
                colnames = c('Year', 'Weekday', 'Clue', 'Answer', 'Outlet'),
                options = list(dom = 'tip')
      ) %>% formatStyle(2, cursor = 'pointer')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

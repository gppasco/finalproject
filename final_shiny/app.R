

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
             
             p(paste("Select an outlet and a year to see the ten most common crossword
                     answers in that outlet for year. Note that short, vowel-heavy
                     answers are predominant among the commonly-used answers.")),
             
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
         
         tabsetPanel(
           
           tabPanel("Exploration",
                    
             fluidPage(
                 
                 titlePanel("Exploration of Word Length vs. Difficulty"),
                 
                 p(paste("Conventionally, the difficulty of New York Times and Los Angeles Times
                           crosswords increases with the day of the week, strictly increasing from
                           Monday to Saturday. (Sunday, while larger in size, is normally intended
                           to be as difficult as a Thursday puzzle). We can use this as a framework
                           to analyze difficulty versus word length. As this introductory analysis
                         shows, there is a general positive relationship between difficulty and word 
                         length from Monday to Saturday, while Sunday is an anomaly due to its 
                         different grid.")),
                 
                 br(),
                 
                 p(paste("Note that on some days of the week, outlets will run 'themeless' puzzles.
                         These generally have lower word counts, and therefore longer words. The NYT
                         runs themeless puzzles on Fridays and Saturdays, while the LAT runs themeless
                         puzzles on Saturdays only. This is why those outlets have spikes in word length
                         at those specific days of the week.")),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     helpText("Choose an outlet:"),
                     
                     selectInput("leng_outlet", h3("Outlet"),
                                 choices = list("NYT",
                                                "LAT",
                                                "Both")),
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     plotOutput("lengplot")
                   )
                 )
                 
         )),
         
         tabPanel("Further Analysis",
                  
                  h3("Analysis"))
         
         )), 
         
          

#############################
#      CLUE REFERENCES      #
#############################  



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
          ggtitle(paste("Ten most common words in the ", input$comm_outlet, " crossword"),
                  subtitle=paste("Based on crosswords from ", input$comm_year)) +
          geom_text(aes(y=times + 0.5, label=times))
      )
    })

 ############################
 #       WORD LENGTH        #
 ############################ 

      observe({
        
        leng_filtered <- 
          if(input$leng_outlet %in% c("NYT", "LAT")) {
            filter(joined, Outlet == input$leng_outlet & Word != "")
          } else {
            joined
          }
        
        
        leng = leng_filtered %>% 
          mutate(length = wordlen(Word)) %>%
          group_by(Year,Weekday) %>%
          summarize(avg_length = sum(length)/n())
        
        output$lengplot <- renderPlot(
          leng %>%
            mutate(Weekday = fct_relevel(Weekday, "Mon", "Tue", "Wed", "Thu",
                                         "Fri", "Sat", "Sun")) %>%
            ggplot(aes(x = Weekday, y = avg_length)) +
            geom_boxplot() +
            labs(x = "Weekday", y = "Average Word Length",
                 title = "Average Word Length of Crosswords by Weekday")
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

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

  library(devtools)
  library(twitteR)
  library(ggplot2)
  library(tidyverse)
  library(tidytext)
  library(wordcloud)
  library(reshape2)
  library(lubridate)
  library(ggvis)
library(shiny)
## read data
data <- read.csv("AlphaGo.csv",stringsAsFactors=FALSE)

bingneg <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bingpos <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")



## tokenize and remove stop words

data(stop_words)

tidy_data <-  data %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

## create custom stop words
custom_stop_words <- bind_rows(data_frame(word = c("mother", "player","learning","intelligence"), 
                                          lexicon = c("custom")), 
                               stop_words)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Twitter Analysis of AlphaGo"),
  
  navbarPage(title="content",
             tabPanel("Introduction",
                      h1("Project Introduction"),
                      br(), # space
                      hr(), # draw a line
                      p("This is a project analyzing twitter data containing the keyword 'AlphaGo' from 2017-11-28 to 2017-12-04. In order to explore the brand
                        perception of AlphaGo on Twitter, word frequency analysis, sentiment anlysis, day part analysis are conducted."),
                      img(src="ai.jpg",align="center"),
                      p("image source: http://tech.sina.com.cn/d/2017-10-20/doc-ifymzksi0552600.shtml")
              ),
             tabPanel("Word Frequency",
                      h1("Word Frequency"),
                      fluidRow(
                        column(5,
                               plotOutput("wordcloud1")
                        ),
                        column(6,
                               dataTableOutput("wordfreqtable")
                        )
                      )
             ),
             tabPanel("Sentiment Analysis",
                      h1("Sentiment Analysis"),
                      fluidRow(
                        column(4,
                               plotOutput("wordcloud2")
                        ),
                        column(4,
                               plotOutput("posplot")
                        ),
                        column(4,
                               plotOutput("negplot")
                        )
                      )
             ), #end of tabpanel sentiment
             tabPanel("Topic Hotness By Date",
                      h1("Topic Hotness By Date"),
                      p("Hover on the dots to see numbers."),
                      p("Scroll down to see data."),
                      fluidRow(
                        column(6,
                               ggvisOutput("bydate")
                        ),
                        column(3,
                               plotOutput("secondpie")
                        ),
                        column(3,
                               
                               plotOutput("thirdpie")
                          
                        )
                      ),
                        dataTableOutput("datetable")
                      
             ),
             tabPanel("Topic Hotness By Hour",
                      h1("Topic Hotness By Hour"),
                      fluidRow(
                        column(8,
                               ggvisOutput("byhour")
                        ),
                        column(4,
                               p("Hover on the dots to see numbers.")
                               
                        )
                      )
             )
                      )#end of navbar
  )#end of fluidpage


# Define server logic
server <- function(input, output) {
   
  
  output$wordcloud1 <- renderPlot({
    
    # draw the wordcloud1
    tidy_data %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
    
  }) #end of wordcloud1
  
  output$wordfreqtable <- renderDataTable({
    tidy_data %>%  count(word, sort=TRUE)
    
  })
  
  output$wordcloud2 <-renderPlot({
    tidy_data %>%
      anti_join(custom_stop_words) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100)
  })
  
  output$posplot <- renderPlot({
    
    poslist <- tidy_data %>%
      anti_join(custom_stop_words) %>%
      inner_join(bingpos) %>%
      count(word, sort = TRUE)
    
    poslist %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill="red") +
      xlab(NULL) +
      coord_flip() +
      scale_y_continuous(limits = c(0,110))
    
  })
  
  output$negplot <- renderPlot({
    
    neglist <- tidy_data %>%
      inner_join(bingneg) %>%
      anti_join(custom_stop_words) %>%
      count(word, sort = TRUE)
    
    neglist %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill="blue") +
      xlab(NULL) +
      coord_flip() +
      scale_y_continuous(limits = c(0,110))
  })
  
  # second dec pie chart

  
  output$secondpie <- renderPlot ({
    df1 <- data.frame(
      group = c("RT@alphagomovie", "RT@chrisalbon", "Others"),
      value = c(85, 11, 144-11-85)
    )
    
    ggplot(df1, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    ggtitle("Dec 2nd Popular Retweets")
  })
  
  # third dec pie chart
  
  output$thirdpie <- renderPlot ({
    
  df2 <- data.frame(
    group = c("RT@alphagomovie", "RT@demishassabis", "Others"),
    value = c(85, 65, 220-65-85)
  )
  
  ggplot(df2, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    ggtitle("Dec 3rd Popular Retweets")  
  })
  
    # create date table
    datedt <-data %>%
      mutate( date1 = date(created)) %>%
      group_by(date1) %>%
      summarise(date_n = n()) %>%
      mutate(Date = as.character(date1))
    
    datedt$id <- 1:nrow(datedt)  # Add an id column to use ask the key
    
    
    # create all_values function for ggvis
    hover1 <- function(x) {
      if(is.null(x)) return(NULL)
      rw <- datedt$date_n[datedt$id == x$id]
      paste0(rw)
    }
    
    # ggvis date 
    datedt %>% ggvis(x = ~Date, y = ~date_n, key := ~id,stroke:="#1DA1F2") %>%
      layer_lines(strokeWidth := 3) %>%
      layer_points() %>%
      add_tooltip(hover1, "hover") %>%
      bind_shiny("bydate")
  
  
  output$datetable <- renderDataTable({
    
    data
  })

  # hour ggvis
  # create hour table
    hourly <- data %>%
      mutate(hour=hour(created)) %>%
      group_by(hour) %>%
      summarise(hour_n = n()) %>%
      mutate(Hour=as.integer(hour))
    
    hourly$id <- 1:nrow(hourly)  # Add an id column to use ask the key
    
    
    # create hover2 function for ggvis
    hover2 <- function(x) {
      if(is.null(x)) return(NULL)
      row <- hourly$hour_n[hourly$id == x$id]
      paste0(row)
    }
    
    # ggvis date 
    hourly %>% ggvis(x = ~Hour, y = ~hour_n, key := ~id,stroke:="#1DA1F2") %>%
      layer_lines(strokeWidth := 2) %>%
      layer_points() %>%
      add_tooltip(hover2, "hover") %>%
      bind_shiny("byhour")
  }#end of ifinteractive
  
  

# Run the application 
shinyApp(ui = ui, server = server)


#Loading required libraries
library(shiny)
library(twitteR)
library(openssl)
library(httpuv)
library(tm)
library(stringr)
library(dplyr)
library(httr)
library(ROAuth)
library(shiny)
library(shinythemes)
library(DT)

shinyUI <- fluidPage(theme = shinytheme("sandstone"), themeSelector(),
        headerPanel("Twitter Sentiment Analyzer"),

  sidebarPanel(
  tags$head(
      tags$style("body"), tags$style("label"),
      tags$style('h1'), tags$style('body')),
      textInput("searchTerm", "Hashtag", "#"),
      sliderInput("maxTweets","Number of Tweets:", min = 9, max = 500, value = 200, step = 30), 
      submitButton(text = "Submit")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Trending Hashtags", HTML("<div> <h1> Trending Hashtags from Selected Location</h1> </div>"),
               selectInput("trendingTable","Choose location for Trending", c("Worldwide", "Austria","Belgium",
                                                                                          "Colombia","Greece",
                                                                                          "Denmark","France",
                                                                                          "Israel","Germany",
                                                                                          "Norway","India",
                                                                                          "Sweden","Canada","Japan",
                                                                                          "Latvia","Lebanon",
                                                                                          "Mexico", 'Venezuela',
                                                                                          "Romania","Spain", 'Brazil',
                                                                                          "Italy","Spain", 'Argentina', 
                                                                                          "Poland","United Kingdom",
                                                                                          "Ukraine","New Zealand","Australia"), 
               selected = "Worldwide", selectize = FALSE),
               submitButton(text = "Search"),
               HTML("<h2>Trending on Twitter</h2>"),
               tableOutput("trendtable"),
               ),
      
      tabPanel("Word Cloud", HTML("<h2>Frequently Used Words from Trending Hashtag</h2>"), plotOutput("word")
               ),
      
      tabPanel("Analysis Table", HTML("<h2>Sentiment Analysis</h2>"), tableOutput("tabledata")),
         
      tabPanel("Sentiment Analysis", plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
               ),
      
      tabPanel("Recent Users", HTML("<h2>Top 10 users who used that Hashtag</h2>"), plotOutput("tweetersplot"), tableOutput("tweeterstable"))
              )
      )
)
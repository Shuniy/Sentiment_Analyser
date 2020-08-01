# Loading Libraries
library(shiny)
library(reshape)
library(tm)
library(wordcloud)

# Importing working file
source("Working.R")

shinyServer <- function(input, output, session) {
  # Api token and keys
  api_key <- '************************************************'
  api_secret <- '************************************************'
  access_token <- '************************************************'
  access_token_secret <- '************************************************'
  
  # Creating cache for authentication, to prevent authentication type popup
  options(httr_oauth_cache = TRUE)
  
  # Authorizing twitter with api key
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  # Getting positive and negative words into list
  positive = scan('positive_words.txt', what = 'character', comment.char = ';')
  negative = scan('negative_words.txt', what = 'character', comment.char = ';')
  
  # Sentiment Analysis
  positive_words<<-c(positive)
  negative_words<<-c(negative)
  
  tweets_list <- reactive(
    {tweets_list <- searchTwitter(input$searchTerm, n = input$maxTweets, lang = "en")}) #Using TwitteR
  
  tweets <- reactive(
    {tweets <- search_tweets_and_return(tweets_list() )})
  
  # Calculating sentiment score
  result <- reactive(
    {result <- compute_sentiment_score(tweets(), positive, negative)})
  
  final <- reactive(
    {final <- analyze_sentiments(result())})
  
  final_percentage <- reactive(
    {final_percentage <- compute_percentage(final())})
  
  output$tabledata <- renderTable(final_percentage())	
  
  # Word cloud data.
  text_words <- reactive(
    {text_words <- clean_wordcloud_data(tweets())})

  output$word <- renderPlot(
    { wordcloud(text_words(), random.order = FALSE, max.words = 80, col = rainbow(100), main = "WordCloud", scale = c(2, 2))})
  
  # Plotting the scores
  
  # Let us render a plot for postive and negative words along with the overall score.
  output$histPos <- renderPlot({hist(final()$Positive, col = 'steelblue', main = "Positive Sentiment", xlab = "Positive Score")})
  output$histNeg <- renderPlot({hist(final()$Negative, col = 'steelblue', main = "Negative Sentiment", xlab = "Negative Score") })
  output$histScore <- renderPlot({hist(final()$Score, col = 'steelblue', main = "Overall Score", xlab = "Overall Score") })	
  
  # Creating trends
  trend_table <- reactive(
    {trend_table <- get_top_trends(input$trendingTable)})
  
  output$trendtable <- renderTable(trend_table())
  
  # TOP 20 USER CHARTS
  # Chart of a particular hashtag
  # Plot the table for the top 10 charts
  tweet_data <- reactive(
    {tweet_data <- toptweeters(tweets_list())})
  
  output$tweetersplot<-renderPlot(barplot(head(tweet_data()$Tweets, 10), names = head(tweet_data()$User, 10), main = "Top 10 users associated with the Hashtag", col = "steelblue") )
  
  output$tweeterstable <- renderTable(head(tweet_data(), 30))
}